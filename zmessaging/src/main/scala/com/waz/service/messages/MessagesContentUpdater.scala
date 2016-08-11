/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.service.messages

import android.content.Context
import com.waz.ZLog._
import com.waz.api.Message
import com.waz.api.Message.Status
import com.waz.content._
import com.waz.model._
import com.waz.service.UserService
import com.waz.service.media.RichMediaContentParser
import com.waz.sync.SyncServiceHandle
import com.waz.threading.Threading
import com.waz.utils._
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future

class MessagesContentUpdater(context: Context, val messagesStorage: MessagesStorage, convs: ConversationStorage, users: UserService, sync: SyncServiceHandle, deletions: MsgDeletionStorage) {

  private implicit val tag: LogTag = logTagFor[MessagesContentUpdater]
  import Threading.Implicits.Background

  val contentParser = new RichMediaContentParser

  def getMessage(msgId: MessageId) = messagesStorage.getMessage(msgId)

  def deleteMessage(msg: MessageData) = messagesStorage.delete(msg)

  def deleteMessagesForConversation(convId: ConvId): Future[Unit] = messagesStorage.deleteAll(convId)

  def updateMessage(id: MessageId)(updater: MessageData => MessageData): Future[Option[MessageData]] = messagesStorage.update(id, updater) map {
    case Some((msg, updated)) if msg != updated =>
      assert(updated.id == id && updated.convId == msg.convId)
      Some(updated)
    case _ =>
      None
  }

  // removes messages and records deletion
  // this is used when user deletes a message manually (on local or remote device)
  def deleteOnUserRequest(ids: Seq[MessageId]) =
    deletions.insert(ids.map(id => MsgDeletion(id, Instant.now()))) flatMap { _ =>
      messagesStorage.remove(ids)
    }

  def addLocalMessage(msg: MessageData, state: Status = Status.PENDING) = Serialized.future("add local message", msg.convId) {
    verbose(s"addLocalMessage: $msg")
    nextLocalEventIdAndTime(msg.convId) flatMap { case (eventId, time) =>
      verbose(s"adding message to storage: $eventId, $time")
      messagesStorage.addMessage(msg.copy(source = eventId, state = state, time = time, localTime = Instant.now))
    }
  }

  def addLocalSentMessage(msg: MessageData) = Serialized.future("add local message", msg.convId) {
    verbose(s"addLocalSentMessage: $msg")
    lastSentEventIdAndTime(msg.convId) flatMap { case (ev, t) =>
      val (eventId, time) = (EventId.nextLocal(ev), t.plusMillis(1))
      verbose(s"adding message to storage: $eventId, $time")
      messagesStorage.addMessage(msg.copy(source = EventId.nextLocal(eventId), state = Status.SENT, time = time, localTime = Instant.now))
    }
  }

  private def nextLocalEventIdAndTime(convId: ConvId) =
    messagesStorage.getLastMessage(convId) flatMap {
      case Some(msg) => Future.successful((msg.source, msg.time))
      case _ => convs.get(convId).map(_.fold((EventId.Zero, MessageData.UnknownInstant)) { c => (c.lastEvent, c.lastEventTime) })
    } map { case (ev, time) =>
      (EventId.nextLocal(ev), Instant.now.max(time.plusMillis(1)))
    }

  private def lastSentEventIdAndTime(convId: ConvId) =
    messagesStorage.getLastSentMessage(convId) flatMap {
      case Some(msg) => Future.successful((msg.source, msg.time))
      case _ => convs.get(convId).map(_.fold((EventId.Zero, MessageData.UnknownInstant)) { c => (c.lastEvent, c.lastEventTime) })
    }

  /**
   * Updates last local message or creates new one.
   */
  def updateOrCreateLocalMessage(convId: ConvId, msgType: Message.Type, update: MessageData => MessageData, create: => MessageData) =
    Serialized.future("update-or-create-local-msg", convId, msgType) {
      messagesStorage.lastLocalMessage(convId, msgType) flatMap {
        case Some(msg) => // got local message, try updating
          @volatile var shouldCreate = false
          verbose(s"got local message: $msg, will update")
          updateMessage(msg.id) { msg =>
            if (msg.source.isLocal) update(msg)
            else { // msg was already synced, need to create new local message
              shouldCreate = true
              msg
            }
          } flatMap { res =>
            verbose(s"shouldCreate: $shouldCreate")
            if (shouldCreate) addLocalMessage(create).map(Some(_))
            else Future.successful(res)
          }
        case _ => addLocalMessage(create).map(Some(_))
      }
    }

  private[service] def addMessages(convId: ConvId, msgs: Seq[MessageData]): Future[Set[MessageData]] = {
    verbose(s"addMessages($msgs)")
    val (systemMsgs, contentMsgs) = msgs.partition(_.isSystemMessage)

    for {
      sm <- addSystemMessages(convId, systemMsgs)
      toAdd <- skipPreviouslyDeleted(contentMsgs)
      cm <- addContentMessages(convId, toAdd)
    } yield sm.toSet ++ cm
  }

  private def skipPreviouslyDeleted(msgs: Seq[MessageData]) =
    deletions.getAll(msgs.map(_.id)) map { deletions =>
      val ds: Set[MessageId] = deletions.collect { case Some(MsgDeletion(id, _)) => id } (breakOut)
      msgs.filter(m => !ds(m.id))
    }

  private def addSystemMessages(convId: ConvId, msgs: Seq[MessageData]): Future[Seq[MessageData]] =
    if (msgs.isEmpty) Future.successful(Seq.empty)
    else {
      messagesStorage.getMessages(convId, msgs.map(_.source)) flatMap { prev =>
        val prevEvents = prev.map(m => m.source)
        val toAdd = msgs.filterNot(m => prevEvents.contains(m.source))

        RichFuture.traverseSequential(toAdd.groupBy(_.source).toSeq) { case (_, ms) =>
          val msg = ms.last
          messagesStorage.lastLocalMessage(convId, msg.msgType) flatMap {
            case Some(m) if m.userId == msg.userId =>
              verbose(s"lastLocalMessage(${msg.msgType}) : $m")

              if (m.msgType == Message.Type.MEMBER_JOIN || m.msgType == Message.Type.MEMBER_LEAVE) {
                val remaining = m.members.diff(msg.members)
                if (remaining.nonEmpty) addMessage(m.copy(id = MessageId(), source = EventId.nextLocal(msg.source max m.source), members = remaining))
              }
              messagesStorage.update(m.id, m => msg.copy(id = m.id, localTime = m.localTime)).map(_.fold2(msg, _._2))
            case res =>
              verbose(s"lastLocalMessage(${msg.msgType}) returned: $res")
              messagesStorage.addMessage(msg)
          }
        }
      }
    }

  private def addContentMessages(convId: ConvId, msgs: Seq[MessageData]): Future[Set[MessageData]] = {

    // merge data from multiple events in single message
    def merge(msgs: Seq[MessageData]): MessageData = {

      def mergeSource(s1: EventId, s2: EventId) =
        if (s1 == EventId.Zero || s1.isLocal) s2
        else if (s2 == EventId.Zero || s2.isLocal) s1
        else s1 min s2

      def mergeLocal(m: MessageData, msg: MessageData) =
        msg.copy(id = m.id, source = mergeSource(msg.source, m.source), localTime = m.localTime)

      def mergeMatching(prev: MessageData, msg: MessageData) = {
        val u = prev.copy(msgType = msg.msgType, source = mergeSource(msg.source, prev.source), time = if (msg.time.isBefore(prev.time) || prev.isLocal) msg.time else prev.time, protos = prev.protos ++ msg.protos, content = msg.content)
        prev.msgType match {
          case Message.Type.RECALLED => prev // ignore updates to already recalled message
          case Message.Type.KNOCK => u.copy(localTime = if (msg.hotKnock && !prev.hotKnock) msg.localTime else prev.localTime)
          case _ => u
        }
      }

      if (msgs.size == 1) msgs.head
      else msgs.reduce { (prev, msg) =>
        if (prev.isLocal && prev.userId == msg.userId) mergeLocal(prev, msg)
        else if (prev.userId == msg.userId) mergeMatching(prev, msg)
        else {
          warn(s"got message id conflict, will add it with random id, existing: $prev, new: $msg")
          addMessage(msg.copy(id = MessageId()))
          prev
        }
      }
    }

    if (msgs.isEmpty) Future.successful(Set.empty)
    else
      messagesStorage.updateOrCreateAll (
        msgs.groupBy(_.id).mapValues { data =>
          { (prev: Option[MessageData]) => merge(prev.toSeq ++ data) }
        }
      )
  }

  private[service] def addMessage(msg: MessageData): Future[Option[MessageData]] = addMessages(msg.convId, Seq(msg)).map(_.headOption)

  // updates server timestamp for local messages, this should make sure that local messages are ordered correctly after one of them is sent
  def updateLocalMessageTimes(conv: ConvId, prevTime: Instant, time: Instant) = {

    def updater(time: Instant)(m: MessageData) = {
      verbose(s"try updating local message time, msg: $m, time: $time")
      if (m.isLocal && m.time.isBefore(time)) m.copy(time = time) else m
    }

    messagesStorage.findLocalFrom(conv, prevTime) flatMap { local =>
      verbose(s"local messages: $local")
      if (local.isEmpty) Future successful Seq.empty[(MessageData, MessageData)]
      else
        messagesStorage updateAll local.sortBy(_.time).zipWithIndex.map { case (m, i) => m.id -> updater(time.plusMillis(i)) _ }(breakOut)
    }
  }
}
