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
package com.waz.content

import java.util.concurrent.ConcurrentHashMap

import android.content.Context
import com.waz.ZLog._
import com.waz.api.Message
import com.waz.model.MessageData.{MessageEntry, MessageDataDao}
import com.waz.model._
import com.waz.service.conversation.ConversationsService
import com.waz.service.messages.{LikingsService, MessageAndLikesNotifier}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils._
import com.waz.utils.events.{EventStream, Signal, SourceSignal}
import org.threeten.bp.Instant

import scala.collection.{Set, SortedMap}
import scala.concurrent.Future

class MessagesStorage(context: Context, storage: ZStorage, selfUser: => Future[UserId], convs: ConversationStorage, users: => UsersStorage, likings: => LikingsService, notifier: => MessageAndLikesNotifier) extends
    CachedStorage[MessageId, MessageData](new TrimmingLruCache[MessageId, Option[MessageData]](context, Fixed(MessagesStorage.cacheSize)), storage)(MessageDataDao, "MessagesStorage_Cached") {

  import com.waz.utils.events.EventContext.Implicits.global

  private implicit val tag: LogTag = logTagFor[MessagesStorage]
  private implicit val dispatcher = new SerialDispatchQueue(name = "MessagesStorage")

  val messageAdded = onAdded
  val messageUpdated = onUpdated

  val messageChanged = EventStream.union(messageAdded, messageUpdated.map(_.map(_._2)))

  private val indexes = new ConcurrentHashMap[ConvId, ConvMessagesIndex]

  lazy val incomingMessages = selfUser flatMap { selfUser =>
    storage {
      MessageDataDao.listIncomingMessages(selfUser, IncomingMessages.sinceTime)(_)
    } map { new IncomingMessages(selfUser, _) }
  }

  def msgsIndex(conv: ConvId): Future[ConvMessagesIndex] =
    Option(indexes.get(conv)).fold {
      Future {
        Option(indexes.get(conv)).getOrElse {
          returning(new ConvMessagesIndex(conv, this, selfUser, users, convs, likings, notifier, storage))(indexes.put(conv, _))
        }
      }
    } {
      Future.successful
    }

  onAdded { added =>
    Future.traverse(added.groupBy(_.convId)) { case (convId, msgs) =>
      msgsIndex(convId).flatMap { index =>
        index.add(msgs).flatMap(_ => index.firstMessageId) map { first =>
          // XXX: calling update here is a bit ugly
          val ms = msgs.map {
            case msg if first.contains(msg.id) =>
              update(msg.id, _.copy(firstMessage = first.contains(msg.id)))
              msg.copy(firstMessage = first.contains(msg.id))
            case msg => msg
          }

          updateIncomingMessagesList(convId, ms)
        }
      }
    } .recoverWithLog(reportHockey = true)
  }

  onUpdated { updates =>
    Future.traverse(updates.groupBy(_._1.convId)) { case (convId, msgs) =>
      for {
        index <- msgsIndex(convId)
        _ <- index.update(msgs)
        _ <- updateIncomingMessagesList(convId, msgs.map(_._2))
      } yield ()
    } .recoverWithLog(reportHockey = true)
  }

  onDeleted { ids =>
    incomingMessages.map { _.remove(ids.toSet) }
  }

  convs.convUpdated.on(dispatcher) {
    case (prev, updated) if updated.lastRead != prev.lastRead =>
      verbose(s"lastRead of conversation ${updated.id} updated to ${updated.lastRead}, will update unread count")
      msgsIndex(updated.id).map(_.updateLastRead(updated)).recoverWithLog()
    case _ => // ignore
  }

  def addMessage(msg: MessageData): Future[MessageData] = put(msg.id, msg)

  // find latest message up to given event
  def findLatestUpTo(conv: ConvId, event: EventId): Future[Option[MessageData]] =
    storage { MessageDataDao.findLatestUpToEvent(conv, event)(_) } .future.flatMap { msg =>
      var result = msg
      foreachCached { m =>
        if (m.source <= event && m.convId == conv && result.forall(_.source <= m.source))
          result = Some(m)
      } map { _ => result }
    }

  def countUnread(conv: ConvId, lastReadTime: Instant): Future[Int] =
    storage { MessageDataDao.countNewer(conv, lastReadTime)(_) } .future.flatMap { count =>
      var cachedCount = 0
      foreachCached { m =>
        if (! m.isLocal && m.convId == conv && m.time.isAfter(lastReadTime)) cachedCount += 1
      } map { _ =>
        verbose(s"countUnread($conv, $lastReadTime), count: $count, cached: $cachedCount")
        math.max(count.toInt, cachedCount)
      }
    }

  def countSentByType(selfUserId: UserId, tpe: Message.Type): Future[Int] = storage(MessageDataDao.countSentByType(selfUserId, tpe)(_).toInt)

  def countMessages(conv: ConvId, p: MessageEntry => Boolean): Future[Int] = storage(MessageDataDao.countMessages(conv, p)(_))

  def getMessage(id: MessageId): Future[Option[MessageData]] = get(id)

  def getMessages(ids: MessageId*): Future[Seq[Option[MessageData]]] = getAll(ids)

  def getIncomingMessages: Signal[List[MessageData]] = Signal.future(incomingMessages).flatMap(_.messages)

  def getMessages(convId: ConvId, events: Seq[EventId]): Future[Vector[MessageData]] = {
    val evSet = events.toSet
    find[MessageData, Vector[MessageData]](
      m => m.convId == convId && evSet(m.source),
      db => MessageDataDao.findMessages(convId, events)(db),
      identity)
  }

  private def findMessageInDb(convId: ConvId, event: EventId) = getMessages(convId, Seq(event)).map(_.headOption)

  def findMessage(convId: ConvId, event: EventId): Future[Option[MessageData]] =
    msgsIndex(convId) flatMap { _.getLastEventId } flatMap { last =>
      if (event > last) Future.successful(None)
      else findMessageInDb(convId, event)
    }

  def getEntries(conv: ConvId) = Signal.future(msgsIndex(conv)).flatMap(_.signals.messagesCursor)

  def lastMessage(conv: ConvId) = Signal.future(msgsIndex(conv)).flatMap(_.signals.lastMessage)

  def getLastMessage(conv: ConvId) = msgsIndex(conv).flatMap(_.getLastMessage)

  def getLastSentMessage(conv: ConvId) = msgsIndex(conv).flatMap(_.getLastSentMessage)

  def unreadCount(conv: ConvId): Signal[Int] = Signal.future(msgsIndex(conv)).flatMap(_.signals.unreadCount)

  def lastRead(conv: ConvId) = Signal.future(msgsIndex(conv)).flatMap(_.signals.lastReadTime)

  def lastLocalMessage(conv: ConvId, tpe: Message.Type): Future[Option[MessageData]] =
    msgsIndex(conv).flatMap(_.lastLocalMessage(tpe)).flatMap {
      case Some(id) => get(id)
      case _ => CancellableFuture.successful(None)
    }

  def findLocalFrom(conv: ConvId, time: Instant) =
    find(m => m.convId == conv && m.isLocal && !m.time.isBefore(time), MessageDataDao.findLocalFrom(conv, time)(_), identity)

  def findMessagesFrom(conv: ConvId, time: Instant) =
    find(m => m.convId == conv && !m.time.isBefore(time), MessageDataDao.findMessagesFrom(conv, time)(_), identity)

  def update(id: MessageId)(updater: MessageData => MessageData): Future[Option[MessageData]] =
    super.update(id, updater) map {
      case Some((msg, updated)) if msg != updated =>
        assert(updated.id == id && updated.convId == msg.convId)
        Some(updated)
      case _ =>
        None
    }

  def delete(conv: ConvId, event: EventId): Future[Unit] =
    findMessage(conv, event) flatMap {
      case Some(msg) => super.remove(msg.id) flatMap { _ => msgsIndex(conv).flatMap(_.delete(msg)) }
      case None =>
        warn(s"No message found for: $conv, $event")
        Future.successful(())
    }

  def delete(id: MessageId): Future[Unit] = remove(id)

  override def remove(id: MessageId): Future[Unit] =
    getMessage(id) flatMap {
      case Some(msg) => super.remove(msg.id) flatMap { _ => msgsIndex(msg.convId).flatMap(_.delete(msg)) }
      case None =>
        warn(s"No message found for: $id")
        Future.successful(())
    }

  def delete(conv: ConvId, upTo: Instant): Future[Unit] = {
    verbose(s"delete($conv, $upTo)")
    for {
      _ <- storage { MessageDataDao.deleteUpTo(conv, upTo)(_) } .future
      _ <- deleteCached(m => m.convId == conv && ! m.time.isAfter(upTo))
      _ <- msgsIndex(conv).flatMap(_.delete(upTo))
      _ <- incomingMessages.map(_.remove(conv))
    } yield ()
  }

  def clear(convId: ConvId, clearTime: Instant): Future[Unit] = {
    verbose(s"clear($convId, $clearTime)")
    delete(convId, clearTime)
  }

  def deleteAll(conv: ConvId) = {
    verbose(s"deleteAll($conv)")
    for {
      _ <- storage { MessageDataDao.deleteForConv(conv)(_) } .future
      _ <- deleteCached(_.convId == conv)
      _ <- msgsIndex(conv).flatMap(_.delete())
      _ <- incomingMessages.map(_.remove(conv))
    } yield ()
  }

  private def updateIncomingMessagesList(convId: ConvId, msgs: Seq[MessageData]) =
    convs.get(convId) flatMap { conv =>
      if (conv.exists(_.muted)) Future.successful(())
      else incomingMessages map { _.add(msgs) }
    }
}

object MessagesStorage {
  val cacheSize = 12048
  val FirstMessageTypes = Set(Message.Type.TEXT, Message.Type.KNOCK, Message.Type.ASSET)
}

class IncomingMessages(selfUserId: UserId, initial: Seq[MessageData]) {
  import com.waz.content.IncomingMessages._

  import scala.collection.breakOut
  private implicit val tag: LogTag = logTagFor[IncomingMessages]

  private val messagesSource = new SourceSignal[SortedMap[(Long, MessageId), MessageData]](Some(initial.map(m => (m.localTime.toEpochMilli, m.id) -> m)(breakOut)))

  val messages = messagesSource map { msgs =>
    val time = sinceTime
    msgs.dropWhile(_._1._1 < time).values.toList
  }

  def add(msgs: Seq[MessageData]) = {
    val time = sinceTime
    val ms = msgs filter(m => m.userId != selfUserId && m.localTime.toEpochMilli > time) map { msg => (msg.localTime.toEpochMilli, msg.id) -> msg }
    val ids = msgs.map(_.id).toSet
    messagesSource.mutate { _.dropWhile(_._1._1 < time).filter(e => !ids.contains(e._1._2)) ++ ms }
  }

  def remove(ids: Set[MessageId]) =
    messagesSource.mutate(_.from((sinceTime, MessageId.Empty)).filter(e => !ids.contains(e._1._2)))

  def remove(msg: MessageData) = {
    messagesSource.mutate(_ - (msg.localTime.toEpochMilli -> msg.id))
  }

  def remove(conv: ConvId) = {
    messagesSource.mutate(_.filter { case (k, v) => v.convId != conv})
  }

  def remove(conv: ConvId, upTo: Instant) = {
    messagesSource.mutate(_.filter { case (k, v) => v.convId != conv || v.time.isAfter(upTo)})
  }
}

object IncomingMessages {
  val Timeout = ConversationsService.KnockTimeout

  def sinceTime = System.currentTimeMillis - Timeout.toMillis
}
