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

import com.waz.ZLog._
import com.waz.api.Message
import com.waz.api.Message.Status
import com.waz.model.MessageData.MessageDataDao
import com.waz.model._
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils._
import com.waz.utils.events.{EventStream, RefreshingSignal, Signal}
import org.threeten.bp.Instant

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class ConvMessagesIndex(conv: ConvId, messages: MessagesStorage, selfUserId: UserId, users: UsersStorage,
    convs: ConversationStorage, msgAndLikes: MessageAndLikesStorage, storage: ZmsDatabase) {

  private implicit val tag: LogTag = s"ConvMessagesIndex_$conv"

  import com.waz.utils.events.EventContext.Implicits.global
  private implicit val dispatcher = new SerialDispatchQueue(name = "ConvMessagesIndex")

  private val indexUpdated = EventStream[Instant]() // will send timestamp after index is updated in db
  private val lastLocalMessageByType = new mutable.HashMap[Message.Type, MessageData]
  private var firstMessage = Option.empty[MessageData]

  private var lastEventId = EventId.Zero

  private object sources {
    val failedCount = Signal(0)
    val missedCall = Signal(Option.empty[MessageData])
    val incomingKnock = Signal(Option.empty[MessageData])
    val lastMessage = Signal(Option.empty[MessageData])
    val lastSentMessage = Signal(Option.empty[MessageData])
    val lastReadTime = returning(Signal[Instant]())(_.disableAutowiring())
  }

  object signals {
    val lastReadTime: Signal[Instant] = sources.lastReadTime
    val failedCount: Signal[Int] = sources.failedCount
    val lastMissedCall: Signal[Option[MessageId]] = Signal(sources.lastReadTime, sources.missedCall).map { case (time, msg) => msg.filter(_.time.isAfter(time)).map(_.id) }
    val incomingKnock: Signal[Option[MessageId]] = Signal(sources.lastReadTime, sources.incomingKnock).map { case (time, msg) => msg.filter(_.time.isAfter(time)).map(_.id) }
    val lastMessage: Signal[Option[MessageData]] = returning(sources.lastMessage)(_.disableAutowiring())
    val lastSentMessage: Signal[Option[MessageData]] = returning(sources.lastSentMessage)(_.disableAutowiring())

    val unreadCount = for {
      time <- sources.lastReadTime
      _ <- Signal.wrap(Instant.now, indexUpdated).throttle(500.millis)
      unread <- Signal.future(messages.countUnread(conv, time))
    } yield unread

    val messagesCursor: Signal[MessagesCursor] = new RefreshingSignal(loadCursor, indexUpdated)
  }

  import sources._

  private val init = convs.get(conv).flatMap { c =>
    c foreach updateLastRead

    storage.read { implicit db =>
      logTime(s"Initial load conversation entries for: $conv") {

        lastLocalMessageByType ++= MessageDataDao.listLocalMessages(conv).groupBy(m => m.msgType).map {
          case (tpe, msgs) => tpe -> msgs.maxBy(_.time)
        }
        missedCall ! MessageDataDao.lastMissedCall(conv)
        incomingKnock ! MessageDataDao.lastIncomingKnock(conv, selfUserId)
        failedCount ! MessageDataDao.countFailed(conv).toInt
        firstMessage = MessageDataDao.first(conv)
        val last = MessageDataDao.last(conv)
        lastMessage ! last
        lastEventId = last.fold(EventId.Zero)(_.source)

        lastSentMessage ! MessageDataDao.lastSent(conv)
      }
    }.map { _ =>
      Signal(signals.unreadCount, signals.failedCount, signals.lastMissedCall, signals.incomingKnock).throttle(500.millis) { case (unread, failed, missed, knock) =>
        verbose(s"update conversation state: unread: $unread, failed: $failed, missed: $missed, knock: $knock")
        convs.update(conv, _.copy(incomingKnockMessage = knock, missedCallMessage = missed, unreadCount = unread, failedCount = failed))
      }
    }
  }.recoverWithLog(reportHockey = true)

  def updateLastRead(c: ConversationData) = lastReadTime.mutateOrDefault(_ max c.lastRead, c.lastRead)

  private[waz] def loadCursor = CancellableFuture.lift(init) flatMap { _ =>
    verbose(s"loadCursor for $conv")
    storage { implicit db =>
      val cursor = MessageDataDao.msgIndexCursor(conv)
      val time = lastReadTime.currentValue.getOrElse(Instant.EPOCH)
      val readMessagesCount = MessageDataDao.countAtLeastAsOld(conv, time).toInt
      verbose(s"index of $time = $readMessagesCount")
      (cursor, time, math.max(0, readMessagesCount - 1))
    } ("ConvMessageIndex_loadCursor") map { case (cursor, time, lastReadIndex) =>
      new MessagesCursor(conv, cursor, lastReadIndex, time, msgAndLikes)
    }
  }

  def getLastMessage = init.map { _ => signals.lastMessage.currentValue.flatten }

  def getLastSentMessage = init.map { _ => signals.lastSentMessage.currentValue.flatten }

  def lastLocalMessage(tpe: Message.Type) = init.map { _ => lastLocalMessageByType.get(tpe).map(_.id) }

  def firstMessageId = init.map { _ => firstMessage.map(_.id) }

  def getLastEventId = init.map { _ => lastEventId }

  private[content] def delete(msg: MessageData): Future[Unit] = init map { _ =>
    if (msg.state.isFailed) failedCount.mutate(c => math.max(c - 1, 0))

    if (msg.isLocal && lastLocalMessageByType.get(msg.msgType).exists(_.id == msg.id))
      lastLocalMessageByType.remove(msg.msgType)

    removeLast(_.id == msg.id)

    indexUpdated ! Instant.now
  }

  private[content] def delete(upTo: Instant = Instant.MAX): Future[Unit] = init map { _ =>
    failedCount ! 0 // XXX: this might be wrong, hopefully not too often

    lastLocalMessageByType.filter { case (_, index) => !index.time.isAfter(upTo) } foreach { case (k, _) => lastLocalMessageByType.remove(k) }
    firstMessage = firstMessage.filter(_.time.isAfter(upTo))
    incomingKnock.mutate(_.filter(_.time.isAfter(upTo)))

    removeLast(!_.time.isAfter(upTo))

    indexUpdated ! Instant.now
  }

  private def removeLast(f: MessageData => Boolean) = {
    val lastRemoved = lastMessage.mutate(_ filterNot f)
    val lastSentRemoved = lastSentMessage.mutate(_ filterNot f)

    if (lastRemoved || lastSentRemoved) {
      verbose("last message was removed, need to fetch it from db")
      storage.read { implicit db =>
        MessageDataDao.last(conv) foreach updateLast
        MessageDataDao.lastSent(conv) foreach updateLastSent
      }
    }
  }

  private[content] def add(msgs: Seq[MessageData]): Future[Unit] = init map { _ =>
    msgs foreach { msg =>
      verbose(s"add($msg), last: ${lastMessage.currentValue}")

      if (msg.isLocal && lastLocalMessageByType.get(msg.msgType).forall(_.time.isBefore(msg.time)))
        lastLocalMessageByType(msg.msgType) = msg

      if (!msg.isLocal) {
        if (msg.msgType == Message.Type.MISSED_CALL) missedCall.mutate(_.filter(_.time.isAfter(msg.time)).orElse(Some(msg)))
        if (msg.msgType == Message.Type.KNOCK && msg.userId != selfUserId) incomingKnock.mutate(_.filter(_.time.isAfter(msg.time)).orElse(Some(msg)))
      }
    }

    if (msgs.nonEmpty) {
      val firstIter = msgs.iterator.filter(m => MessagesStorage.FirstMessageTypes(m.msgType))
      if (firstIter.nonEmpty) {
        val first = firstIter.minBy(_.time)
        if (first.source.sequence < 20 && firstMessage.forall(_.time.isAfter(first.time)))
          firstMessage = Some(first)
      }

      val failed = msgs.count(_.state == Status.FAILED)
      if (failed > 0) failedCount.mutate(_ + failed)

      updateLast(msgs)

      indexUpdated ! Instant.now
    }
  }

  private[content] def update(updates: Seq[(MessageData, MessageData)]): Future[Unit] = init map { _ =>
    updates foreach { case (msg, updated) =>
      verbose(s"update($msg, $updated)")
      assert(msg.id == updated.id, "trying to change message id")

      if (msg.isLocal && !updated.isLocal && lastLocalMessageByType.get(msg.msgType).exists(_.id == msg.id))
        lastLocalMessageByType.remove(msg.msgType)
      else if (updated.isLocal && !msg.isLocal)
        error(s"non-local message was updated to local: $msg -> $updated")
    }

    if (updates.nonEmpty) {
      val failed = updates.foldLeft(0) { case (count, (p, u)) =>
        if (p.state.isFailed == u.state.isFailed) count
        else if (u.state.isFailed) count + 1 else count - 1
      }
      if (failed != 0) failedCount.mutate(_ + failed)

      updateLast(updates.view.map(_._2))

      if (updates.exists { case (prev, up) => prev.convId == conv && (prev.time != up.time || !prev.hasSameContentType(up)) })
        indexUpdated ! Instant.now()
    }
  }

  private def updateLast(msgs: Iterable[MessageData]): Unit = {
    updateLast(msgs.maxBy(_.time))

    val sent = msgs.filter(_.state == Status.SENT)
    if (sent.nonEmpty)
      updateLastSent(sent.maxBy(_.time))

    lastEventId = lastEventId max msgs.maxBy(_.source).source
  }

  private def updateLast(last: MessageData): Unit =
    if (lastMessage.currentValue.forall(_.forall(!_.time.isAfter(last.time))))
      lastMessage.mutate(_.filter(_.time.isAfter(last.time)).orElse(Some(last)))

  private def updateLastSent(last: MessageData): Unit =
    if (lastSentMessage.currentValue.forall(_.forall(!_.time.isAfter(last.time))))
      lastSentMessage.mutate(_.filter(_.time.isAfter(last.time)).orElse(Some(last)))
}
