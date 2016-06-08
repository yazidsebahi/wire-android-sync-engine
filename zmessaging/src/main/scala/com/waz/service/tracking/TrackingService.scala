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
package com.waz.service.tracking

import java.util.regex.Pattern.{CASE_INSENSITIVE, compile}

import com.waz.ZLog._
import com.waz.api.KindOfCallingEvent.CALL_ESTABLISHED
import com.waz.api.Message.Type.{ASSET, KNOCK, RICH_MEDIA, TEXT}
import com.waz.content.{ConversationStorage, MessagesStorage, UsersStorage}
import com.waz.model.ConversationData.ConversationType.{Group, OneToOne}
import com.waz.model.MessageData.MessageEntry
import com.waz.model.UserData.UserDataDao.findWelcomeBot
import com.waz.model._
import com.waz.service.UserService
import com.waz.service.call.CallLogService
import com.waz.utils._
import com.waz.utils.events.{AggregatingSignal, EventStream, Signal}

import scala.concurrent.Future
import scala.language.postfixOps

class TrackingService(conversations: ConversationStorage, callLog: CallLogService, messages: MessagesStorage,
    userService: UserService, usersStorage: UsersStorage) {

  import TrackingService._
  import TrackingStats._
  import com.waz.threading.Threading.Implicits.Background

  val trackingSignal = Signal.future(userService.selfUserOrFail.logFailure(true)) flatMap { self =>
    val statsSignal = new AggregatingSignal[Op, TrackingStats](statsChanges(self), loadStats(self), (stats, op) => op(stats), stashing = false)
    val botStatsSignal =
      for {
        botRConvId <- new AggregatingSignal[RConvId, Option[RConvId]](botConvChanges, loadBotConv, (b, a) => Some(a))
        botConvId  <- Signal.future(botRConvId.flatMapFuture(conversations.getByRemoteId).map(_.map(_.id)).logFailure(true))
        botStats   <- new AggregatingSignal[Op, TrackingStats](botChanges(self, botConvId), loadBotStats(self, botConvId), (stats, op) => op(stats))
      } yield botStats

    statsSignal.combine(botStatsSignal)(_ + _)
  }

  private def botConvChanges: EventStream[RConvId] = usersStorage.onChanged.map(_.find(isBot).flatMap(_.conversation)).collect { case Some(id) => id }
  private def loadBotConv: Future[Option[RConvId]] = usersStorage.find(isBot, findWelcomeBot(_), _.conversation).map(_.headOption.flatten).logFailure(true)

  private def statsChanges(self: UserId): EventStream[Op] =
    EventStream.union[Op](
      conversations.convAdded.map(c => Add(convDiff(c))),
      conversations.convDeleted.map(c => Subtract(convDiff(c))),
      conversations.convUpdated.map { case (old, nu) => Fold(Subtract(convDiff(old)), Add(convDiff(nu))) },
      callLog.callLogEntryAdded.map(e => Add(callDiff(e))),
      messages.onAdded.map(ms => Fold(ms.map(m => Add(msgDiff(self, m))):_*)),
      messages.onUpdated.map(ms => Fold(ms.map { case (old, nu) => Fold(Subtract(msgDiff(self, old)), Add(msgDiff(self, nu))) }:_*)),
      usersStorage.onAdded.map(us => Fold(us.map(u => Add(userDiff(u))):_*)),
      usersStorage.onUpdated.map(us => Fold(us.map { case (old, nu) => Fold(Subtract(userDiff(old)), Add(userDiff(nu))) }:_*)))

  private def loadStats(self: UserId): Future[TrackingStats] = {
    for {
      convs  <- conversations.list
      users  <- usersStorage.list
      voice  <- callLog.numberOfEstablishedVoiceCalls
      video  <- callLog.numberOfEstablishedVideoCalls
      texts  <- messages.countSentByType(self, TEXT)
      rich   <- messages.countSentByType(self, RICH_MEDIA)
      images <- messages.countSentByType(self, ASSET)
    } yield {
      val initial = TrackingStats(0, 0, 0, 0, 0, 0, voice, video, texts + rich, images, 0)
      val diffs = users.iterator.map(userDiff) ++ convs.iterator.map(convDiff)
      diffs.foldLeft(initial)(_ + _)
    }
  } logFailure true

  private def botChanges(self: UserId, botConvId: Option[ConvId]): EventStream[Op] =
    EventStream.union[Op](
      messages.onAdded.map(ms => Fold(ms.map(m => Add(botDiff(self, botConvId, m))):_*)),
      messages.onUpdated.map(ms => Fold(ms.map { case (old, nu) => Fold(Subtract(botDiff(self, botConvId, old)), Add(botDiff(self, botConvId, nu))) }:_*)))

  private def loadBotStats(self: UserId, botConvId: Option[ConvId]): Future[TrackingStats] =
    botConvId.mapFuture(c => messages.countMessages(c, isMsgFrom(self))).map(_.fold2(TrackingStats.Empty, TrackingStats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, _))).logFailure(true)

  private def convDiff(c: ConversationData) = TrackingStats((c.convType == Group)?, c.archived?, c.muted?,
    (c.convType == OneToOne && !c.hidden)?, (c.convType == OneToOne && c.hidden)?, 0, 0, 0, 0, 0, 0)

  private def callDiff(c: CallLogEntry) = TrackingStats(0, 0, 0, 0, 0, 0, (c.event == CALL_ESTABLISHED && ! c.isVideo)?,
    (c.event == CALL_ESTABLISHED && c.isVideo)?, 0, 0, 0)

  private def msgDiff(self: UserId, m: MessageData) = TrackingStats(0, 0, 0, 0, 0, 0, 0, 0,
    (m.userId == self && (m.msgType == TEXT || m.msgType == RICH_MEDIA))?, (m.userId == self && m.msgType == ASSET)?, 0)

  private def botDiff(self: UserId, botConv: Option[ConvId], m: MessageData) = TrackingStats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, isBotInteraction(self, botConv, m)?)

  private def userDiff(u: UserData) = TrackingStats(0, 0, 0, 0, 0, isAutoConnect(u)?, 0, 0, 0, 0, 0)

  private val botEmail = compile("welcome(\\+\\d+)?@wire\\.com", CASE_INSENSITIVE)
  private def isBot(u: UserData) = u.email.exists(e => botEmail.matcher(e.str).matches)
  private def isAutoConnect(u: UserData) = u.isAutoConnect && ! isBot(u)
  private def isMsgFrom(self: UserId)(m: MessageEntry) = m.user == self && interactive(m.tpe)
  private def isBotInteraction(self: UserId, bot: Option[ConvId], m: MessageData) = m.userId == self && interactive(m.msgType) && bot.exists(_ == m.convId)
  private val interactive = Set(ASSET, KNOCK, TEXT, RICH_MEDIA, ASSET)
}

object TrackingService {
  private implicit val logTag: LogTag = logTagFor[TrackingService]

  private implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def ? : Int = if (b) 1 else 0
  }
}

case class TrackingStats(groups: Int, archived: Int, muted: Int, contacts: Int, blocked: Int,
    autoConnected: Int, voiceCalls: Int, videoCalls: Int, textsSent: Int, imagesSent: Int, botInteractions: Int) {

  import TrackingStats._

  def +(b: TrackingStats) = combine(plus, this, b)
  def -(b: TrackingStats) = combine(minus, this, b)

}

object TrackingStats {
  val Empty = TrackingStats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  sealed trait Op { def apply(left: TrackingStats): TrackingStats }
  abstract class BinOp(right: TrackingStats, f: (Int, Int) => Int) extends Op { def apply(left: TrackingStats): TrackingStats = combine(f, left, right) }
  case class Add(stats: TrackingStats) extends BinOp(stats, plus)
  case class Subtract(stats: TrackingStats) extends BinOp(stats, minus)
  case class Fold(ops: Op*) extends Op { def apply(left: TrackingStats): TrackingStats = ops.foldLeft(left)((s, op) => op(s)) }

  private val plus = (_: Int) + (_: Int)
  private val minus = (_: Int) - (_: Int)

  private def combine(op : (Int, Int) => Int, a: TrackingStats, b: TrackingStats): TrackingStats =
    TrackingStats(
      op(a.groups, b.groups),
      op(a.archived, b.archived),
      op(a.muted, b.muted),
      op(a.contacts, b.contacts),
      op(a.blocked, b.blocked),
      op(a.autoConnected,b.autoConnected),
      op(a.voiceCalls, b.voiceCalls),
      op(a.videoCalls, b.videoCalls),
      op(a.textsSent, b.textsSent),
      op(a.imagesSent, b.imagesSent),
      op(a.botInteractions, b.botInteractions))
}
