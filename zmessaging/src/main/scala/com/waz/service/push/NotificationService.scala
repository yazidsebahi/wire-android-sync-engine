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
package com.waz.service.push

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.Message
import com.waz.api.NotificationsHandler.GcmNotification
import com.waz.api.NotificationsHandler.GcmNotification.LikedContent
import com.waz.api.NotificationsHandler.GcmNotification.Type._
import com.waz.content._
import com.waz.model.AssetStatus.UploadDone
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Asset
import com.waz.model.GenericContent.Asset.Original
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._
import com.waz.utils.events.{AggregatingSignal, EventStream, Signal}
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future

class NotificationService(selfUserId: UserId, messages: MessagesStorage, lifecycle: ZmsLifecycle,
    storage: NotificationStorage, usersStorage: UsersStorage, convs: ConversationStorage, reactionStorage: LikingsStorage,
    push: PushService, kv: KeyValueStorage, timeouts: Timeouts) {

  import NotificationService._
  import com.waz.utils.events.EventContext.Implicits.global

  private implicit val dispatcher = new SerialDispatchQueue(name = "NotificationService")

  @volatile private var uiActive = false

  private val lastUiVisibleTime = kv.keyValuePref("last_ui_visible_time", Instant.EPOCH)

  lifecycle.lifecycleState { state =>
    verbose(s"state updated: $state")
    uiActive = returning(state == LifecycleState.UiActive) { active =>
      if (active || uiActive) lastUiVisibleTime := Instant.now()
    }
  }

  val notificationEventsStage = EventScheduler.Stage[Event]({ (c, events) =>
    add(events collect {
      case ev @ UserConnectionEvent(_, _, _, userId, msg, ConnectionStatus.PendingFromOther, time, name) if ev.hasLocalTime =>
        NotificationData(s"$CONNECT_REQUEST-$userId", msg.getOrElse(""), ConvId(userId.str), userId, CONNECT_REQUEST, time.instant, userName = name)
      case ev @ UserConnectionEvent(_, _, _, userId, _, ConnectionStatus.Accepted, time, name) if ev.hasLocalTime =>
        NotificationData(s"$CONNECT_ACCEPTED-$userId", "", ConvId(userId.str), userId, CONNECT_ACCEPTED, time.instant, userName = name)
      case ContactJoinEvent(_, userId, _) =>
        verbose("ContactJoinEvent")
        NotificationData(s"$CONTACT_JOIN-$userId", "", ConvId(userId.str), userId, CONTACT_JOIN, Instant.EPOCH)
    })
  }, _ => ! uiActive)

  /**
    * Map containing lastRead time for every conversation.
    * For muted conversations lastRead is always set to Instant.MAX,
    * we don't want to show notifications for muted conversations.
    */
  private val lastReadMap = {
    def convLastRead(c: ConversationData) = if (c.muted) Instant.MAX else c.lastRead

    val timeUpdates = EventStream.union(
      convs.onAdded,
      convs.onUpdated map { _ collect { case (prev, conv) if convLastRead(prev) != convLastRead(conv) => conv } }
    ) filter(_.nonEmpty)

    def loadAll() = convs.getAll.map(_.map(c => c.id -> convLastRead(c)).toMap)

    def update(times: Map[ConvId, Instant], updates: Seq[ConversationData]) =
      times ++ updates.map(c => c.id -> convLastRead(c))(breakOut)

    new AggregatingSignal(timeUpdates, loadAll(), update)
  }

  // remove older notifications when lastRead or uiTime is updated
  lastReadMap.zip(lastUiVisibleTime.signal).throttle(timeouts.notifications.clearThrottling) {
    case (lastRead, uiTime) => removeReadNotifications(lastRead, uiTime)
  }

  messages.onAdded { addForIncoming(_) }

  messages.onUpdated { updates =>
    // add notification when message sending fails
    val failedMsgs = updates collect {
      case (prev, msg) if prev.state != msg.state && msg.state == Message.Status.FAILED => msg
    }
    if (!uiActive && failedMsgs.nonEmpty) {
      storage.insert(failedMsgs map { msg => NotificationData(msg.id.str, msg.contentString, msg.convId, msg.userId, MESSAGE_SENDING_FAILED, Instant.EPOCH) })
    }

    // add notifications for uploaded assets
    val updatedAssets = updates collect {
      case (prev, msg) if msg.state == Message.Status.SENT && msg.msgType == Message.Type.ANY_ASSET => msg
    }
    if (updatedAssets.nonEmpty) addForIncoming(updatedAssets)
  }

  messages.onDeleted { ids =>
    storage.remove(ids.map(_.str))
  }

  private def addForIncoming(msgs: Seq[MessageData]) = {
    verbose(s"addForIncoming($msgs)")
    lastReadMap.head map { lastRead =>
      verbose(s"lastRead: $lastRead")
      msgs.filter(m => m.userId != selfUserId && lastRead.get(m.convId).forall(_.isBefore(m.time)) && m.localTime != Instant.EPOCH)
    } flatMap { ms =>
      verbose(s"filtered: $ms")
      storage.insert(ms flatMap notification)
    }
  }

  private def notification(msg: MessageData) = {
    import Message.Type._

    def data(tpe: GcmNotification.Type) = Some(NotificationData(msg.id.str, msg.contentString, msg.convId, msg.userId, tpe, msg.time, msg.localTime, mentions = msg.mentions.keys.toSeq, hotKnock = msg.hotKnock))

    msg.msgType match {
      case TEXT | TEXT_EMOJI_ONLY | RICH_MEDIA => data(GcmNotification.Type.TEXT)
      case KNOCK => data(GcmNotification.Type.KNOCK)
      case ASSET => data(GcmNotification.Type.ASSET)
      case LOCATION => data(GcmNotification.Type.LOCATION)
      case RENAME => data(GcmNotification.Type.RENAME)
      case MISSED_CALL => data(GcmNotification.Type.MISSED_CALL)
      case ANY_ASSET =>
        msg.protos.lastOption match {
          case Some(GenericMessage(_, Asset(Some(Original(Mime.Video(), _, _, _, _)), _, UploadDone(_)))) => data(GcmNotification.Type.VIDEO_ASSET)
          case Some(GenericMessage(_, Asset(Some(Original(Mime.Audio(), _, _, _, _)), _, UploadDone(_)))) => data(GcmNotification.Type.AUDIO_ASSET)
          case Some(GenericMessage(_, Asset(_, _, UploadDone(_)))) => data(GcmNotification.Type.ANY_ASSET)
          case _ => None
        }
      case MEMBER_JOIN =>
        if (msg.members == Set(msg.userId)) None // ignoring auto-generated member join event when user accepts connection
        else data(GcmNotification.Type.MEMBER_JOIN)
      case MEMBER_LEAVE => data(GcmNotification.Type.MEMBER_LEAVE)
      case _ => None
    }
  }

  reactionStorage.onChanged { reactions =>
    val reactionsFromOthers = reactions.filterNot(_.user == selfUserId)

    messages.getAll(reactionsFromOthers.map(_.message)).flatMap { msgs =>
      val convsByMsg = msgs.iterator.flatten.by[MessageId, Map](_.id).mapValues(_.convId)
      val myMsgs = msgs.collect { case Some(m) if m.userId == selfUserId => m.id }(breakOut): Set[MessageId]
      val rs = reactionsFromOthers.filter(r => myMsgs contains r.message).sortBy(_.timestamp)
      val (toRemove, toAdd) = rs.foldLeft((Set.empty[(MessageId, UserId)], Map.empty[(MessageId, UserId), Liking])) {
        case ((rs, as), r @ Liking(m, u, t, Liking.Action.Like))  => (rs - r.id, as + (r.id -> r))
        case ((rs, as), r @ Liking(m, u, t, Liking.Action.Unlike)) => (rs + r.id, as - r.id)
      }

      storage.remove(toRemove.map(reactionID)).flatMap { _ =>
        if (! uiActive)
          add(toAdd.valuesIterator.map(r => NotificationData(reactionID(r.id), "", convsByMsg.getOrElse(r.message, ConvId(r.user.str)), r.user, LIKE, Instant.EPOCH, referencedMessage = Some(r.message))).toVector)
        else
          Future.successful(Nil)
      }
    }.logFailure(reportHockey = true)
  }

  reactionStorage.onDeleted { ids =>
    storage.remove(ids.map(reactionID))
  }

  private def reactionID(id: (MessageId, UserId)) = s"$LIKE-$id"

  def clearNotifications() =
    for {
      _ <- lastUiVisibleTime := Instant.now()
      // will execute removeReadNotifications as part of this call,
      // this ensures that it's actually done while wakeLock is acquired by caller
      lastRead <- lastReadMap.head
      _ <- removeReadNotifications(lastRead, Instant.now)
    } yield ()

  private def removeReadNotifications(lastRead: Map[ConvId, Instant], uiTime: Instant) = {
    verbose(s"removeRead($lastRead, $uiTime)")

    def isRead(notification: NotificationData) =
      (notification.serverTime == Instant.EPOCH && uiTime.isAfter(notification.localTime)) || lastRead.get(notification.conv).exists(!_.isBefore(notification.serverTime))

    storage.notifications.head flatMap { data =>
      val toRemove = data collect {
        case (id, notification) if isRead(notification) => id
      }
      verbose(s"toRemove on lastRead change: $toRemove")
      storage.remove(toRemove)
    }
  }

  private def add(notifications: Seq[NotificationData]) =
    for {
      lastRead <- lastReadMap.head
      res <- storage.insert(notifications filter { n =>
        // filter notifications for unread messages in un-muted conversations
        n.serverTime == Instant.EPOCH || lastRead.get(n.conv).forall(_.isBefore(n.serverTime))
      })
      _ = verbose(s"inserted: $res")
    } yield res

  def getNotifications =
    lifecycle.lifecycleState flatMap {
      case LifecycleState.UiActive => Signal const Seq.empty[Notification]
      case _ =>
        for {
          data <- storage.notifications.map(_.values.toIndexedSeq.sorted)
          time <- lastUiVisibleTime.signal
          ns <-
            if (data.forall(_.localTime.isBefore(time))) Signal const Seq.empty[Notification] // no new messages, don't show anything
            else Signal.future(createNotifications(data))
        } yield
          ns
    }

  private def createNotifications(ns: Seq[NotificationData]): Future[Seq[Notification]] = {
    Future.traverse(ns) { data =>
      usersStorage.get(data.user).flatMap { user =>
        val userName = user map (_.getDisplayName) filterNot (_.isEmpty) orElse data.userName getOrElse ""

        data.msgType match {
          case CONNECT_REQUEST | CONNECT_ACCEPTED =>
            Future.successful(Notification(data, userName, userName, groupConv = false))
          case _ =>
            for {
              msg  <- data.referencedMessage.fold2(Future.successful(None), messages.getMessage)
              conv <- convs.get(data.conv)
            } yield {
              val (g, t) =
                if (data.msgType == LIKE) (data.copy(msg = msg.fold("")(_.contentString)), msg.map (m => if (m.msgType == Message.Type.ASSET) LikedContent.PICTURE else LikedContent.TEXT_OR_URL))
                else (data, None)

              conv.fold {
                Notification(g, "", userName, groupConv = false, mentioned = data.mentions.contains(selfUserId), likedContent = t)
              } { conv =>
                Notification(g, conv.displayName, userName, conv.convType == ConversationType.Group, mentioned = data.mentions.contains(selfUserId), likedContent = t)
              }
            }
        }
      }
    }
  }
}

object NotificationService {

  case class Notification(data: NotificationData, convName: String = "", userName: String = "", groupConv: Boolean = false, mentioned: Boolean = false, likedContent: Option[LikedContent] = None) extends GcmNotification {
    override def getType = data.msgType
    override def getConversationName: String = convName
    override def getConversationId: String = data.conv.str
    override def getMessage: String = data.msg
    override def getUserName: String = userName
    override def getUserId: String = data.user.str
    override def getTypeOfLikedContent: LikedContent = likedContent.getOrElse(LikedContent.TEXT_OR_URL)
    override def isGroupConversation: Boolean = groupConv
    override def isHotKnock: Boolean = data.hotKnock
    override def isUserMentioned: Boolean = mentioned

    override def getContent: String =
      if (data.user == UserId.Zero) data.msg
      else {
        (if (convName.isEmpty) "UNKNOWN" else convName) + "\n" + data.msg // XXX: this is only some fallback string to be used in UI until they implement correct handling
      }
  }
}
