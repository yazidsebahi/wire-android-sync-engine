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

import com.waz.ZLog._
import ImplicitTag._
import com.waz.api.Message
import com.waz.api.NotificationsHandler.GcmNotification
import com.waz.api.NotificationsHandler.GcmNotification.LikedContent
import com.waz.api.NotificationsHandler.GcmNotification.Type._
import com.waz.content._
import com.waz.model.AssetStatus.UploadDone
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Asset.Original
import com.waz.model.GenericContent.{Asset, ImageAsset, Knock, Location, Text}
import com.waz.model.Liking.Action.Like
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service._
import com.waz.utils._
import com.waz.utils.events.{AggregatingSignal, EventStream, Signal}
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future

class NotificationService(selfUserId: UserId, messages: MessagesStorage, lifecycle: ZmsLifecycle, storage: NotificationStorage, usersStorage: UsersStorage, convs: ConversationStorage, push: PushService, kv: KeyValueStorage, timeouts: Timeouts) {
  import NotificationService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  @volatile private var uiActive = false

  private val lastUiVisibleTime = kv.keyValuePref("last_ui_visible_time", Instant.EPOCH)

  lifecycle.lifecycleState { state =>
    verbose(s"state updated: $state")
    uiActive = returning(state == LifecycleState.UiActive) { active =>
      if (active || uiActive) lastUiVisibleTime := Instant.now()
    }
  }

  val notificationEventsStage = EventScheduler.Stage[Event]({ (c, events) =>
    Future {
      // remove events from self user
      val incoming = events collect {
        case e: MessageEvent if e.from != selfUserId => e
        case e: ContactJoinEvent => e
        case e: UserEvent => e
      }
      add(incoming collect {
        case ev @ UserConnectionEvent(_, convId, _, userId, msg, ConnectionStatus.PendingFromOther, time, name) if ev.hasLocalTime => NotificationData(s"$CONNECT_REQUEST-$convId-$userId", msg.getOrElse(""), convId, userId, CONNECT_REQUEST, time.instant, userName = name)
        case ev @ UserConnectionEvent(_, convId, _, userId, _, ConnectionStatus.Accepted, time, name) if ev.hasLocalTime => NotificationData(s"$CONNECT_ACCEPTED-$convId-$userId", "", convId, userId, CONNECT_ACCEPTED, time.instant, userName = name)
        case ContactJoinEvent(_, userId, _) => NotificationData(s"$CONTACT_JOIN-$userId", "", RConvId.Empty, userId, CONTACT_JOIN, Instant.EPOCH)
        case MemberJoinEvent(_, convId, eventId, time, userId, members) if members != Seq(userId) => NotificationData(s"$MEMBER_JOIN-$convId-$eventId", "", convId, userId, MEMBER_JOIN, time.instant) // ignoring auto-generated member join event when user accepts connection
        case MemberLeaveEvent(_, convId, eventId, time, userId, _) => NotificationData(s"$MEMBER_LEAVE-$convId-$eventId", "", convId, userId, MEMBER_LEAVE, time.instant)
        case RenameConversationEvent(_, convId, eventId, time, userId, name) => NotificationData(s"$RENAME-$convId-$eventId", "", convId, userId, RENAME, time.instant)
        case MissedCallEvent(_, convId, eventId, time, userId) => NotificationData(s"$MISSED_CALL-$convId-$eventId", "", convId, userId, MISSED_CALL, time.instant)
        case GenericMessageEvent(_, convId, time, userId, GenericMessage(id, Text(msg, mentions, _))) => NotificationData(s"$TEXT-$convId-$id", msg, convId, userId, TEXT, time.instant, mentions = mentions.keys.toSeq)
        case GenericMessageEvent(_, convId, time, userId, GenericMessage(id, Knock(hot))) => NotificationData(s"$KNOCK-$convId-$id-$hot", "", convId, userId, KNOCK, time.instant, hotKnock = hot)
        case GenericMessageEvent(_, convId, time, userId, GenericMessage(id, Like)) => NotificationData(s"$LIKE-$convId-$id-$userId", "", convId, userId, LIKE, time.instant, referencedMessage = Some(MessageId(id.str)))
        case GenericMessageEvent(_, convId, time, userId, GenericMessage(id, Location(_, _, _, _))) => NotificationData(s"$LOCATION-$convId-$id", "", convId, userId, LOCATION, time.instant)
        case GenericAssetEvent(_, convId, time, userId, GenericMessage(id, Asset(Some(Original(Mime.Video(), _, _, _, _)), _, UploadDone(_))), _, _) => NotificationData(s"$VIDEO_ASSET-$id", "", convId, userId, VIDEO_ASSET, time.instant)
        case GenericAssetEvent(_, convId, time, userId, GenericMessage(id, Asset(Some(Original(Mime.Audio(), _, _, _, _)), _, UploadDone(_))), _, _) => NotificationData(s"$AUDIO_ASSET-$id", "", convId, userId, AUDIO_ASSET, time.instant)
        case GenericAssetEvent(_, convId, time, userId, GenericMessage(id, Asset(_, _, UploadDone(_))), _, _) => NotificationData(s"$ANY_ASSET-$id", "", convId, userId, ANY_ASSET, time.instant)
        case GenericAssetEvent(_, convId, time, userId, GenericMessage(id, _: ImageAsset), _, _) => NotificationData(s"$ASSET-$id", "", convId, userId, ASSET, time.instant)
      })
    }
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

    def loadAll() = convs.getAll.map(_.map(c => c.remoteId -> convLastRead(c)).toMap)

    def update(times: Map[RConvId, Instant], updates: Seq[ConversationData]) =
      times ++ updates.map(c => c.remoteId -> convLastRead(c))(breakOut)

    new AggregatingSignal(timeUpdates, loadAll(), update)
  }

  // remove older notifications when lastRead or uiTime is updated
  lastReadMap.zip(lastUiVisibleTime.signal).throttle(timeouts.notifications.clearThrottling) {
    case (lastRead, uiTime) => removeReadNotifications(lastRead, uiTime)
  }

  // add notification when message sending fails
  messages.onUpdated { updates =>
    val failedMsgs = updates collect {
      case (prev, msg) if prev.state != msg.state && msg.state == Message.Status.FAILED => msg
    }
    if (!uiActive && failedMsgs.nonEmpty) {
      // XXX: notification data keeps remoteId, so we need to fetch convs here
      Future.traverse(failedMsgs) { msg =>
        convs.get(msg.convId) map {
          case Some(conv) => Some(NotificationData(s"$MESSAGE_SENDING_FAILED-${msg.id}", msg.contentString, conv.remoteId, msg.userId, MESSAGE_SENDING_FAILED, Instant.EPOCH, referencedMessage = Some(msg.id)))
          case None => None // that's pretty much impossible
        }
      } map { ns => add(ns.flatten) }
    }
  }

  def clearNotifications() =
    for {
      _ <- lastUiVisibleTime := Instant.now()
      // will execute removeReadNotifications as part of this call,
      // this ensures that it's actually done while wakeLock is acquired by caller
      lastRead <- lastReadMap.head
      _ <- removeReadNotifications(lastRead, Instant.now())
    } yield ()

  private def removeReadNotifications(lastRead: Map[RConvId, Instant], uiTime: Instant) = {
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
      // filter notifications for unread messages in un-muted conversations
      ns: Map[String, NotificationData] = notifications.collect {
        case n if n.serverTime == Instant.EPOCH => n.id -> n
        case n if lastRead.get(n.conv).forall(_.isBefore(n.serverTime)) => n.id -> n
      } (breakOut)
      res <- storage.updateOrCreateAll2(ns.keys, (id, n) => n.getOrElse(ns(id)))
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
            Future.successful(Notification(data, ConvId(data.user.str), userName, userName, groupConv = false))
          case _ =>
            for {
              msg  <- data.referencedMessage.fold2(Future.successful(None), messages.getMessage)
              conv <- convs.getByRemoteId(data.conv)
            } yield {
              val (g, t) =
                if (data.msgType == LIKE) (data.copy(msg = msg.fold("")(_.contentString)), msg.map (m => if (m.msgType == Message.Type.ASSET) LikedContent.PICTURE else LikedContent.TEXT_OR_URL))
                else (data, None)

              conv.fold {
                Notification(g, ConvId(data.conv.str), "", userName, groupConv = false, mentioned = data.mentions.contains(selfUserId), likedContent = t)
              } { conv =>
                Notification(g, conv.id, conv.displayName, userName, conv.convType == ConversationType.Group, mentioned = data.mentions.contains(selfUserId), likedContent = t)
              }
            }
        }
      }
    }
  }
}

object NotificationService {

  case class Notification(data: NotificationData, conv: ConvId, convName: String = "", userName: String = "", groupConv: Boolean = false, mentioned: Boolean = false, likedContent: Option[LikedContent] = None) extends GcmNotification {
    override def getType = data.msgType
    override def getConversationName: String = convName
    override def getConversationId: String = conv.str
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
