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
package com.waz.service

import com.waz.ZLog._
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
import com.waz.utils._
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.duration._

class NotificationService(messages: MessagesStorage, lifecycle: ZmsLifecycle, storage: NotificationStorage, users: UserService, usersStorage: UsersStorage, convs: ConversationStorage, push: PushService) {
  import NotificationService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  @volatile private var uiActive = false

  lifecycle.lifecycleState { state =>
    verbose(s"state updated: $state")
    uiActive = state == LifecycleState.UiActive
    if (uiActive) clearNotifications()
  }

  val notificationEventsStage = EventScheduler.Stage[Event]({ (c, events) =>
    users.getSelfUserId map { selfUser =>
      // remove events from self user (may happen that we still have web socket open when ui is paused and new message comes from other device)
      val incoming = events collect {
        case e: MessageEvent if !selfUser.contains(e.from) => e
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
        case MessageAddEvent(id, convId, eventId, time, userId, msg)                                     => NotificationData(s"$TEXT-$convId-$id", msg, convId, userId, TEXT, time.instant)
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

  // add notification when message sending fails
  messages.onUpdated { updates =>
    val failedMsgs = updates collect {
      case (prev, msg) if prev.state != msg.state && msg.state == Message.Status.FAILED => msg
    }
    if (failedMsgs.nonEmpty) {
      // XXX: notification data keeps remoteId, so we need to fetch convs here
      Future.traverse(failedMsgs) { msg =>
        convs.get(msg.convId) map {
          case Some(conv) => Some(NotificationData(s"$MESSAGE_SENDING_FAILED-${msg.id}", msg.contentString, conv.remoteId, msg.userId, MESSAGE_SENDING_FAILED, Instant.EPOCH, referencedMessage = Some(msg.id)))
          case None => None // that's pretty much impossible
        }
      } map { ns => add(ns.flatten) }
    }
  }

  // remove older notifications when lastRead is updated
  convs.onUpdated { updates =>
    if (!uiActive) {
      val lastReadUpdated: Map[RConvId, Instant] = updates.collect {
        case (prev, conv) if conv.lastRead != prev.lastRead => conv.remoteId -> conv.lastRead
      } (breakOut)

      if (lastReadUpdated.nonEmpty) {
        storage.notifications.head flatMap { data =>
          val toClear = data.filter { case (id, notification) =>
            lastReadUpdated.get(notification.conv).exists { lastRead =>
              notification.serverTime != Instant.EPOCH && !lastRead.isBefore(notification.serverTime)
            }
          }
          if (toClear.isEmpty) Future.successful(())
          else storage.updateAll2(toClear.keys, { n =>
            n.copy(clearTime = n.clearTime.orElse(lastReadUpdated.get(n.conv)))
          })
        }
      }
    }
  }

  private def add(notifications: => Seq[NotificationData]) =
    if (!uiActive) {
      val ns: Map[String, NotificationData] = notifications.map { n => n.id -> n } (breakOut)
      if (ns.nonEmpty) storage.updateOrCreateAll2(ns.keys, (id, n) => n.getOrElse(ns(id)))
    }

  def getNotifications(throttleDelay: FiniteDuration = 1.second) = storage.notifications.throttle(throttleDelay).map(_.values.toIndexedSeq.sorted) flatMap { data => Signal.future(createNotifications(data)) }

  def clearNotifications() = for {
    data <- storage.notifications.head
    time = Instant.now
    _ <- storage.updateAll2(data.keys, _.copy(clearTime = Some(time)))
    _ <- storage.deleteClearedBefore(time - 1.hour)
  } yield ()

  private def createNotifications(ns: Seq[NotificationData]): Future[Seq[Notification]] =
    users.withSelfUserFuture { selfUserId =>
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
  private implicit val Tag: LogTag = logTagFor[NotificationService]

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
