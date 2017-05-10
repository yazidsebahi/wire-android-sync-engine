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
package com.waz.zms

import android.content.Intent
import android.util.Base64
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.model._
import com.waz.service.push.PushTokenService.PushSenderId
import com.waz.service.{LifecycleState, ZMessaging}
import com.waz.sync.client.PushNotification
import com.waz.threading.Threading
import com.waz.utils.{JsonDecoder, LoggedTry, TimedWakeLock}
import org.json
import org.json.JSONObject

import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

class CloudMessageHandlerService extends FutureService with ZMessagingService {
  import Threading.Implicits.Background

  lazy val pushSenderId = ZMessaging.currentGlobal.backend.pushSenderId
  lazy val accounts = ZMessaging.currentAccounts

  override lazy val wakeLock = new TimedWakeLock(getApplicationContext, 2.seconds)

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = wakeLock.async {
    import com.waz.zms.CloudMessageHandlerService._

    def extrasToString = Option(intent.getExtras).map(ex => ex.keySet().asScala.map(k => s"$k -> ${ex.get(k)}").toSeq)

    verbose(s"onIntent with extras: $extrasToString")
    accounts.getCurrentZms.flatMap {
      case Some(zms) =>

        def decryptNotification(content: Array[Byte], mac: Array[Byte]) =
          zms.otrService.decryptGcm(content, mac) map {
            case Some(EncryptedNot(notification)) => Some(notification)
            case resp =>
              warn(s"gcm decoding failed: $resp")
              None
          }

        def addNotificationToProcess(nId: Uid, not: Option[PushNotification] = None): Future[Any] = {
          zms.lifecycle.lifecycleState.head flatMap {
            case LifecycleState.UiActive | LifecycleState.Active => Future.successful(()) // no need to process GCM when ui is active
            case _ =>
              verbose(s"addNotification: $nId")
              // call state events can not be directly dispatched like the other events because they might be stale
              not.foreach(n => syncCallStateForConversations(n.events.collect { case e: CallStateEvent => e }.map(_.withCurrentLocalTime())))
              Future.successful(zms.push.cloudPushNotificationsToProcess.mutate(_ + nId))
          }
        }

        def syncCallStateForConversations(events: Seq[Event]): Unit = {
          val convs: Set[RConvId] = events.collect { case e: CallStateEvent => e.convId }(breakOut)
          Future.traverse(convs) { convId =>
            zms.convsContent.processConvWithRemoteId(convId, retryAsync = false) { conv =>
              zms.sync.syncCallState(conv.id, fromFreshNotification = true)
            }
          }
        }

        intent match {

          case FromExtra(from) if from != pushSenderId =>
            info(s"received gcm notification is from unexpected sender, will ignore it")
            Future.successful({})

          case EncryptedContent(userId, content, mac) =>
            if (zms.selfUserId == userId) decryptNotification(content, mac) flatMap {
              case Some(notification) => addNotificationToProcess(notification.id, Some(notification))
              case None => Future.successful(())
            } else
              warn(s"received notification for wrong user"); Future.successful(())


          case UnencryptedNotification(userId, notification) =>
            if (zms.selfUserId == userId)
              addNotificationToProcess(notification.id, Some(notification))
            else
              warn(s"received notification for wrong user"); Future.successful(())


          case LargeNotification(nId) =>
            warn("Received large notification - need to trigger fetch")
            addNotificationToProcess(nId)

          case _ => warn(s"Unexpected notification"); Future.successful({})
        }
      case _ => warn("No zms instance available"); Future.successful({})
    }
  }
}

object CloudMessageHandlerService {
  val ContentExtra = "data"
  val UserExtra = "user"
  val TypeExtra = "type"
  val MacExtra = "mac"

  object FromExtra {
    def unapply(intent: Intent): Option[PushSenderId] = Option(intent.getStringExtra("from")) map PushSenderId
  }

  object EncryptedContent {
    def unapply(intent: Intent): Option[(UserId, Array[Byte], Array[Byte])] =
      (Option(intent.getStringExtra(UserExtra)), Option(intent.getStringExtra(TypeExtra)), Option(intent.getStringExtra(ContentExtra)), Option(intent.getStringExtra(MacExtra))) match {
        case (Some(userId), Some("otr" | "cipher"), Some(content), Some(mac)) =>
          LoggedTry.local { (UserId(userId), Base64.decode(content, Base64.NO_WRAP | Base64.NO_CLOSE), Base64.decode(mac, Base64.NO_WRAP | Base64.NO_CLOSE)) } .toOption
        case _ => None
      }
  }

  object UnencryptedNotification {
    def unapply(intent: Intent): Option[(UserId, PushNotification)] =
      (Option(intent.getStringExtra(ContentExtra)), Option(intent.getStringExtra(UserExtra))) match {
        case (Some(content), Some(user)) =>
          LoggedTry.local { (UserId(user), PushNotification.NotificationDecoder(new JSONObject(content))) } .toOption
        case _ => None
      }
  }

  //Note, large notifications exceeding 2KB of data don't contain the user id
  //TODO, maybe we can ask BE for this information
  object LargeNotification {
    def unapply(intent: Intent): Option[Uid] = Option(intent.getStringExtra(ContentExtra)).flatMap(s => Try(JsonDecoder.decodeUid('id)(new json.JSONObject(s))).toOption)
  }

  object EncryptedNot {
    def unapply(js: JSONObject): Option[PushNotification] = LoggedTry.local { PushNotification.NotificationDecoder(js.getJSONObject("data")) }.toOption
  }
}
