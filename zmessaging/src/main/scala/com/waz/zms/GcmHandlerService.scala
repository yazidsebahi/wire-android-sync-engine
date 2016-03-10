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

import android.content.{Context, Intent}
import android.util.Base64
import com.waz.ZLog._
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.sync.client.PushNotification
import com.waz.threading.Threading
import org.json.JSONObject

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.Try

class GcmHandlerService extends WakefulFutureService with ZMessagingService {

  private implicit val logTag: LogTag = logTagFor[GcmHandlerService]
  import Threading.Implicits.Background

  lazy val gcm = ZMessaging.currentGlobal.gcmGlobal
  lazy val instance = ZMessaging.currentInstance

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = {
    import com.waz.zms.GcmHandlerService._

    def extrasToString = Option(intent.getExtras).map(ex => ex.keySet().asScala.map(k => s"$k -> ${ex.get(k)}").toSeq)

    verbose(s"onIntent with extras: $extrasToString")

    if (intent.getAction == ActionClear) {
      instance.getCurrent flatMap {
        case Some(zms) => zms.notifications.clearNotifications()
        case None => Future.successful(())
      }
    } else if (intent.hasExtra("from") && intent.getStringExtra("from") != gcm.gcmSenderId.str) {
      info(s"received gcm notification is from unexpected sender, will ignore it")
      Future.successful(())
    } else if (intent.hasExtra(TypeExtra) && intent.hasExtra(ContentExtra) && intent.hasExtra(MacExtra) && (intent.getStringExtra(TypeExtra) == "otr" || intent.getStringExtra(TypeExtra) == "cipher")) {
      val content = intent.getStringExtra(ContentExtra)
      val mac = intent.getStringExtra(MacExtra)
      instance.getCurrent.map {
        case Some(zms) => zms.otrService.decryptGcm(Base64.decode(content, Base64.NO_WRAP | Base64.NO_CLOSE), Base64.decode(mac, Base64.NO_WRAP | Base64.NO_CLOSE)) map {
          case Some(EncryptedGcm(notification)) => handleNotification(notification, None)
          case resp => warn(s"gcm decoding failed: $resp")
        }
        case None => warn("no current zmessaging instance")
      }
    } else if (intent.hasExtra(ContentExtra) && intent.hasExtra(UserExtra)) {
      val content = intent.getStringExtra(ContentExtra)
      val userId = intent.getStringExtra(UserExtra)
      Future {
        (PushNotification.NotificationDecoder(new JSONObject(content)), UserId(userId))
      } flatMap {
        case (notification, user) => handleNotification(notification, Some(user))
      }
    } else {
      error(s"Received GCM notification, but some required extra field is missing, intent extras: $extrasToString")
      Future.successful(())
    }
  }

  def handleNotification(n: PushNotification, user: Option[UserId]): Future[Any] = {
    verbose(s"handleNotification($n")
    handleGcmNotification(user) { zms =>
      val (callStateEvents, otherEvents) = n.events.partition(_.isInstanceOf[CallStateEvent])

      // call state events can not be directly dispatched like the other events because they might be stale
      handleCallStateNotifications(zms, callStateEvents)

      zms.eventPipeline(otherEvents)
    }
  }

  private def handleCallStateNotifications(zms: ZMessaging, events: Seq[Event]): Unit = events foreach {
    case e: CallStateEvent =>
      zms.convsContent.processConvWithRemoteId(e.convId, retryAsync = false) { conv =>
        zms.sync.syncCallState(conv.id, fromFreshNotification = true)
      }

    case _ => () // ignore all other events
  }

  def handleGcmNotification[A](user: Option[UserId])(body: ZMessaging => Future[A]) = {
    instance.getCurrent flatMap {
      case Some(zms) =>
        zms.users.getSelfUserId flatMap {
          case Some(userId) if user.forall(_ == userId) =>
            if (zms.push.pushConnected.currentValue.contains(true)) {
              debug(s"PushService is connected, ignoring GCM")
              Future.successful(())
            } else {
              body(zms)
            }
          case current =>
            error(s"Received GCM notification but current ZMessaging user is different from intended user, will re-register")
            gcm.unregister() flatMap { _ => zms.sync.registerGcm() }
        }
      case _ =>
        error(s"Received GCM notification but no ZMessaging is available, will unregister from Play Services")
        gcm.unregister()
    }
  }
}

object GcmHandlerService {
  val ContentExtra = "data"
  val TypeExtra = "type"
  val MacExtra = "mac"
  val UserExtra = "user"

  val ActionClear = "com.wire.gcm.CLEAR_NOTIFICATIONS"

  def clearNotificationsIntent(context: Context) = new Intent(context, classOf[GcmHandlerService]).setAction(ActionClear)

  object EncryptedGcm {
    def unapply(js: JSONObject): Option[PushNotification] = Try { PushNotification.NotificationDecoder(js.getJSONObject("data")) }.toOption
  }
}
