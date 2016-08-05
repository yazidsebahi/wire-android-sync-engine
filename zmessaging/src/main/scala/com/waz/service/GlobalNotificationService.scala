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

import java.util

import android.app.PendingIntent
import android.content.Context
import android.graphics.Bitmap
import com.waz.ZLog._
import com.waz.api.NotificationsHandler.{GcmNotification, NotificationsHandlerFactory}
import com.waz.api._
import com.waz.api.impl.ActiveChannel
import com.waz.bitmap
import com.waz.bitmap.BitmapUtils
import com.waz.model._
import com.waz.service.push.NotificationService.Notification
import com.waz.service.ZMessaging.EmptyNotificationsHandler
import com.waz.service.assets.AssetService.BitmapRequest.Regular
import com.waz.service.assets.AssetService.BitmapResult
import com.waz.service.images.BitmapSignal
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zms.GcmHandlerService

import scala.collection.JavaConverters._
import scala.concurrent.Future

class GlobalNotificationService(context: Context, currentZms: Signal[Option[ZMessaging]], handlerFactory: NotificationsHandlerFactory) {
  import EventContext.Implicits.global
  import com.waz.service.GlobalNotificationService._
  private implicit val dispatcher = new SerialDispatchQueue(name = "NotificationsService")
  private implicit val logTag: LogTag = logTagFor[GlobalNotificationService]

  private lazy val callImageSize = (GlobalNotificationService.CallImageSizeDp * context.getResources.getDisplayMetrics.density).toInt

  private val uiActive = currentZms flatMap { _.fold(Signal.const(false))(_.lifecycle.uiActive) }

  private val activeCalls = currentZms flatMap {
    case Some(zms) => zms.voiceContent.ongoingAndTopIncomingChannel flatMap { case (ongoing, incoming) => activeChannel(ongoing, zms) zip activeChannel(incoming, zms) }
    case None => Signal const (Option.empty[ActiveChannel], Option.empty[ActiveChannel])
  }

  private val notifications = currentZms flatMap { _.fold(Signal.const(Seq.empty[Notification]))(_.notifications.getNotifications()) }

  @volatile
  private var notificationsHandler: NotificationsHandler = EmptyNotificationsHandler

  Threading.Ui {
    notificationsHandler = handlerFactory.getNotificationsHandler
  }

  activeCalls.zip(uiActive).on(Threading.Ui) {
    case ((ongoing, incoming), active) =>
      debug(s"updateOngoingCallNotification(ongoing = $ongoing, incoming = $incoming, uiActive = $active)")
      notificationsHandler.updateOngoingCallNotification(ongoing.orNull, incoming.orNull, active)
  }

  notifications.on(Threading.Ui) { ns =>
    verbose(s"updateGcmNotifications($ns)")
    notificationsHandler.updateGcmNotification(new NotificationsList(context, ns))
  }


  private def activeChannel(channel: Option[VoiceChannelData], zms: ZMessaging): Signal[Option[ActiveChannel]] = {
    verbose(s"activeChannel($channel)")
    channel.fold(Signal.const(Option.empty[ActiveChannel])) { ch =>
      Signal.future(loadCallData(ch.id, ch.caller, zms)) flatMap { case CallData(convName, callerName, asset) =>
        bitmapSignal(asset, zms) map { bmp =>
          Some(new ActiveChannel(context, state = ch.state, callerName = callerName getOrElse "", kindOfCall = ch.tracking.kindOfCall, isVideoCall = ch.video.isVideoCall, convId = ch.id, convName = convName getOrElse "", picture = bmp))
        }
      }
    }
  }

  private def bitmapSignal(asset: Option[ImageAssetData], zms: ZMessaging): Signal[Bitmap] = {
    verbose(s"bitmapSignal($asset)")
    asset.fold(Signal.const(bitmap.EmptyBitmap)) { asset =>
      BitmapSignal(asset, Regular(callImageSize), zms.imageLoader, zms.imageCache).map {
        case BitmapResult.BitmapLoaded(bmp, _, _) => BitmapUtils.cropRect(bmp, callImageSize)
        case _ => bitmap.EmptyBitmap
      }
    }
  }

  private def loadCallData(convId: ConvId, caller: Option[UserId], zms: ZMessaging): Future[CallData] = {
    verbose(s"loadCallData($convId)")
    (for {
      conv <- zms.convsContent.convById(convId)
      user <- caller.fold(Future.successful(Option.empty[UserData]))(zms.users.getUser)
    } yield (conv, user)) flatMap {
      case (Some(conv), Some(user)) =>
        user.picture map zms.assetsStorage.getImageAsset getOrElse Future.successful(None) map (CallData(Some(conv.displayName), Some(user.displayName), _))

      case (Some(conv), None) => Future.successful(CallData(Some(conv.displayName), None, None))
      case (None, _) => Future.successful(CallData(None, None, None)) // you know nothing, Jon Snow
    }
  }
}

object GlobalNotificationService {

  val CallImageSizeDp = 64

  case class CallData(convName: Option[String], callerName: Option[String], callerPicture: Option[ImageAssetData])

  class NotificationsList(context: Context, notifications: Seq[GcmNotification]) extends GcmNotificationsList {

    lazy val gcmNotifications = notifications.asJava
    lazy val clearIntent = PendingIntent.getService(context, 9730, GcmHandlerService.clearNotificationsIntent(context), PendingIntent.FLAG_UPDATE_CURRENT)

    override def getNotifications: util.Collection[GcmNotification] = gcmNotifications

    override def getClearNotificationsIntent: PendingIntent = clearIntent
  }
}
