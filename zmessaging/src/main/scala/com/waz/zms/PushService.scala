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

import android.app.{AlarmManager, PendingIntent, Service}
import android.content.{BroadcastReceiver, Context, Intent}
import android.support.v4.content.WakefulBroadcastReceiver
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.service.ZMessaging
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils._
import com.waz.znet.WebSocketClient

import scala.concurrent.Future

/**
  * Receiver called on boot or when app is updated.
  */
class PushBroadcastReceiver extends BroadcastReceiver {
  override def onReceive(context: Context, intent: Intent): Unit = {
    debug(s"onReceive $intent")
    WakefulBroadcastReceiver.startWakefulService(context, new Intent(context, classOf[PushService]))
  }
}


/**
  * Service keeping the process running as long as web socket should be connected.
  */
class PushService extends FutureService {

  private def context = getApplicationContext
  private lazy val alarmService = context.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
  private lazy val restartIntent = PendingIntent.getService(context, 89426, new Intent(context, classOf[PushService]), PendingIntent.FLAG_ONE_SHOT)

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = wakeLock {
    verbose(s"onStartCommand($intent, $startId)")

    Option(intent) foreach WakefulBroadcastReceiver.completeWakefulIntent

    onIntent(intent, startId).onComplete(_ => onComplete(startId))

    Service.START_STICKY
  }

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = wakeLock async {

    // schedule service restart every couple minutes to send ping on web socket (needed to keep connection alive)
    def scheduleRestarts() = {
      alarmService.setRepeating(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + WebSocketClient.PING_INTERVAL.toMillis, WebSocketClient.PING_INTERVAL.toMillis, restartIntent)
    }

    val currentZms = Option(ZMessaging.currentAccounts).fold2(Future successful None, _.getCurrentZms)

    currentZms flatMap {
      case None =>
        warn("Current ZMessaging not available, stopping")
        alarmService.cancel(restartIntent)
        Future successful None

      case Some(zms) if !zms.websocket.webSocketAlwaysOn =>
        verbose(s"WebSocket is not always ON, stopping")
        alarmService.cancel(restartIntent)
        zms.gcm.ensureGcmRegistered() map { _ => None } // let's make sure GCM is registered since we rely on it for push notifications

      case Some(zms) =>
        verbose(s"current zms: $zms, scheduling restarts")
        scheduleRestarts()
        zms.websocket.verifyConnection() map { _ => Some(zms) } recover { case _ => Some(zms) }
    }
  } flatMap {
    case None => Future.successful(())
    case Some(zms) => zms.websocket.awaitActive()
  }
}

object PushService {
  def apply(context: Context) = context.startService(new Intent(context, classOf[PushService]))
}
