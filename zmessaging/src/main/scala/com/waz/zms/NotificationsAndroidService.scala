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

import android.app.PendingIntent
import android.content.{Context, Intent}
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.verbose
import com.waz.service.ZMessaging
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils._
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._

class NotificationsAndroidService extends FutureService {

  import NotificationsAndroidService._

  override protected lazy val wakeLock = new TimedWakeLock(getApplicationContext, 2.seconds)

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = wakeLock.async {
    Option(ZMessaging.currentAccounts).fold2(Future successful None, _.getCurrentZms).flatMap {
      case None => Future successful None
      case Some(zms) if ActionClear == intent.getAction =>
        verbose("Clearing notifications")
        Future.successful(zms.notifications.clearNotifications())
      case Some(zms) =>
        verbose("Other device no longer active, resetting otherDeviceActiveTime")
        Future.successful(zms.notifications.otherDeviceActiveTime ! Instant.EPOCH)
    }
  }
}

object NotificationsAndroidService {
  val ActionClear = "com.wire.CLEAR_NOTIFICATIONS"

  val checkNotificationsTimeout = 1.minute

  def clearNotificationsIntent(context: Context) = PendingIntent.getService(context, 9730, new Intent(context, classOf[NotificationsAndroidService]).setAction(ActionClear), PendingIntent.FLAG_UPDATE_CURRENT)
  def checkNotificationsIntent(context: Context) = PendingIntent.getService(context, 19047, new Intent(context, classOf[NotificationsAndroidService]), PendingIntent.FLAG_ONE_SHOT)
}
