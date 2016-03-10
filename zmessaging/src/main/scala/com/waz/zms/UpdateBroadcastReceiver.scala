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

import android.content.{BroadcastReceiver, Context, Intent}
import android.support.v4.content.WakefulBroadcastReceiver
import com.waz.ZLog._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.EventContext

import scala.concurrent.Future

class UpdateBroadcastReceiver extends BroadcastReceiver {
  private implicit val tag: LogTag = logTagFor[UpdateBroadcastReceiver]

  override def onReceive(context: Context, intent: Intent): Unit = {
    debug(s"onReceive $intent")
    WakefulBroadcastReceiver.startWakefulService(context, new Intent(context, classOf[UpdateService]))
  }
}

class UpdateService extends WakefulFutureService with ZMessagingService {
  implicit val ec = EventContext.Global
  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[UpdateService]

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = {
    debug(s"onIntent $intent")
    ZMessaging.currentInstance.getCurrent flatMap {
      case Some(zms) => zms.gcm.ensureGcmRegistered()
      case None =>
        debug(s"ZMessaging not available, not registering")
        Future.successful({})
    }
  }
}
