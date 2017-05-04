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

import android.app.Activity
import android.content.{BroadcastReceiver, Context, Intent}
import android.support.v4.content.WakefulBroadcastReceiver
import com.waz.ZLog._

class GcmBroadcastReceiver extends BroadcastReceiver {
  private implicit val logTag: LogTag = logTagFor[GcmBroadcastReceiver]

  override def onReceive(context: Context, intent: Intent): Unit = {
    import scala.collection.JavaConverters._

    val extras = Option(intent.getExtras)
    val extrasToPrint = extras.map { ex => ex.keySet().asScala.map(k => (k, ex.get(k)))}
    verbose(s"Received gcm: $intent, extras: $extrasToPrint")

    val serviceIntent = new Intent(context, classOf[GcmHandlerService])
    extras.foreach(serviceIntent.putExtras)
    WakefulBroadcastReceiver.startWakefulService(context, serviceIntent)
    setResultCode(Activity.RESULT_OK)
  }
}
