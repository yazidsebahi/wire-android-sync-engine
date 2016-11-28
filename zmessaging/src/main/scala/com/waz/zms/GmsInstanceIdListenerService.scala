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

import com.google.android.gms.iid.InstanceIDListenerService
import com.waz.ZLog._
import com.waz.service.ZMessaging
import com.waz.threading.Threading

import scala.concurrent.Future

class GmsInstanceIdListenerService extends InstanceIDListenerService with ZMessagingService {
  private implicit val logTag: LogTag = logTagFor[GmsInstanceIdListenerService]
  import Threading.Implicits.Background

  // it's possible to test that this works via
  // adb shell am startservice -a com.google.android.gms.iid.InstanceID --es "CMD" "RST" -n com.waz.zclient.dev/com.waz.zms.GmsInstanceIdListenerService
  // this requires temporary setting android:exported="true" for the service, though (don't forget to change back afterwards!)
  override def onTokenRefresh(): Unit = {
    info("GCM: onTokenRefresh() called")
    ZMessaging.currentAccounts.getCurrentZms flatMap {
      case Some(zms) =>
        debug("clearing gcm token and requesting re-registration with gcm")
        zms.sync.resetGcm()
      case None =>
        debug(s"ZMessaging not available, not registering")
        Future.successful(())
    }
  }
}
