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
import com.waz.model.VersionBlacklist
import com.waz.service.VersionBlacklistService._
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{EventContext, Signal}

import scala.concurrent.duration._

class VersionBlacklistService(metadata: MetaDataService, prefs: PreferenceService, sync: SyncServiceHandle) {

  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[VersionBlacklistService]
  private implicit val ec = EventContext.Global
  import metadata._

  val upToDate = Signal[Boolean]()

  prefs.withPreferences { p =>
    val lastSyncTime = p.getLong(LastUpToDateSyncPref, 0L)
    val lastVersion = p.getInt(LastCheckedVersionPref, -1)
    val isUpToDate = p.getBoolean(UpToDatePref, true)

    upToDate ! (lastVersion != metadata.appVersion || isUpToDate)
    if (lastVersion != metadata.appVersion || (System.currentTimeMillis - lastSyncTime).millis > 1.day) sync.syncVersionBlacklist()
  }

  def updateBlacklist(blacklist: VersionBlacklist): CancellableFuture[Unit] = {
    verbose(s"app Version: $appVersion, blacklist: $blacklist")
    val isUpToDate = appVersion >= blacklist.oldestAccepted && !blacklist.blacklisted.contains(appVersion)
    upToDate ! isUpToDate
    prefs.editPreferences { prefs =>
      prefs.putBoolean(UpToDatePref, isUpToDate)
      prefs.putLong(LastUpToDateSyncPref, System.currentTimeMillis)
      prefs.putInt(LastCheckedVersionPref, appVersion)
    } map { _ => Unit }
  }
}

object VersionBlacklistService {
  val UpToDatePref = "UpToDate"
  val LastUpToDateSyncPref = "LastUpToDateSync"
  val LastCheckedVersionPref = "UpToDateVersion"
}
