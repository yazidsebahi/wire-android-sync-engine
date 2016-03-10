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
package com.waz.sync.handler

import com.waz.ZLog._
import com.waz.model.VersionBlacklist
import com.waz.service.VersionBlacklistService
import com.waz.sync.SyncResult
import com.waz.sync.client.VersionBlacklistClient
import com.waz.threading.{CancellableFuture, Threading}

class VersionBlacklistSyncHandler(versionBlacklistService: VersionBlacklistService, client: VersionBlacklistClient) {

  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[VersionBlacklistSyncHandler]

  def syncVersionBlackList(): CancellableFuture[SyncResult] = client.loadVersionBlacklist() flatMap { blacklist =>
    debug(s"Retrieved version blacklist: $blacklist")
    versionBlacklistService.updateBlacklist(blacklist.right.getOrElse(VersionBlacklist()))
  } map { _ => SyncResult.Success }
}
