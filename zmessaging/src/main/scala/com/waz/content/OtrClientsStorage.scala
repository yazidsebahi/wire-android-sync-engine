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
package com.waz.content

import android.content.Context
import com.waz.ZLog._
import com.waz.model.UserId
import com.waz.model.otr.UserClients
import com.waz.model.otr.UserClients.UserClientsDao
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.{CachedStorage, TrimmingLruCache}

class OtrClientsStorage(context: Context, storage: Database) extends CachedStorage[UserId, UserClients](new TrimmingLruCache(context, Fixed(2000)), storage)(UserClientsDao, "OtrClientsStorage") {
  private implicit val tag: LogTag = logTagFor[OtrClientsStorage]
  private implicit val dispatcher = new SerialDispatchQueue(name = "OtrClientStorage")
}
