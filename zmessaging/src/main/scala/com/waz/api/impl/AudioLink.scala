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
package com.waz.api.impl

import com.waz.ZLog._
import com.waz.api.UsersList
import com.waz.model.UserId
import com.waz.ui.{SignalLoading, UiModule}

class AudioLink(implicit ui: UiModule) extends com.waz.api.AudioLink  {
  private implicit val logTag: LogTag = logTagFor[AudioLink]

  verbose("initialized")

  lazy val nearbyUsers = new AudioLinkUsers
  
  override def getNearbyUsers = nearbyUsers
}


class AudioLinkUsers(implicit ui: UiModule) extends UsersList with CoreList[com.waz.api.User] with SignalLoading {
  private implicit val logTag: LogTag = logTagFor[AudioLinkUsers]

  var userIds = IndexedSeq.empty[UserId]

  verbose("starting")

  addLoader(_.audiolink.nearbyUsers) { ids =>
    verbose(s"usersSignal reported ids: $ids")
    userIds = ids.toIndexedSeq
    notifyChanged()
  }

  override def get(position: Int): User = ui.users.getUser(userIds(position))

  override def size(): Int = userIds.size
}
