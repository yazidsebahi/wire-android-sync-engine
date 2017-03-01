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

import com.waz.api
import com.waz.api.UsersList
import com.waz.model.{CommonConnectionsData, UserId}
import com.waz.service.UserSearchService
import com.waz.ui.{SignalLoading, UiModule}

class CommonConnections(userId: UserId)(implicit ui: UiModule) extends com.waz.api.CommonConnections with UiObservable with SignalLoading {

  var data = CommonConnectionsData(userId, 0, Nil)

  lazy val fullList = new CommonConnectionsList(userId, data.connections)

  override def getTopConnections: Array[api.User] = data.connections.take(UserSearchService.MinCommonConnections).map(ui.users.getUser).toArray

  override def getTotalCount: Int = data.totalCount

  override def getFullList: UsersList = fullList
}


class CommonConnectionsList(userId: UserId, var data: Seq[UserId] = Nil)(implicit ui: UiModule) extends UsersList with CoreList[com.waz.api.User] with SignalLoading {

  override def size: Int = data.size
  override def get(index: Int): User = ui.users.getUser(data(index))
}
