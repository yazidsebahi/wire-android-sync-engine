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
import com.waz.api.impl.search.UsersSearch.EmptyResult
import com.waz.service.SearchKey
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.Locales._

import scala.collection._

class ConnectionsSearch(query: SearchKey, filter: Array[String])(implicit val ui: UiModule)extends api.UsersSearchResult with CoreList[api.User] with SignalLoading {
  private val filteredIds = filter.toSet

  private var users = EmptyResult

  addLoader(_.users.acceptedUsers.map {
      _.values.filter(u => query.isAtTheStartOfAnyWordIn(u.searchKey)).filterNot(u => filteredIds.contains(u.id.str)).toVector.sortBy(_.name)(currentLocaleOrdering)
  }, EmptyResult) { conns =>
    users = conns
    notifyChanged()
  }

  override def get(position: Int): api.User = ui.users.getUser(users(position))
  override def size(): Int = users.length
  override def getAll: Array[api.User] = users.map(ui.users.getUser)(breakOut)
  override def getUnconnected: Array[api.User] = Array.empty
  override def getContacts: Array[api.User] = getAll
}
