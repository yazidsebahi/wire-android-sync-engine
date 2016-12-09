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
import com.waz.api.User.ConnectionStatus.BLOCKED
import com.waz.model.{ConvId, UserData, UserId}
import com.waz.service.{SearchKey, ZMessaging}
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.collection._

class ConnectionsSearch(searchTerm: String, limit: Int, filter: Array[String], alsoSearchByEmail: Boolean, showBlockedUsers: Boolean)(implicit val ui: UiModule) extends api.UserSearchResult with CoreList[api.User] with SignalLoading {
  private val filteredIds = filter.toSet
  private val query = SearchKey(searchTerm)
  private val predicate: UserData => Boolean = u =>
    (query.isAtTheStartOfAnyWordIn(u.searchKey) || u.handle.exists(_.containsQuery(searchTerm)) || (alsoSearchByEmail && u.email.exists(e => searchTerm.trim.equalsIgnoreCase(e.str)))) && ! filteredIds.contains(u.id.str) && (showBlockedUsers || (u.connection != BLOCKED))

  private var users = Option.empty[Vector[UserData]]

  addLoader { zms =>
    usersFrom(zms).map(_.sortBy(_.name)(currentLocaleOrdering).take(limit))
  } { conns =>
    val changed = users.forall(u => ! u.corresponds(conns)((a, b) => a.id == b.id))
    users = Some(conns)
    if (changed) notifyChanged()
  }

  private def usersFrom(zms: ZMessaging): Signal[Vector[UserData]] =
    zms.users.acceptedOrBlockedUsers.map(_.valuesIterator.filter(predicate).toVector)

  private def lastEventTimes(zms: ZMessaging, ids: Set[ConvId]): Signal[Map[UserId, Instant]] =
    zms.convsStorage.convsSignal.map { convs =>
      convs.conversations.iterator
        .filter(c => ids.contains(c.id))
        .map(c => UserId(c.id.str) -> c.lastEventTime)
        .toMap
    }

  private[this] def currentUsers = users.getOrElse(Vector.empty)

  override def get(position: Int): api.User = ui.users.getUser(currentUsers(position))
  override def size(): Int = currentUsers.length
  override def getAll: Array[api.User] = currentUsers.map(ui.users.getUser)(breakOut)
}
