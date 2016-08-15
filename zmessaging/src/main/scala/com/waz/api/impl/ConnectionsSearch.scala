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
import com.waz.model.{ConvId, UserData, UserId}
import com.waz.service.{SearchKey, ZMessaging}
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.collection._

class ConnectionsSearch(query: SearchKey, filter: Array[String])(implicit val ui: UiModule) extends api.UserSearchResult with CoreList[api.User] with SignalLoading {
  private val filteredIds = filter.toSet

  private var users = Vector.empty[UserData]

  addLoader { zms =>
    usersFrom(zms).map(_.sortBy(_.name)(currentLocaleOrdering))
  } { conns =>
    val changed = ! users.corresponds(conns)((a, b) => a.id == b.id)
    users = conns
    if (changed) notifyChanged()
  }

  private def usersFrom(zms: ZMessaging): Signal[Vector[UserData]] =
    zms.users.acceptedUsers
       .map(_.valuesIterator
             .filter(u => query.isAtTheStartOfAnyWordIn(u.searchKey))
             .filterNot(u => filteredIds.contains(u.id.str))
             .toVector)

  private def lastEventTimes(zms: ZMessaging, ids: Set[ConvId]): Signal[Map[UserId, Instant]] =
    zms.convsStorage.convsSignal.map { convs =>
      convs.conversations.iterator
        .filter(c => ids.contains(c.id))
        .map(c => UserId(c.id.str) -> c.lastEventTime)
        .toMap
    }

  override def get(position: Int): api.User = ui.users.getUser(users(position))
  override def size(): Int = users.length
  override def getAll: Array[api.User] = users.map(ui.users.getUser)(breakOut)
  override def getFirstN(n: Int): Array[api.User] = users.iterator.take(n).map(ui.users.getUser).toArray
}
