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
import com.waz.api.SearchResultOrdering
import com.waz.model.{ConvId, UserData, UserId}
import com.waz.service.{SearchKey, ZMessaging}
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.collection._

class ConnectionsSearch(query: SearchKey, filter: Array[String], order: SearchResultOrdering)(implicit val ui: UiModule) extends api.UserSearchResult with CoreList[api.User] with SignalLoading {
  private val filteredIds = filter.toSet

  private var users = Vector.empty[UserData]

  addLoader { zms =>
    order match {
      case SearchResultOrdering.BY_NAME =>
        usersFrom(zms).map(_.sortBy(_.name)(currentLocaleOrdering))
      case SearchResultOrdering.BY_LAST_EVENT_TIME =>
        for {
          users <- usersFrom(zms)
          times <- lastEventTimes(zms, users.map(u => ConvId(u.id.str))(breakOut))
          order  = Ordering.by[UserData, Instant](u => times.getOrElse(u.id, Instant.EPOCH)).reverse
        } yield users.sorted(order)
    }
  } { conns =>
    users = conns
    notifyChanged()
  }

  private def usersFrom(zms: ZMessaging): Signal[Vector[UserData]] =
    zms.users.acceptedUsers
       .map(_.values.filter(u => query.isAtTheStartOfAnyWordIn(u.searchKey))
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
}
