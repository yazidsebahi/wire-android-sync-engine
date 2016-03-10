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
package com.waz.api.impl.search

import com.waz.api
import com.waz.api.impl.SearchQuery.{DbQuery, RecommendedPeople, TopPeople}
import com.waz.api.impl.{OnlyContactsBySearchKeyFiltering, Contacts, SyncIndicator}
import com.waz.content.Uris
import com.waz.content.Uris.SyncIndicatorUri
import com.waz.model.sync.SyncCommand
import com.waz.service.SearchKey
import com.waz.ui.UiModule

class Search(implicit ui: UiModule) extends api.Search {

  override def getUsers(query: String, limit: Int, filter: Array[String]) =
    new UsersSearch(DbQuery(query.trim), limit, filter.toSet)

  override def getTopPeople(limit: Int, filter: Array[String]) =
    new UsersSearch(TopPeople, limit, filter.toSet)

  override def getRecommendedPeople(limit: Int, filter: Array[String]) =
    new UsersSearch(RecommendedPeople, limit, filter.toSet)

  override def getGroupConversations(query: String, limit: Int) =
    new ConversationsSearch(query.trim, limit)

  override def getContacts(query: String): api.Contacts =
    new Contacts(OnlyContactsBySearchKeyFiltering(SearchKey(query)))

  override def getSyncIndicator: api.SyncIndicator =
    ui.cached(SyncIndicatorUri(Uris.UserSearchUri), new SyncIndicator(SyncCommand.SyncSearchQuery))
}
