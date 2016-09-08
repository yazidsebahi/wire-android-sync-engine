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
package com.waz.api

trait Search {
  def getTopPeople(limit: Int, filter: Array[String]): UserSearchResult
  def getRecommendedPeople(query: String, limit: Int, filter: Array[String]): UserSearchResult
  def getGroupConversations(query: String, limit: Int): ConversationSearchResult
  def getContacts(query: String): Contacts
  def getConnectionsByName(query: String, limit: Int, filter: Array[String]): UserSearchResult
  def getConnectionsByNameOrEmailIncludingBlocked(query: String, limit: Int, filter: Array[String]): UserSearchResult
}

trait UserSearchResult extends CoreList[User] {
  def getAll: Array[User]
}

trait ConversationSearchResult extends CoreList[IConversation] {
  def getAll: Array[IConversation]
}
