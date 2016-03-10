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
import com.waz.api.impl.search.{ConversationsSearch, UsersSearch}
import com.waz.api.{IConversation, UpdateListener}
import com.waz.content.Uris
import com.waz.content.Uris.SyncIndicatorUri
import com.waz.model.sync.SyncCommand
import com.waz.threading.Threading
import com.waz.ui.UiModule
import com.waz.utils.returning

class SearchQuery(implicit ui: UiModule) extends com.waz.api.SearchQuery with UiObservable {
  import com.waz.api.impl.SearchQuery._

  private implicit val logTag: LogTag = logTagFor[SearchQuery]

  private val listener = new UpdateListener {
    override def updated(): Unit = notifyChanged()
  }
  private val users = returning(new UsersSearch)(_.addUpdateListener(listener))
  private lazy val conversations = returning(new ConversationsSearch)(_.addUpdateListener(listener))

  private def setQuery(query: DbQuery, limit: Int, filter: Array[String]): Unit = {
    Threading.assertUiThread()

    debug(s"queryParams changed ($query, $limit, ${filter.toSeq}), will restart loader")

    users.query(query, limit, filter.toSet)

    query match {
      case Named(prefix) => conversations.query(prefix, limit)
      case _ =>
    }
    // XXX: this is an ugly hack to make SearchQuery behave closer to what UI expects (call notify at least once on every setQuery)
    notifyChanged()
  }

  override def getUsers = returning(users.getAll)(users => verbose(s"getUsers, count: ${users.length}"))

  override def getContacts = returning(users.getContacts)(users => verbose(s"getContacts, count: ${users.length}"))

  override def getRelated = Array.empty

  override def getOther = users.getUnconnected

  override def getConversations: Array[IConversation] = conversations.getAll

  override def setQuery(query: String, limit: Int, filter: Array[String]): Unit = setQuery(DbQuery(query), limit, filter)

  override def setQuery(query: String, limit: Int): Unit = setQuery(query, limit, EmptyFilter)

  override def setTopPeopleQuery(limit: Int): Unit = setTopPeopleQuery(limit, EmptyFilter)

  override def setTopPeopleQuery(limit: Int, filter: Array[String]): Unit = setQuery(TopPeople, limit, filter)

  override def setRecommendedPeopleQuery(limit: Int): Unit = setRecommendedPeopleQuery(limit, EmptyFilter)

  override def setRecommendedPeopleQuery(limit: Int, filter: Array[String]): Unit = setQuery(RecommendedPeople, limit, filter)

  override def getSyncIndicator =
    ui.cached(SyncIndicatorUri(Uris.UserSearchUri), new SyncIndicator(SyncCommand.SyncSearchQuery))
}

object SearchQuery {
  val EmptyFilter = Array[String]()

  def fromCacheKey(key: String): Query =
    if (key.startsWith(s"##${TopPeople.name}##")) TopPeople
    else if (key.startsWith(s"##${RecommendedPeople.name}##")) RecommendedPeople
    else if (key.startsWith(s"##${AddressBook.name}##")) AddressBook
    else if (key.startsWith("##named##")) Named(key.substring(9))
    else throw new IllegalStateException(s"unknown or invalid cache key: $key")

  sealed trait Query {
    def name: String
    def cacheKey: String = s"##$name##"
  }

  case object AddressBook extends Query { val name = "addressbook" }
  sealed trait DbQuery extends Query
  object DbQuery {
    def apply(prefix: String): DbQuery = if (prefix.trim.isEmpty) TopPeople else Named(prefix.trim)
  }

  case object TopPeople extends DbQuery { val name = "top" }
  case object RecommendedPeople extends DbQuery { val name = "recommended" }

  case class Named(prefix: String) extends DbQuery {
    val name = "named"
    override val cacheKey = super.cacheKey + prefix
  }
}
