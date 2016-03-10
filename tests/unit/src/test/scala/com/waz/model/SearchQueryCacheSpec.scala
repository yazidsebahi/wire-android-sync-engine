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
package com.waz.model

import com.waz.api.impl.SearchQuery
import com.waz.model.UserData.UserDataDao
import org.scalatest._
import com.waz.db.ZMessagingDB
import org.robolectric.Robolectric
import SearchQueryCache.SearchQueryCacheDao
import SearchEntry.SearchEntryDao
import com.waz.service.SearchKey

class SearchQueryCacheSpec extends FeatureSpec with GivenWhenThen with BeforeAndAfter with Matchers with RobolectricTests {

  lazy val dbHelper = new ZMessagingDB(Robolectric.application, "dbName")
  implicit def db = dbHelper.getWritableDatabase

  lazy val users = List(
    UserData(UserId(), "Foo", None, None, searchKey = SearchKey("Foo")),
    UserData(UserId(), "Bar", None, None, searchKey = SearchKey("Bar")),
    UserData(UserId(), "Goo", None, None, searchKey = SearchKey("Goo")),
    UserData(UserId(), "Boo", None, None, searchKey = SearchKey("Boo"))
  )

  def searchEntriesForId(id: Long) = List(
    SearchEntry(id, users(0).id, 1, 2),
    SearchEntry(id, users(1).id, 1, 4),
    SearchEntry(id, users(2).id, 1, 1),
    SearchEntry(id, users(3).id, 1, 3)
  )

  def foo = SearchQuery.Named("foo")

  before {
    UserDataDao.insertOrReplace(users)
  }

  after {
    dbHelper.close()
  }

  feature("SearchQueryCache") {
    scenario("add query with queryLimit to cache and read it") {
      val entry = SearchQueryCacheDao.add(foo, Some(22))
      SearchQueryCacheDao.get(foo, Some(22)) should be(Some(entry))
    }

    scenario("add query without queryLimit to cache and read it") {
      val entry = SearchQueryCacheDao.add(SearchQuery.Named("bar"))
      SearchQueryCacheDao.get(SearchQuery.Named("bar")) should be(Some(entry))
    }

    scenario("multiple queries in cache get query without queryLimit") {
      SearchQueryCacheDao.add(foo, Some(11))
      SearchQueryCacheDao.add(foo, Some(15))

      val entry = SearchQueryCacheDao.add(foo)
      SearchQueryCacheDao.get(foo) should be(Some(entry))
    }

    scenario("multiple queries in cache get query with queryLimit"){

      Given("some entries with different queryLimit and timestamp")
      val entry_01 = SearchQueryCache(1, foo, Some(15), 1000)
      val entry_02 = SearchQueryCache(2, foo, Some(30), 2000)
      val entry_03 = SearchQueryCache(3, foo, Some(10), 3000)

      SearchQueryCacheDao.insertOrReplace(entry_01)
      SearchQueryCacheDao.insertOrReplace(entry_02)
      SearchQueryCacheDao.insertOrReplace(entry_03)

      When("get entry with queryLimit")
      val result = SearchQueryCacheDao.get(foo, Some(11))

      Then("the result should be the newest entry witch has a fitting queryLimit")
      result should be(Some(entry_02))
    }
  }

  feature("SearchEntries") {
    scenario("get entries for searchQuery") {

      Given("a SearchQueryCache and some entries")
      val queryCache = SearchQueryCacheDao.add(foo, Some(22))

      val searchEntries_1 = searchEntriesForId(queryCache.id)
      val searchEntries_2 = searchEntriesForId(120)

      SearchEntryDao.insertOrReplace(searchEntries_1)
      SearchEntryDao.insertOrReplace(searchEntries_2)

      When("findEntries for SearchQueryCache")
      val result = SearchEntryDao.findEntries(queryCache)

      Then("the result should be a list of entries ordered by the order field")
      result should be(searchEntries_1.sortBy(_.order))
    }

    scenario("get entries for searchQuery with queryLimit") {

      Given("a SearchQueryCache and some entries")
      val queryCache = SearchQueryCacheDao.add(foo, Some(22))

      val searchEntries_1 = searchEntriesForId(queryCache.id)
      val searchEntries_2 = searchEntriesForId(120)

      SearchEntryDao.insertOrReplace(searchEntries_1)
      SearchEntryDao.insertOrReplace(searchEntries_2)

      When("findEntries for SearchQueryCache with queryLimit")
      val result = SearchEntryDao.findEntries(queryCache, limit = Some(2))

      Then("the result should be a limited list of entries ordered by the order field")
      result should be(searchEntries_1.sortBy(_.order).take(2))
    }


    scenario("delete entries for searchQuery") {

      Given("a SearchQueryCache and some entries")
      val queryCache = SearchQueryCacheDao.add(foo, Some(22))
      SearchEntryDao.insertOrReplace(searchEntriesForId(queryCache.id))

      When("deleteEntries for SearchQueryCache")
      SearchEntryDao.deleteEntries(queryCache)

      Then("the the result should be the empty List")
      SearchEntryDao.findEntries(queryCache) should be(List.empty)
    }

    scenario("get Users for searchQuery") {

      Given("a searchQueryCache and some entries")
      val queryCache = SearchQueryCacheDao.add(foo, Some(22))
      val searchEntries  = searchEntriesForId(queryCache.id)
      SearchEntryDao.insertOrReplace(searchEntries)

      When("userForQuery for searchQueryCache is called")
      val result = SearchEntryDao.usersForQuery(queryCache)

      Then("the result should be a List of UserData ordered like the searchEntry order")
      val orderedUsers = searchEntries.sortBy(_.order).map(entry => users.find(_.id == entry.userId).get)
      result should be(orderedUsers.map(_.id))
    }

    scenario("get Users for searchQuery with queryLimit") {

      Given("a searchQueryCache and some entries")
      val queryCache = SearchQueryCacheDao.add(foo, Some(22))
      val searchEntries  = searchEntriesForId(queryCache.id)
      SearchEntryDao.insertOrReplace(searchEntries)

      When("userForQuery for searchQueryCache and queryLimit is called")
      val result = SearchEntryDao.usersForQuery(queryCache, Some(2))

      Then("the result should be a limited List of Users ordered like the searchEntry order")
      val orderedUsers = searchEntries.sortBy(_.order).map(entry => users.find(_.id == entry.userId).get)
      result should be(orderedUsers.take(2).map(_.id))
    }
  }
}
