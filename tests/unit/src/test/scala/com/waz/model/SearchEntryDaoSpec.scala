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
import org.scalatest.{RobolectricTests, BeforeAndAfter, Matchers, FeatureSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.waz.db.ZMessagingDB
import org.robolectric.Robolectric
import com.waz.model.UserData.UserDataDao
import com.waz.model.SearchEntry.SearchEntryDao
import com.waz.model.SearchQueryCache.SearchQueryCacheDao

class SearchEntryDaoSpec extends FeatureSpec with Matchers with BeforeAndAfter with GeneratorDrivenPropertyChecks with RobolectricTests {

  lazy val dbHelper = new ZMessagingDB(Robolectric.application, "dbName")

  lazy val users = Seq(UserData("other user 1"), UserData("other user 2"), UserData("some name"),
    UserData("related user 1"),
    UserData("related user 2"),
    UserData("other related"),
    UserData("friend user 1"),
    UserData("friend user 2"),
    UserData("some other friend")
  )

  import SearchEntryDao._
  implicit def db = dbHelper.getWritableDatabase

  var cache: SearchQueryCache = _

  before {
    cache = SearchQueryCacheDao.add(SearchQuery.Named("query"))
    UserDataDao.insertOrReplace(users)
  }

  after {
    dbHelper.close()
  }


  scenario("save and load single entry") {
    val user = users(0)
    val entry = insertOrReplace(SearchEntry(cache.id, user.id, 0, 0))

    single(find(SearchEntryDao.QueryId, cache.id)) shouldEqual Some(entry)
  }

  scenario("find entries for query") {
    insertOrReplace(users.map(u => SearchEntry(cache.id, u.id, 0, 0)))

    findEntries(cache).map(_.userId).toSet shouldEqual users.map(_.id).toSet
  }

  scenario("find users for query") {
    insertOrReplace(users.map(u => SearchEntry(cache.id, u.id, 0, 0)))

    usersForQuery(cache).toSet shouldEqual users.map(_.id).toSet
  }

  scenario("preserve user result ordering") {
    insertOrReplace(users.sortBy(_.name).zipWithIndex.map { case (u, n) => SearchEntry(cache.id, u.id, 0, n) })

    usersForQuery(cache) shouldEqual users.sortBy(_.name).map(_.id)
  }
}
