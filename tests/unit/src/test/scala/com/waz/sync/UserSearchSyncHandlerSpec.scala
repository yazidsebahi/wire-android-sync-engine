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
package com.waz.sync

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.impl.{ErrorResponse, SearchQuery}
import com.waz.model.SearchEntry.SearchEntryDao
import com.waz.model.SearchQueryCache.SearchQueryCacheDao
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.sync.client._
import com.waz.sync.handler.UsersSyncHandler
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.robolectric.Robolectric
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Try}

class UserSearchSyncHandlerSpec extends FeatureSpec with Matchers with BeforeAndAfter with GivenWhenThen with RobolectricTests with RobolectricUtils { test =>
  private lazy implicit val dispatcher = Threading.Background

  def storage = zms.storage

  lazy val users = Seq(UserData("other user 1"), UserData("other user 2"), UserData("some name"),
    UserData("related user 1"),
    UserData("related user 2"),
    UserData("other related"),
    UserData("friend user 1"),
    UserData("friend user 2"),
    UserData("some other friend")
  )

  var userSyncRequests = List[UserId]()
  var response: Try[Either[ErrorResponse, Seq[UserSearchEntry]]] = Success(Right(List[UserSearchEntry]()))
  var delay = 0.seconds

  var zms: MockZMessaging = _
  def searchSync = zms.usersearchSync
  def sync = zms.sync

  implicit def db: SQLiteDatabase = storage.dbHelper.getWritableDatabase

  def query = SearchQuery.Named("query")

  before {
    userSyncRequests = Nil

    zms = new MockZMessaging() {

      override lazy val sync = new EmptySyncService {
        override def syncSearchQuery(cache: SearchQueryCache) = {
          searchSync.syncSearchQuery(cache.id)
          super.syncSearchQuery(cache)
        }
        override def syncUsers(ids: UserId*) = {
          ids foreach { id => userSyncRequests ::= id }
          super.syncUsers(ids: _*)
        }
      }

      override lazy val userSearchClient: UserSearchClient = new UserSearchClient(znetClient) {
        override def graphSearch(query: SearchQuery.Query, limit: Int): ErrorOrResponse[Seq[UserSearchEntry]] = CancellableFuture.delay(delay) map { _ => response.get }
      }

      keyValue.lastSlowSyncTimestamp = System.currentTimeMillis()
      override lazy val usersSync: UsersSyncHandler = new UsersSyncHandler(assetSync, users, usersStorage, assetsStorage, usersClient) {
        override def syncUsers(ids: UserId*) = {
          sync.syncUsers(ids: _*)
          CancellableFuture.successful(SyncResult.Success)
        }
      }
    }
  }

  after {
    Await.result(storage.close(), 10.seconds)
    Robolectric.application.getDatabasePath(storage.dbHelper.getDatabaseName).delete()
  }

  scenario("Create new users from successful search result") {
    Given("empty local db and working search client")

    val entries = List(
      UserSearchEntry(UserId(), "user 1", Some(EmailAddress("email 1")), None, 1, connected = Some(true), blocked = true, Relation.First),
      UserSearchEntry(UserId(), "user 2", Some(EmailAddress("email 2")), None, 1, connected = Some(true), blocked = true, Relation.First)
    )
    response = Success(Right(entries))

    When("executing sync for new query")
    val cache = SearchQueryCacheDao.add(query)
    sync.syncSearchQuery(cache)

    Then("sync should create new users for received data")
    withDelay {
      UserDataDao.list.toSet map withoutDisplayName shouldEqual entries.map(UserData(_)).toSet
    }

    And("newly cached query should reference all these new users")
    SearchEntryDao.usersForQuery(cache) shouldEqual entries.map(_.id)
  }

  scenario("Create new users from succsessful search result and preserve the order") {
    Given("empty local db and working search client")

    val entries = List(
      UserSearchEntry(UserId(), "user 1", Some(EmailAddress("email 1")), None, 1, connected = Some(true), blocked = true, Relation.First),
      UserSearchEntry(UserId(), "user 2", Some(EmailAddress("email 2")), None, 1, connected = Some(true), blocked = true, Relation.First)
    )
    response = Success(Right(entries))

    When("executing sync for new query")
    val cache = SearchQueryCacheDao.add(query)
    sync.syncSearchQuery(cache)

    Then("cached query should reference all users in the same order as in the response")
    withDelay {
      SearchEntryDao.usersForQuery(cache) should be(entries.map(_.id))
    }
  }

  scenario("Update users data from search result") {

    val entries = List(
        UserSearchEntry(UserId(), "user 1", Some(EmailAddress("email 1")), None, 1, connected = Some(true), blocked = true, Relation.First),
        UserSearchEntry(UserId(), "user 2", Some(EmailAddress("email 2")), None, 1, connected = Some(true), blocked = true, Relation.First)
    )
    response = Success(Right(entries))

    Given("local db with some users")
    UserDataDao.insertOrReplace(entries.map(UserData(_).copy(name = "test")))

    When("executing sync returning these users")
    val cache = SearchQueryCacheDao.add(query)
    sync.syncSearchQuery(cache)

    Then("service updates existing users data")
    withDelay {
      UserDataDao.list.toSet map withoutDisplayName shouldEqual entries.map(UserData(_)).toSet
    }

    And("newly cached query should reference all users")
    SearchEntryDao.usersForQuery(cache) shouldEqual entries.map(_.id)
  }

  scenario("Schedule sync for newly created users") {
    val entries = List(
      UserSearchEntry(UserId(), "user 1", Some(EmailAddress("email 1")), None, 1, connected = Some(true), blocked = true, Relation.First),
      UserSearchEntry(UserId(), "user 2", Some(EmailAddress("email 2")), None, 1, connected = Some(true), blocked = true, Relation.First)
    )
    response = Success(Right(entries))

    When("executing sync for new query")
    sync.syncSearchQuery(SearchQueryCacheDao.add(query))

    Then("service schedules sync for all added users")
    withDelay {
      userSyncRequests.toSet shouldEqual entries.map(_.id).toSet
    }
  }

  scenario("Schedule sync for old users contained in query results") {
    val entries = List(
        UserSearchEntry(UserId(), "user 1", Some(EmailAddress("email 1")), None, 1, connected = Some(true), blocked = true, Relation.First),
        UserSearchEntry(UserId(), "user 2", Some(EmailAddress("email 2")), None, 1, connected = Some(true), blocked = true, Relation.First),
        UserSearchEntry(UserId(), "user 3", Some(EmailAddress("email 3")), None, 1, connected = Some(true), blocked = true, Relation.First)
    )
    response = Success(Right(entries))

    Given("two old users in db")
    UserDataDao.insertOrReplace(entries.take(2).map(UserData(_).copy(name = "", syncTimestamp = 1)))

    And("one fresh user")
    UserDataDao.insertOrReplace(entries.drop(2).map(UserData(_).copy(name = "", syncTimestamp = System.currentTimeMillis())))

    When("executing sync returning results for this existing users")
    sync.syncSearchQuery(SearchQueryCacheDao.add(query))

    Then("service schedules sync for old users")
    withDelay {
      userSyncRequests.toSet shouldEqual entries.take(2).map(_.id).toSet
    }
  }

  def withoutDisplayName(data: UserData): UserData = data.copy(displayName = "")
}
