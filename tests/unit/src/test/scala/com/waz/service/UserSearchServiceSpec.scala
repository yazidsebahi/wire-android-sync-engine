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
package com.waz.service

import java.util.concurrent.TimeoutException

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.User.ConnectionStatus
import com.waz.api.impl.SearchQuery
import com.waz.model.CommonConnectionsData.CommonConnectionsDataDao
import com.waz.model.SearchEntry.SearchEntryDao
import com.waz.model.SearchQueryCache.SearchQueryCacheDao
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.sync.SyncServiceHandle
import com.waz.testutils.Matchers._
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.{CancellableFuture, Threading}
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UserSearchServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with GivenWhenThen with RobolectricTests with RobolectricUtils {

  implicit def db: SQLiteDatabase = zms.storage.dbHelper.getWritableDatabase
  def context = testContext

  lazy val users = Seq(UserData("other user 1"), UserData("other user 2"), UserData("some name"),
    UserData("related user 1").copy(relation = Relation.Second),
    UserData("related user 2").copy(relation = Relation.Second),
    UserData("other related").copy(relation = Relation.Third),
    UserData("friend user 1").copy(connection = ConnectionStatus.ACCEPTED),
    UserData("friend user 2").copy(connection = ConnectionStatus.ACCEPTED),
    UserData("some other friend").copy(connection = ConnectionStatus.ACCEPTED)
  )

  var syncRequest = None: Option[SearchQueryCache]
  var commonSyncRequest = Option.empty[UserId]
  var onSync: SearchQueryCache => CancellableFuture[Boolean] = _

  lazy val zms = new MockZMessaging() {
    override lazy val sync: SyncServiceHandle = new EmptySyncService {
      override def syncSearchQuery(cache: SearchQueryCache) = {
        syncRequest = Some(cache)
        onSync(cache)
        super.syncSearchQuery(cache)
      }

      override def syncCommonConnections(id: UserId): Future[SyncId] = {
        commonSyncRequest = Some(id)
        super.syncCommonConnections(id)
      }
    }
  }
  
  def service = zms.usersearch

  def query(q: String = "friend") = SearchQuery.Named(q)

  before {
    onSync = { _ => CancellableFuture.successful(true)}
    syncRequest = None
    commonSyncRequest = None

    Await.result(Future.sequence(users map zms.usersStorage.addOrOverwrite), 5.seconds)
    deleteCache()
  }

  scenario("Don't return local results too soon, should have a delay") {
    intercept[TimeoutException] {
      Await.result(service.searchUsers(query()), zms.timeouts.userSearch.localSearchDelay - 50.millis)
    }
  }

  scenario("Return local search results after a delay") {
    val res = Await.result(service.searchUsers(query()), zms.timeouts.userSearch.localSearchDelay + 500.millis)

    res.toSet shouldEqual users.filter(_.name.contains("friend")).map(_.id).toSet
  }

  scenario("Schedule sync if no cached query is found") {
    Await.result(service.searchUsers(query()), 10.seconds)

    syncRequest match {
      case Some(SearchQueryCache(_, SearchQuery.Named("friend"), _, _)) => // expected
      case _ => fail(s"Expected sync request, got: $syncRequest")
    }
  }

  scenario("Schedule sync with limit if no cached query is found") {
    Await.result(service.searchUsers(query("frien"), 10), 10.seconds)

    syncRequest match {
      case Some(SearchQueryCache(_, SearchQuery.Named("frien"), Some(10), _)) => // expected
      case _ => fail(s"Expected sync request, got: $syncRequest")
    }
  }

  feature("Top people search") {
    scenario("Return only connected users on top people search") {
      val topPeople = Await.result(service.searchUsers(SearchQuery.TopPeople, 10), 10.seconds)

      topPeople should not be empty
      topPeople.map(id => users.find(_.id == id).get) foreach (_.connection shouldEqual ConnectionStatus.ACCEPTED)
    }
  }

  feature("Recommended people search") {
    scenario("Recommended people search returns the latest cached result if current") {
      addCacheEntry(SearchQuery.RecommendedPeople, users.drop(3).take(3), System.currentTimeMillis())

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 3).matcher[Seq[UserId]])(10.seconds)
      syncRequest shouldEqual None
    }

    scenario("Recommended people search doesn't return excluded users") {
      addCacheEntry(SearchQuery.RecommendedPeople, users.drop(3).take(3), System.currentTimeMillis())
      Await.result(service.excludeFromPymk(users(4).id), 5.seconds)

      info(s"users: ${users.map(_.id)}")

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 2).matcher[Seq[UserId]])(10.seconds)
      syncRequest shouldEqual None
    }

    scenario("Recommended people search returns the latest cached result if current and schedules sync if not fresh") {
      addCacheEntry(SearchQuery.RecommendedPeople, users.drop(3).take(3), System.currentTimeMillis() - zms.timeouts.userSearch.cacheRefreshInterval.toMillis - 100L)

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 3).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("Recommended people search returns backend result if it isn't empty") {
      onSync = backendResults(users.drop(3).take(3))

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 3).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("Recommended people search doesn't return excluded users even when returned by backend") {
      Await.result(service.excludeFromPymk(users(4).id), 5.seconds)
      onSync = backendResults(users.drop(3).take(3))

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 2).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("Recommended people search returns address book matches if backend result for recommended people is empty") {
      onSync = backendResults(Seq())
      addCacheEntry(SearchQuery.AddressBook, users.drop(2), System.currentTimeMillis())

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 4).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("Recommended people search returns address book matches as long as backend doesn't sync") {
      addCacheEntry(SearchQuery.AddressBook, users.drop(2), System.currentTimeMillis())

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 4).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("Recommended people search returns address book matches if the latest backend results are expired") {
      addCacheEntry(SearchQuery.AddressBook, users.drop(2), System.currentTimeMillis())
      addCacheEntry(SearchQuery.RecommendedPeople, users, 0L)

      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 4).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }

    scenario("For no recommended people results and no address book matches, we fall back to local database") {
      val recommended = service.searchUsers(SearchQuery.RecommendedPeople)
      recommended should eventually((have size 3).matcher[Seq[UserId]])(10.seconds)
      syncRequest should be (defined)
    }
  }

  feature("Common connections") {
    lazy val userId = users.head.id

    scenario("Request sync when common connections are requested") {
      service.getCommonConnections(userId, fullList = false) should eventually(be(None))
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Request sync when local common connections are expired") {
      val data = CommonConnectionsDataDao.insertOrReplace(CommonConnectionsData(userId, 3, Seq.fill(3)(UserId()), System.currentTimeMillis() - zms.timeouts.userSearch.cacheRefreshInterval.toMillis - 1))

      service.getCommonConnections(userId, fullList = false) should eventually(be(Some(data)))
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Request sync when not enough common connections is cached") {
      val data = CommonConnectionsDataDao.insertOrReplace(CommonConnectionsData(userId, 10, Seq.fill(3)(UserId()), System.currentTimeMillis()))

      service.getCommonConnections(userId, fullList = false) should eventually(be(Some(data)))
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Don't request sync when requesting top common connections are already loaded") {
      val data = CommonConnectionsDataDao.insertOrReplace(CommonConnectionsData(userId, 3, Seq.fill(3)(UserId()), System.currentTimeMillis()))
      service.getCommonConnections(userId, fullList = false) should eventually(be(Some(data)))
      commonSyncRequest shouldEqual None

      val data1 = CommonConnectionsDataDao.insertOrReplace(CommonConnectionsData(userId, 10, Seq.fill(UserSearchService.MinCommonConnections)(UserId()), System.currentTimeMillis()))
      service.getCommonConnections(userId, fullList = false) should eventually(be(Some(data1)))
      commonSyncRequest shouldEqual None

      val data2 = CommonConnectionsDataDao.insertOrReplace(CommonConnectionsData(userId, 10, Seq.fill(10)(UserId()), System.currentTimeMillis()))

      service.getCommonConnections(userId, fullList = true) should eventually(be(Some(data2)))
      commonSyncRequest shouldEqual None
    }
  }

  scenario("Return fresh data from backend immediately after the sync is completed") {
    Given("fast backend sync")
    onSync = backendResults(users.take(2))

    When("searching for users")
    val res = Await.result(service.searchUsers(query()), 500.millis)

    Then("sync request is sent")
    syncRequest.isDefined shouldEqual true

    And("freshly synced results are returned almost immediately")
    res.toSet shouldEqual users.take(2).map(_.id).toSet
  }

  scenario("Load fresh results from cache") {
    Given("fresh query cache")
    addCacheEntry(query(), users.take(2), System.currentTimeMillis())

    When("searching for users")
    val res = Await.result(service.searchUsers(query()), 200.millis)

    Then("sync is not scheduled")
    syncRequest shouldEqual None

    And("cached result is returned")
    res.toSet shouldEqual users.take(2).map(_.id).toSet
  }

  scenario("Load fresh results from cache with limit") {
    Given("fresh query cache with a query limit")
    addCacheEntry(query(), users.take(1), System.currentTimeMillis(), limit = Some(1))

    When("searching for users")
    val res = Await.result(service.searchUsers(query(), limit = 1), 200.millis)

    Then("sync is not scheduled")
    syncRequest shouldEqual None

    And("cached result is returned")
    res.toSet shouldEqual users.take(1).map(_.id).toSet
  }

  scenario("Load old cache result and schedule sync") {
    Given("old query cache result")
    val cache = addCacheEntry(query(), users.take(2), System.currentTimeMillis() - zms.timeouts.userSearch.cacheRefreshInterval.toMillis - 100)

    When("searching for users")
    val res = Await.result(service.searchUsers(query()), 200.millis)

    Then("sync is scheduled")
    syncRequest shouldEqual Some(cache)

    And("cached result is returned")
    res.toSet shouldEqual users.take(2).map(_.id).toSet
  }

  scenario("Discard very old cache results") {
    Given("expired query cache result")
    addCacheEntry(query(), users.take(2), System.currentTimeMillis() - SearchQueryCache.CacheExpiryTime.toMillis - 100)
    And("users in local db")
    Await.ready(zms.storage(UserDataDao.insertOrReplace(users)(_)), 15.seconds)

    When("searching for users")
    val res = Await.result(service.searchUsers(query()), zms.timeouts.userSearch.localSearchDelay + 500.millis)

    Then("sync is scheduled")
    syncRequest match {
      case Some(SearchQueryCache(_, friend, _, _)) => // expected
      case _ => fail(s"Expected sync request, got: $syncRequest")
    }

    And("expired result is ignored, returning local search result")
    res.toSet shouldEqual users.filter(_.name.contains("friend")).map(_.id).toSet
  }

  def backendResults(results: Seq[UserData]): SearchQueryCache => CancellableFuture[Boolean] = { cache =>
    Threading.Background {
      updateCacheResults(cache, results)
      service.onSearchResultChanged ! cache.query
      true
    }
  }

  def addCacheEntry(query: SearchQuery.Query, users: Seq[UserData], timestamp: Long = System.currentTimeMillis(), limit: Option[Int] = None): SearchQueryCache = {
    val cache = Await.result(zms.storage { implicit db => SearchQueryCacheDao.add(query, limit) }, 5.seconds)
    updateCacheResults(cache, users, timestamp)
  }

  def deleteCache() =
    Await.result(zms.storage { implicit db =>
      SearchQueryCacheDao.deleteAll
      SearchEntryDao.deleteAll
    }, 5.seconds)

  def updateCacheResults(cache: SearchQueryCache, users: Seq[UserData], timestamp: Long = System.currentTimeMillis()): SearchQueryCache = {
    Await.result(zms.storage { implicit db =>
      UserDataDao.insertOrReplace(users)
      SearchEntryDao.insertOrReplace(users.sortBy(_.name).zipWithIndex map { case (u, n) => SearchEntry(cache.id, u.id, 0, n) })
      SearchQueryCacheDao.update(cache)(_.copy(timestamp = timestamp)).get
    }, 15.seconds)
  }
}
