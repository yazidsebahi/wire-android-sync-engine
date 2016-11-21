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

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.User.ConnectionStatus
import com.waz.model.SearchQuery.{Recommended, TopPeople}
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.service.UserSearchService.MinCommonConnections
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.testutils.Implicits.SignalToSink
import com.waz.testutils.Matchers._
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.Threading
import com.waz.utils._
import org.scalatest._
import org.threeten.bp.Instant
import org.threeten.bp.Instant.{EPOCH, now}

import scala.collection.breakOut
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.successful
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UserSearchServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with GivenWhenThen with OptionValues with RobolectricTests with RobolectricUtils {

  implicit def db: SQLiteDatabase = zms.storage.db.dbHelper.getWritableDatabase

  lazy val users = Seq(
    UserData(id('a), "other user 1"),
    UserData(id('b), "other user 2"),
    UserData(id('c), "some name"),
    UserData(id('d), "related user 1").copy(relation = Relation.Second),
    UserData(id('e), "related user 2").copy(relation = Relation.Second),
    UserData(id('f), "other related").copy(relation = Relation.Third),
    UserData(id('g), "friend user 1").copy(connection = ConnectionStatus.ACCEPTED),
    UserData(id('h), "friend user 2").copy(connection = ConnectionStatus.ACCEPTED),
    UserData(id('i), "some other friend").copy(connection = ConnectionStatus.ACCEPTED),
    UserData(id('j), "meep moop").copy(email = Some(EmailAddress("moop@meep.me"))))

  @volatile var syncRequest = Option.empty[SearchQuery]
  @volatile var commonSyncRequest = Option.empty[UserId]
  @volatile var onSync: SearchQuery => Future[Unit] = { _ => successful(()) }
  case object Meep

  lazy val zms = new MockZMessaging() {
    override lazy val sync: SyncServiceHandle = new EmptySyncService {
      override def syncSearchQuery(query: SearchQuery) = Serialized.future(Meep) {
        syncRequest = Some(query)
        onSync(query).flatMap(_ => super.syncSearchQuery(query))(Threading.Background)
      }

      override def syncCommonConnections(id: UserId): Future[SyncId] = {
        commonSyncRequest = Some(id)
        super.syncCommonConnections(id)
      }
    }
  }

  def service = zms.userSearch
  def id(s: Symbol) = UserId(s.toString)
  def ids(s: Symbol*) = s.map(id)(breakOut): Vector[UserId]

  before {
    Await.result(Future.traverse(users)(zms.usersStorage.addOrOverwrite), 5.seconds)
  }

  after {
    onSync = { _ => successful(()) }
    syncRequest = None
    commonSyncRequest = None

    Await.result(Serialized.future(Meep)(successful(())), 5.seconds)

    deleteCache()
  }

  def search(q: SearchQuery, limit: Option[Int] = None) = service.searchUserData(q, limit).map(_.keys)

  feature("Recommended people search") {
    scenario("Return local search results") {
      def verifySearch(prefix: String, matches: Seq[UserId]) = {
        val result = search(Recommended(prefix)).sink
        withClue(s"searching for: $prefix")(forAsLongAs(250.millis)(result.current.value should contain theSameElementsAs(matches)).soon)
      }

      verifySearch("r", ids('d, 'e, 'f))
      verifySearch("re", ids('d, 'e, 'f))
      verifySearch("rel", ids('d, 'e, 'f))
      verifySearch("relt", Nil)
      verifySearch("u", ids('d, 'e))
      verifySearch("us", ids('d, 'e))
      verifySearch("use", ids('d, 'e))
      verifySearch("user", ids('d, 'e))
      verifySearch("use", ids('d, 'e))
      verifySearch("used", Nil)
      verifySearch("meep", Nil)
      verifySearch("moop@meep", Nil)
      verifySearch("moop@meep.me", ids('j))
    }

    scenario("Schedule sync if no cached query is found") {
      val result = search(Recommended("rel")).sink
      forAsLongAs(250.millis)(result.current.value should have size 3).soon
      syncRequest.value shouldEqual Recommended("rel")
    }

    scenario("Do not schedule another sync if cache is found") {
      val result = search(Recommended("rel"), Some(2)).sink
      forAsLongAs(250.millis)(result.current.value should have size 2).soon
      syncRequest.value shouldEqual Recommended("rel")
      syncRequest = None

      val result2 = search(Recommended("rel"), Some(3)).sink
      forAsLongAs(250.millis)(result2.current.value should have size 3).soon
      syncRequest shouldBe None
    }

    scenario("Return backend result if it isn't empty") {
      onSync = backendResults(users.filter(_.name contains "user").filterNot(_.id == id('e)))

      val result = search(Recommended("user")).sink
      forAsLongAs(250.millis)(result.current.value should contain theSameElementsAs(ids('a, 'b, 'd))).soon
      syncRequest shouldBe defined
    }

    scenario("Return local result if backend result is empty") {
      onSync = backendResults(Nil)

      val result = search(Recommended("user")).sink
      forAsLongAs(250.millis)(result.current.value should contain theSameElementsAs(ids('d, 'e))).soon
      syncRequest shouldBe defined
    }

    scenario("Refresh when cached results are expired") {
      zms.searchQueryCache.insert(SearchQueryCache(SearchQuery.Recommended("user"), EPOCH, Some(ids('a, 'b, 'd, 'e))))

      val result = search(Recommended("user")).sink
      forAsLongAs(250.millis)(result.current.value should contain theSameElementsAs(ids('d, 'e))).soon
      syncRequest shouldBe defined
    }

    scenario("DB search for email") {
      val user = UserData(id('k), "quiet mouse").copy(email = Some(EmailAddress("sneaky@stealth.org")))
      zms.db.withTransaction(UserDataDao.insertOrReplace(user)(_))

      val result = search(Recommended("sneaky@stealth.org")).sink
      forAsLongAs(250.millis)(result.current.value should contain theSameElementsAs(ids('k))).soon
      syncRequest shouldBe defined
    }
  }

  feature("Top people search") {
    scenario("Return only connected users on top people search") {
      val result = search(TopPeople).sink

      within(1.second) {
        result.current.value should contain theSameElementsAs(ids('g, 'h, 'i))
      }
    }
  }

  feature("Common connections") {
    lazy val userId = users.head.id

    scenario("Request sync when common connections are requested") {
      val result = service.commonConnections(userId, fullList = false).sink
      forAsLongAs(250.millis)(result.current.value shouldBe None).soon
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Request sync when local common connections are expired") {
      val data = zms.commonConnections.insert(CommonConnectionsData(userId, 3, Seq.fill(3)(UserId()), now - zms.timeouts.search.cacheRefreshInterval - 1.milli)).await()

      val result = service.commonConnections(userId, fullList = false).sink
      forAsLongAs(250.millis)(result.current.value.value shouldEqual data).soon
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Request sync when not enough common connections is cached") {
      val data = zms.commonConnections.insert(CommonConnectionsData(userId, 10, Seq.fill(3)(UserId()), now)).await()

      val result = service.commonConnections(userId, fullList = false).sink
      forAsLongAs(250.millis)(result.current.value.value shouldEqual data).soon
      commonSyncRequest shouldEqual Some(userId)
    }

    scenario("Don't request sync when requesting top common connections are already loaded") {
      val data = zms.commonConnections.insert(CommonConnectionsData(userId, 3, Seq.fill(3)(UserId()), now)).await()
      val result = service.commonConnections(userId, fullList = false).sink
      forAsLongAs(250.millis)(result.current.value.value shouldEqual data).soon
      commonSyncRequest shouldEqual None

      val data1 = zms.commonConnections.insert(CommonConnectionsData(userId, 10, Seq.fill(MinCommonConnections)(UserId()), now)).await()
      val result1 = service.commonConnections(userId, fullList = false).sink
      forAsLongAs(250.millis)(result1.current.value.value shouldEqual data1).soon
      commonSyncRequest shouldEqual None

      val data2 = zms.commonConnections.insert(CommonConnectionsData(userId, 10, Seq.fill(10)(UserId()), now)).await()
      val result2 = service.commonConnections(userId, fullList = true).sink
      forAsLongAs(250.millis)(result2.current.value.value shouldEqual data2).soon
      commonSyncRequest shouldEqual None
    }
  }

  def backendResults(results: Seq[UserData]): SearchQuery => Future[Unit] = { query =>
    service.updateSearchResults(query, results.map { r =>
      UserSearchEntry(r.id, r.name, r.email, r.phone, r.accent, Some(r.connection == ConnectionStatus.ACCEPTED), r.connection == ConnectionStatus.BLOCKED, r.relation, handle = r.handle)
    })
  }

  def deleteCache() = Await.result(zms.searchQueryCache.deleteBefore(Instant.now + 1.day), 5.seconds)
}
