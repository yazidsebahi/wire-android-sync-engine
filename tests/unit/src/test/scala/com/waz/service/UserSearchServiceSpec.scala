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

import com.waz.api.User.ConnectionStatus
import com.waz.content._
import com.waz.model.SearchQuery.Recommended
import com.waz.model.{UserId, _}
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.utils.events.{Signal, SourceSignal}

import scala.collection.breakOut
import scala.concurrent.Future
import com.waz.utils.Managed
import com.waz.utils.wrappers.DB
import org.threeten.bp.Instant
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.service.conversation.ConversationsUiService
import com.waz.service.teams.TeamsService
import scala.language.higherKinds

import scala.collection.generic.CanBuild

class UserSearchServiceSpec extends AndroidFreeSpec {

  lazy val users = Map(
    id('a) -> UserData(id('a), "other user 1"),
    id('b) -> UserData(id('b), "other user 2"),
    id('c) -> UserData(id('c), "some name"),
    id('d) -> UserData(id('d), "related user 1").copy(relation = Relation.Second),
    id('e) -> UserData(id('e), "related user 2").copy(relation = Relation.Second),
    id('f) -> UserData(id('f), "other related").copy(relation = Relation.Third),
    id('g) -> UserData(id('g), "friend user 1").copy(connection = ConnectionStatus.ACCEPTED),
    id('h) -> UserData(id('h), "friend user 2").copy(connection = ConnectionStatus.ACCEPTED),
    id('i) -> UserData(id('i), "some other friend").copy(connection = ConnectionStatus.ACCEPTED),
    id('j) -> UserData(id('j), "meep moop").copy(email = Some(EmailAddress("moop@meep.me")))
  )

  def id(s: Symbol) = UserId(s.toString)
  def ids(s: Symbol*) = s.map(id)(breakOut).toSet

  def stubService(userId: UserId = UserId(),
                  queryCacheStorage: SearchQueryCacheStorage = stub[SearchQueryCacheStorage],
                  teamId: Option[TeamId] = None,
                  userService: UserService = stub[UserService],
                  usersStorage: UsersStorage = stub[UsersStorage],
                  teamsService: TeamsService = stub[TeamsService],
                  membersStorage: MembersStorage = stub[MembersStorage],
                  timeouts: Timeouts = new Timeouts,
                  sync: SyncServiceHandle = stub[SyncServiceHandle],
                  messagesStorage: MessagesStorage = stub[MessagesStorage],
                  convsUi: ConversationsUiService = stub[ConversationsUiService]) =
    new UserSearchService(userId, queryCacheStorage, teamId, userService, usersStorage, teamsService, membersStorage, timeouts, sync, messagesStorage, convsUi)

  def verifySearch(prefix: String, matches: Set[UserId]) = {
    val query = Recommended(prefix)
    val expected = users.filterKeys(matches.contains).values.toVector
    val querySignal = new SourceSignal[Option[SearchQueryCache]]()
    val firstQueryCache = SearchQueryCache(query, Instant.now, None)
    val secondQueryCache = SearchQueryCache(query, Instant.now, Some(matches.toVector))


    val queryCacheStorage = mock[SearchQueryCacheStorage]
    (queryCacheStorage.deleteBefore _).expects(*).anyNumberOfTimes().returning(Future.successful[Unit]({}))
    val usersStorage = mock[UsersStorage]
    val sync = mock[SyncServiceHandle]

    (queryCacheStorage.optSignal _).expects(query).once().returning(querySignal)
    (usersStorage.find(_: UserData => Boolean, _: DB => Managed[TraversableOnce[UserData]], _: UserData => UserData)(_: CanBuild[UserData, Vector[UserData]]))
      .expects(*, *, *, *).once().returning(Future.successful(expected))

    if (expected.nonEmpty) {
      (queryCacheStorage.updateOrCreate _).expects(*, *, *).once().returning {
        Future.successful(secondQueryCache)
      }
      (sync.syncSearchQuery _).expects(query).once().onCall { _: SearchQuery =>
        Future.successful[SyncId] {
          querySignal ! Some(secondQueryCache)
          SyncId()
        }
      }
      (usersStorage.listSignal _).expects(matches.toVector).once().returning(Signal.const(expected))
    }

    val service = stubService(queryCacheStorage = queryCacheStorage, usersStorage = usersStorage, sync = sync)
    querySignal ! Some(firstQueryCache)
    result(service.searchUserData(Recommended(prefix)).map(_.keys.toSet).filter(_ == matches).head)
  }


  feature("Recommended people search") {
    scenario("Return search results") {
      verifySearch("r", ids('d, 'e, 'f))
      verifySearch("re", ids('d, 'e, 'f))
      verifySearch("rel", ids('d, 'e, 'f))
      verifySearch("u", ids('d, 'e))
      verifySearch("us", ids('d, 'e))
      verifySearch("use", ids('d, 'e))
      verifySearch("user", ids('d, 'e))
      verifySearch("use", ids('d, 'e))
      verifySearch("moop@meep.me", ids('j))

      /*verifySearch("relt", Set.empty[UserId])
      verifySearch("used", Set.empty[UserId])
      verifySearch("meep", Set.empty[UserId])
      verifySearch("moop@meep", Set.empty[UserId])*/
    }
  }

  // TODO: Turn into android-free tests or remove (some of them are reduntant now)
  /*
  feature("Exact handle match") {
    scenario("Run exact handle match if the handle is not found locally") {
      val service = stubService()
      val signal = service.search(RecommendedHandle("@fasol"), Set.empty[UserId])

    }

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
    */

/*
  feature("Top people search") {
    scenario("Return only connected users on top people search") {
      val result = search(TopPeople).sink

      within(1.second) {
        result.current.value should contain theSameElementsAs(ids('g, 'h, 'i))
      }
    }
  }

  def backendResults(results: Seq[UserData]): SearchQuery => Future[Unit] = { query =>
    service.updateSearchResults(query, results.map { r =>
      UserSearchEntry(r.id, r.name, Some(r.accent), handle = r.handle.get)
    })
  }


  */
}
