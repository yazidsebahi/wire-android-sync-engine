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
package com.waz.mocked.search

import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.SearchQuery.{Named, RecommendedPeople, TopPeople}
import com.waz.api.{MockedClientApiSpec, SearchQuery, User}
import com.waz.mocked.MockBackend
import com.waz.model.SearchQueryCache.SearchQueryCacheDao
import com.waz.model._
import com.waz.service.SearchKey
import com.waz.sync.client.AddressBookClient.UserAndContactIds
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.testutils.HasId.idsOfAll
import com.waz.testutils.Implicits._
import com.waz.testutils._
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.Locales._
import com.waz.utils._
import com.waz.znet.ZNetClient._
import org.scalatest.concurrent.ScaledTimeSpans
import org.scalatest.{FeatureSpec, Inspectors}

import scala.collection.breakOut
import scala.concurrent.duration._
import scala.language.postfixOps

class SearchSpec extends FeatureSpec with Inspectors with ScaledTimeSpans with MockedClientApiSpec with MockBackend {
  import DefaultPushBehaviour.Implicit

  override lazy val selfUserId = UserId("thisisme")

  val self = UserInfo(selfUserId, Some("Cartoon Consumer"))
  val meeper = UserInfo(UserId("meeper"), Some("Road Runner"))
  val coyote = UserInfo(UserId("coyote"), Some("Wile E. Coyote"))
  val elmer = UserInfo(UserId("elmer"), Some("Elmer Fudd"))
  val bugs = UserInfo(UserId("bugs"), Some("Bugs Bunny"))
  val carrot = UserInfo(UserId("carrot"), Some("Carrot"))
  
  val recommended = 1 to 30 map { idx => UserInfo(UserId(s"user-$idx"), Some(s"User $idx"), email = Some(EmailAddress(s"user-$idx@dev.null"))) }

  object generated {
    val coyoteAndElmer = addGroupConversation(idsOfAll(elmer, coyote), id = RConvId("a"))
    val meeperAndBugs = addGroupConversation(idsOfAll(meeper, bugs), id = RConvId("b"))
    val bugsAndCarrot = addGroupConversation(idsOfAll(bugs, carrot), id = RConvId("c"))
  }

  object named {
    val neitherNameNorMembersMatch = addGroupConversation(idsOfAll(meeper, bugs), id = RConvId("d"), name = Some("some inconspicuous name"))
    val nameMatchesButMembersDont = addGroupConversation(idsOfAll(elmer, meeper, bugs), id = RConvId("e"), name = Some("this name clearly matches"))
    val nameDoesntMatchButMemberGivenNameMatches = addGroupConversation(idsOfAll(elmer, carrot, bugs), id = RConvId("f"), name = Some("some other inconspicuous name"))
    val nameDoesntMatchButMemberSurnameMatches = addGroupConversation(idsOfAll(meeper, coyote, bugs), id = RConvId("g"), name = Some("a totally unrelated name"))
    val nameAndMembersMatch = addGroupConversation(idsOfAll(coyote, carrot, meeper), id = RConvId("h"), name = Some("checkmate chess"))
  }

  lazy val selfUser = api.getSelf
  lazy val convs = api.getConversations

  var requestedIgnore = Option.empty[UserId]

  override def postExcludePymk(user: UserId): CancellableFuture[Option[ErrorResponse]] = {
    requestedIgnore = Some(user)
    super.postExcludePymk(user)
  }

  override protected def beforeAll(): Unit = {
    (Seq(self, meeper, coyote, elmer, bugs, carrot) ++ recommended) foreach (u => users += u.id -> u)
    Seq(meeper, coyote, elmer, bugs, carrot) foreach (u => addConnection(u.id))
    searchResults.put(Named("röäd"), Seq(meeper.entry()))
    searchResults.put(Named("C"), Seq(coyote.entry(), carrot.entry()))
    searchResults.put(RecommendedPeople, recommended.map(_.entry(Relation.Second)))

    prepareContacts(providedContacts:_*)
    postAddressBookResponse = Seq(
      (recommended(0).id, Set(cid("I"))),
      (recommended(1).id, Set(cid("II"))),
      (coyote.id, Set.empty[ContactId]),
      (elmer.id, Set.empty[ContactId]))

    generated
    named

    super.beforeAll()
  }

  feature("Search") {
    scenario("Searching users by name.") {
      givenSomeUsersAndConversations()

      whenSearchingFor("röäd ") {
        _.getUsers.userInfo should contain theSameElementsAs Seq(meeper)
      }
    }

    scenario("Searching group conversations by name.") {
      givenSomeUsersAndConversations()

      whenSearchingFor("Ç") { search =>
        idsOfAll(search.getConversations:_*) should contain theSameElementsAs idsOfAll(
          generated.coyoteAndElmer,
          generated.bugsAndCarrot,
          named.nameMatchesButMembersDont,
          named.nameDoesntMatchButMemberGivenNameMatches,
          named.nameDoesntMatchButMemberSurnameMatches,
          named.nameAndMembersMatch
        )

        search.getConversations should equal(search.getConversations.sortBy(_.getName)(currentLocaleOrdering))
      }
    }

    scenario("Searching for oneself.") {
      givenSomeUsersAndConversations()

      whenSearchingFor("Cartoon") { search =>
        search.getUsers shouldBe empty
        search.getConversations shouldBe empty
      }
    }
  }

  feature("Recommended people") {
    scenario("Include contacts in recommended people by default") {
      zmessaging.storage(SearchQueryCacheDao.deleteAll(_))
      val search = api.search.getRecommendedPeople(10)
      withDelay {
        idsOfAll(search.getAll:_*) should contain theSameElementsAs idsOfAll(recommended.take(10):_*)
      }
    }

    scenario("Search for recommended and exclude one.") {
      zmessaging.storage(SearchQueryCacheDao.deleteAll(_))
      requestedIgnore = None
      val search = api.search().getRecommendedPeople(5)
      withDelay {
        idsOfAll(search.getAll: _*) should contain theSameElementsAs idsOfAll(recommended.take(5): _*)
      }
      val excluded = search.getAll.head
      excluded.excludeFromPeopleYouMayKnow()
      withDelay {
        search.getAll should have size 4
        idsOfAll(search.getAll: _*) should not contain excluded.getId
        requestedIgnore shouldEqual Some(UserId(excluded.getId))
      }
    }

    scenario("Load more recommended people after removing a few") {
      requestedIgnore = None
      val search = api.search.getRecommendedPeople(15)
      withDelay {
        idsOfAll(search.getAll: _*) should contain theSameElementsAs idsOfAll(recommended.slice(1, 16): _*)
      }
      val excluded = search.getAll.take(9)
      excluded.take(9) foreach { _.excludeFromPeopleYouMayKnow() }
      val excludedIds = idsOfAll(excluded: _*)
      withDelay {
        search.getAll should have size 6
        idsOfAll(search.getAll: _*) should contain noneOf (excludedIds.head, excludedIds(1), excludedIds.drop(2): _*)
      }
      val extraExcluded = search.getAll.head
      extraExcluded.excludeFromPeopleYouMayKnow()
      withDelay {
        search.getAll should have size 15
        idsOfAll(search.getAll: _*) should contain noneOf (extraExcluded.getId, excludedIds.head, excludedIds.drop(1): _*)
      }
    }
  }

  feature("Top people") {

    scenario("Exclude just blocked users") {
      awaitUi(3.seconds) // previous tests spill into this one
      zmessaging.storage(SearchQueryCacheDao.deleteAll(_))
      searchResults.put(TopPeople, Seq(meeper, coyote, elmer, bugs, carrot).map(_.entry()))
      val topPeople = api.search.getTopPeople(12)
      withDelay {
        topPeople should have size 5
      }
      val all = topPeople.getAll
      all should have size 5

      all(1).block() // block user
      withDelay {
        topPeople should have size 4
        topPeople.getAll should have size 4
      }
    }

    scenario("Exclude blocked users on new search") {
      val topPeople = api.search.getTopPeople(12)
      withDelay {
        topPeople should have size 4
      }
    }
  }

  private def givenSomeUsersAndConversations(): Unit = withDelay {
    selfUser.isLoggedIn shouldBe true
    convs should have size conversations.size - 1
    val uiUser: Set[User] = convs.flatMap(_.getUsers)(breakOut)
    forEvery (uiUser) { u => u.getName shouldEqual users(UserId(u.getId)).name.get }
  }

  private def whenSearchingFor(pattern: String)(f: SearchQuery => Unit): Unit = {
    val search = api.searchQuery()
    val listener = returning(UpdateSpy())(search.addUpdateListener)
    search.setQuery(pattern, 12)
    withDelay {
      listener.numberOfTimesCalled should be >= 1
      f(search)
    }
  }

  override def postAddressBook(a: AddressBook): ErrorOrResponse[Seq[UserAndContactIds]] = CancellableFuture.delayed(1.second)(Right(postAddressBookResponse))(Threading.Background)

  @volatile var postAddressBookResponse: Seq[UserAndContactIds] = Nil

  lazy val providedContacts = Seq(
    Contact(ContactId("I"), "Bugs Hase", NameSource.StructuredName, "Bugs Hase", SearchKey("Bugs Hase"), Set(), Set(EmailAddress("bugs@bun.ny"))),
    Contact(ContactId("II"), "Karotte", NameSource.Nickname, "Karotte", SearchKey("Karotte"), Set(), Set(EmailAddress("c@arr.ot"))),
    Contact(ContactId("III"), "Valley", NameSource.Nickname, "Valley", SearchKey("Valley"), Set(), Set(EmailAddress("v@all.ey"))))

  def uids(strs: String*) = strs map UserId
  def cid(str: String) = ContactId(sha2(str))

  implicit class EnrichedUserInfo(user: UserInfo) { def entry(rel: Relation = Relation.First) = UserSearchEntry(user.id, user.name.get, None, None, user.accentId.getOrElse(0), Some(rel == Relation.First), blocked = false, rel) }
  implicit class EnrichedUserArray(users: Array[User]) { def userInfo: Seq[UserInfo] = users.map(u => UserInfo(UserId(u.getId), Some(u.getName)))(breakOut) }
}
