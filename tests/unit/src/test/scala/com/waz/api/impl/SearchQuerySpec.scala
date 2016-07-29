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

import android.database.sqlite.SQLiteDatabase
import com.waz._
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service.conversation.ConversationsUiService
import com.waz.service.{SearchKey, GraphSearchService$}
import com.waz.testutils.{MockUiModule, MockZMessaging}
import com.waz.threading.CancellableFuture
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SearchQuerySpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with RobolectricTests with RobolectricUtils {

  var userSearchRequest: Option[(SearchQuery.DbQuery, Int)] = None
  var userSearchResult: List[UserId] = _
  var convSearchRequest: Option[(String, Int)] = None
  var convSearchResult: List[ConversationData] = _

  lazy val zmessaging = new MockZMessaging() {
    override lazy val userSearch: GraphSearchService = new GraphSearchService(context, db, users, usersStorage, timeouts, sync) {
      override def searchUsers(query: SearchQuery.DbQuery, limit: Int) = {
        userSearchRequest = Some((query, limit))
        CancellableFuture.successful(userSearchResult)
      }
    }
    override lazy val convsUi: ConversationsUiService = new ConversationsUiService(assets, users, usersStorage, db, messages, membersStorage, assetsStorage, convsContent, convsStorage, network, conversations, voice, sync, lifecycle, trackingEvents, errors) {
      override def findGroupConversations(query: SearchKey, limit: Int): Future[List[ConversationData]] = {
        convSearchRequest = Some((query.asciiRepresentation, limit))
        Future.successful(convSearchResult)
      }
    }
  }
  
  implicit lazy val ui = new MockUiModule(zmessaging)

  implicit def database: SQLiteDatabase = zmessaging.db.dbHelper.getWritableDatabase

  lazy val query = new SearchQuery

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    ui.onCreate(context)
    ui.onResume()
  }

  before {
    userSearchRequest = None
    userSearchResult = Nil
    convSearchRequest = None
    convSearchResult = Nil
 }

  after {
    query.setQuery("", 0, Array.empty)
    Await.result(zmessaging.db { _ => }, 5.seconds)
    context.deleteDatabase(zmessaging.db.dbHelper.getDatabaseName)
    ui.users.users.clear()
  }

  feature("Loader setup") {

    scenario("Search for users") {
      query.setQuery("friend", 5)
      withDelay { userSearchRequest shouldEqual Some(SearchQuery.Named("friend"), 5) }
    }

    scenario("Search for users with filter") {
      query.setQuery("friend", 10, Array("1", "2"))
      withDelay { userSearchRequest shouldEqual Some(SearchQuery.Named("friend"), 12) }
    }
  }

  feature("Processing load results") {

    scenario("Search for users") {
      userSearchResult = getUsersResult("1", "2", "3")
      query.setQuery("friend", 10, Array())
      withDelay(query.getUsers shouldEqual List("1", "2", "3").map(id => ui.users.getUser(UserId(id))))
    }

    scenario("Search with limit") {
      userSearchResult = getUsersResult("1", "2", "3", "4")
      query.setQuery("friend", 2, Array())
      withDelay(query.getUsers shouldEqual List("1", "2").map(id => ui.users.getUser(UserId(id))))
    }

    scenario("Search with filter") {
      userSearchResult = getUsersResult("1", "2", "3", "4")
      query.setQuery("friend", 10, Array("2", "3"))
      withDelay(query.getUsers shouldEqual List("1", "4").map(id => ui.users.getUser(UserId(id))))
    }

    scenario("Search with filter and limit") {
      userSearchResult = getUsersResult("1", "2", "3", "4", "5")
      query.setQuery("friend", 2, Array("2", "3"))
      withDelay(query.getUsers shouldEqual List("1", "4").map(id => ui.users.getUser(UserId(id))))
    }

    scenario("Process conversations results") {
      convSearchResult = getConvResult("1", "2", "3")

      query.setQuery("friend", 10, Array())
      withDelay(query.getConversations.toList shouldEqual List("1", "2", "3").map(id => ui.convs.convById(ConvId(id))))
    }

    scenario("Return only accepted connections in getContacts") {
      import com.waz.model.UserData.ConnectionStatus._
      userSearchResult = getUsersResultWithConnection("1" -> Accepted, "2" -> Ignored, "3" -> Blocked, "4" -> Accepted, "5" -> PendingFromOther, "6" -> PendingFromUser)
      query.setQuery("friend", 10, Array())
      withDelay {
        query.getUsers.map(_.getId) shouldEqual List("1", "2", "3", "4", "5", "6")
        query.getContacts.map(_.getId).toList shouldEqual List("1", "4")
      }
    }
  }

  feature("Get top people") {

    scenario("Load top people") {
      query.setTopPeopleQuery(10)

      withDelay {
        userSearchRequest shouldEqual Some(SearchQuery.TopPeople, 10)
      }
    }
  }

  def getUsersResult(userIds: String*) = {
    userIds.foreach { id =>
      val name = s"friend_$id"
      Await.result(zmessaging.usersStorage.addOrOverwrite(UserData(UserId(id), name, None, None, None, None, 0, SearchKey(name))), 1.second)
    }
    userIds.map(UserId(_)).toList
  }

  def getUsersResultWithConnection(users: (String, ConnectionStatus)*) = {
    users foreach { case (id, conn) =>
      val name = s"friend_$id"
      Await.result(zmessaging.usersStorage.addOrOverwrite(UserData(UserId(id), name, None, None, None, None, 0, SearchKey(name), connection = conn)), 1.second)
    }
    users.map { case (id, conn) => UserId(id) } .toList
  }

  def getConvResult(convIds: String*) =
      convIds.map(id => ConversationData(ConvId(id), RConvId(id), Some(id), UserId(), ConversationData.ConversationType.Group)).toList
}
