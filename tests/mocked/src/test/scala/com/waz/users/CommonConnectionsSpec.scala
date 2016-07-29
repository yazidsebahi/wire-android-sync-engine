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
package com.waz.users

import com.waz.api.MockedClientApiSpec
import com.waz.api.impl.SearchQuery.Query
import com.waz.mocked.MockBackend
import com.waz.model.{Relation, UserId}
import com.waz.service.GraphSearchService$
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.testutils.Implicits._
import com.waz.threading.CancellableFuture
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.duration._

class CommonConnectionsSpec extends FeatureSpec with Matchers with MockBackend with MockedClientApiSpec {
  import DefaultPushBehaviour.Implicit
  implicit val timeout: Timeout = 15.seconds

  lazy val convs = api.getConversations

  lazy val search = api.search()
  lazy val recommendedPeople = search.getRecommendedPeople(10)

  var requestedCommon = Option.empty[UserId]
  val recommended = UserId()
  lazy val topCommonConnections = connections.keys.take(GraphSearchService.MinCommonConnections).toSeq


  override protected def beforeAll(): Unit = {
    for (_ <- 1 to 10) addConnection()
    super.beforeAll()
  }

  override def graphSearch(query: Query, limit: Int): ErrorOrResponse[Seq[UserSearchEntry]] = CancellableFuture.successful(Right(Seq(
    UserSearchEntry(recommended, "test", None, None, 0, Some(false), blocked = false, Relation.Third, Some(10), topCommonConnections)
  )))

  override def loadCommonConnections(id: UserId): ErrorOrResponse[Seq[UserSearchEntry]] = {
    requestedCommon = Some(id)
    CancellableFuture.successful(Right(connections.keys.toSeq.map(id =>
      UserSearchEntry(id, "test " + id, None, None, 0, Some(true), blocked = false, Relation.First, None, Nil)
    )))
  }
  
  feature("load common connections") {

    scenario("load top common connections from recommended ppl search result") {
      withDelay(convs should have size 10) //self + 10 connections

      val users = recommendedPeople
      withDelay {
        users.getAll should have size 1
      }
      val user = users.getAll.head
      user.id shouldEqual recommended

      val common = user.getCommonConnections
      withDelay {
        common.getTotalCount shouldEqual 10
      }
      common.getTopConnections should have size GraphSearchService.MinCommonConnections
      common.getTopConnections.map(_.id).toSeq shouldEqual topCommonConnections

      awaitUi(250.millis)
      requestedCommon shouldEqual None
    }

    scenario("fetch more connections when full list is requested") {
      val common = recommendedPeople.getAll.head.getCommonConnections

      withDelay {
        common.getFullList should have size 10
      }
      requestedCommon shouldEqual Some(recommended)
      common.getFullList.map(_.id) shouldEqual connections.keys.toSeq
    }
  }
}
