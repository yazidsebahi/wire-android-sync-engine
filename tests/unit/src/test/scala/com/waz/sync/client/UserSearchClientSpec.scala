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
package com.waz.sync.client

import com.waz.api.impl.SearchQuery.{Named, RecommendedPeople, TopPeople}
import com.waz.model.UserId
import com.waz.service.ZMessaging
import com.waz.threading.CancellableFuture
import com.waz.znet.ZNetClient.EmptyClient
import com.waz.znet._
import org.json.JSONObject
import org.robolectric.Robolectric
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UserSearchClientSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests {

  val suggestionsResponse =
    """
      |{
      |    "buildNumber": 3737,
      |    "compares": 30445,
      |    "description": "ip-10-104-211-81.eu-west-1.compute.internal",
      |    "documents": [
      |        {
      |            "accent_id": 0,
      |            "id": "3514effc-b813-421f-bf17-9e26e9fece28",
      |            "level": 1,
      |            "name": "Foo",
      |            "weight": 11
      |        },
      |        {
      |            "accent_id": 0,
      |            "id": "ffd8f54e-5241-4ba6-84e5-480fd23a16f9",
      |            "level": 2,
      |            "name": "Meep",
      |            "weight": 43
      |        },
      |        {
      |            "accent_id": 0,
      |            "id": "202474e5-aab8-4696-a871-6f23c7731cf0",
      |            "level": 2,
      |            "name": "Moop",
      |            "weight": 30
      |        }
      |    ],
      |    "error": null,
      |    "found": 0,
      |    "friends": 0,
      |    "friendsOfFriends": 0,
      |    "returned": 3,
      |    "time": 24
      |}
      |    """.stripMargin

  val suggestionsWithCommonResponse =
    """
      |{
      |  "took": 1,
      |  "found": 0,
      |  "documents": [
      |    {
      |      "total_mutual_friends": 1,
      |      "mutual_friends": [
      |        "3514effc-b813-421f-bf17-9e26e9fece28"
      |      ],
      |      "weight": 32767,
      |      "accent_id": 1,
      |      "name": "Somebody",
      |      "id": "b3744e9b-dde2-4bfe-80f0-a57579b4e4b4",
      |      "level": 1
      |    },
      |    {
      |      "total_mutual_friends": 0,
      |      "mutual_friends": [],
      |      "weight": 32719,
      |      "accent_id": 1,
      |      "name": "Batman",
      |      "id": "2f559c52-1a83-4122-950a-e7fd8ff4b377",
      |      "level": 1
      |    },
      |    {
      |      "total_mutual_friends": 2,
      |      "mutual_friends": [
      |        "3514effc-b813-421f-bf17-9e26e9fece28",
      |        "855edd40-e735-4960-92d2-951256c5af31"
      |      ],
      |      "weight": 32719,
      |      "accent_id": 6,
      |      "name": "Spiderman",
      |      "id": "dcae6e1b-acce-4e7a-9c46-4eeac32e64b1",
      |      "level": 1
      |    },
      |    {
      |      "accent_id": 0,
      |      "id": "202474e5-aab8-4696-a871-6f23c7731cf0",
      |      "level": 2,
      |      "name": "Knuddel",
      |      "weight": 30
      |    }
      |  ],
      |  "returned": 3
      |}
    """.stripMargin

  val searchQueryResponse =
    """
      |{
      |    "buildNumber": 3737,
      |    "compares": 30445,
      |    "description": "ip-10-104-211-81.eu-west-1.compute.internal",
      |    "documents": [
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "zbigniew@wearezeta.com",
      |            "id": "3514effc-b813-421f-bf17-9e26e9fece28",
      |            "level": 1,
      |            "name": "Foo",
      |            "phone": "",
      |            "weight": 11
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": false,
      |            "email": null,
      |            "id": "ffd8f54e-5241-4ba6-84e5-480fd23a16f9",
      |            "level": 2,
      |            "name": "Broooot",
      |            "phone": null,
      |            "weight": 43
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": false,
      |            "email": null,
      |            "id": "202474e5-aab8-4696-a871-6f23c7731cf0",
      |            "level": 2,
      |            "name": "Schnucki",
      |            "phone": null,
      |            "weight": 30
      |        }
      |    ],
      |    "error": null,
      |    "found": 0,
      |    "friends": 0,
      |    "friendsOfFriends": 0,
      |    "returned": 3,
      |    "time": 24
      |}
    """.stripMargin

  val searchQueryResponse1 =
    """
      |{
      |   "took":82,
      |   "returned":6,
      |   "documents":[
      |      {
      |         "id":"3f1a831a-4379-4139-9119-b87b1c9fd5ba",
      |         "blocked":false,
      |         "level":1,
      |         "weight":0,
      |         "colorId":0,
      |         "name":"mqa104",
      |         "connected":false
      |      },
      |      {
      |         "id":"cc52c77b-765c-4283-828c-bc21354a8ef3",
      |         "blocked":false,
      |         "level":2,
      |         "weight":90,
      |         "colorId":0,
      |         "name":"mqa109",
      |         "connected":false
      |      },
      |      {
      |         "id":"417c262a-7d8e-4224-b40f-3bc2e05524ea",
      |         "blocked":false,
      |         "level":2,
      |         "weight":1,
      |         "colorId":0,
      |         "name":"mqa105😙",
      |         "connected":false
      |      },
      |      {
      |         "id":"4d1c1ed5-0926-4849-9d75-213a15efc401",
      |         "blocked":false,
      |         "level":2,
      |         "weight":0,
      |         "colorId":0,
      |         "name":"mqa107",
      |         "connected":false
      |      },
      |      {
      |         "id":"937c7e69-a326-493e-be3b-59f59cef19e3",
      |         "colorId":0,
      |         "name":"mqa102"
      |      },
      |      {
      |         "id":"c6eba669-253a-42df-9da1-177f7809f168",
      |         "colorId":0,
      |         "name":"mqa108"
      |      }
      |   ],
      |   "found":6
      |}
    """.stripMargin

  val searchQueryResponse2 = """{"documents":[{"id":"9c2c28d8-1e88-422a-8b2a-d611c660415a","level":1,"weight":190,"email":"android.test+auto2_ea4198a68dfea819@wearezeta.com","name":"auto2 user","connected":true}],"took":541,"returned":1,"found":0}"""

  val commonConnectionsResponse =
    """
      |{
      |    "buildNumber": 2478,
      |    "compares": 96,
      |    "description": null,
      |    "documents": [
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "zbigniew@wearezeta.com",
      |            "id": "3514effc-b813-421f-bf17-9e26e9fece28",
      |            "level": 1,
      |            "name": "Foo",
      |            "phone": "",
      |            "weight": 12
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "bjorn.herbig@wearezeta.com",
      |            "id": "636b79c2-027b-4503-b25e-b96eff400f1d",
      |            "level": 1,
      |            "name": "Harvey Metal",
      |            "phone": "",
      |            "weight": 11
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "kenny@wearezeta.com",
      |            "id": "5ee13682-aa9e-4714-a098-79b68a434923",
      |            "level": 1,
      |            "name": "Kliensmann",
      |            "phone": "",
      |            "weight": 8
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "hbo@wearezeta.com",
      |            "id": "f76c1c7a-7278-4b70-9df7-eca7980f3a5d",
      |            "level": 1,
      |            "name": "The Clown",
      |            "phone": "",
      |            "weight": 7
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "daniel.eggert@wearezeta.com",
      |            "id": "08316f5e-3c0a-4847-a235-2b4d93f291a4",
      |            "level": 1,
      |            "name": "Egbert",
      |            "phone": "",
      |            "weight": 4
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "malia.denis@wearezeta.com",
      |            "id": "80eb599e-8a82-44b5-ac67-f36735d6b5d8",
      |            "level": 1,
      |            "name": "Papa Smurf",
      |            "phone": "",
      |            "weight": 3
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "shumeng.ye@wearezeta.com",
      |            "id": "dcae6e1b-acce-4e7a-9c46-4eeac32e64b1",
      |            "level": 1,
      |            "name": "Green Lantern",
      |            "phone": "",
      |            "weight": 2
      |        },
      |        {
      |            "blocked": false,
      |            "colorId": 0,
      |            "connected": true,
      |            "email": "brianna@wearezeta.com",
      |            "id": "e384896c-8205-4b4a-8999-7ac29ae25f93",
      |            "level": 1,
      |            "name": "The Hulk",
      |            "phone": "",
      |            "weight": 1
      |        }
      |    ],
      |    "error": null,
      |    "found": 0,
      |    "friends": 0,
      |    "friendsOfFriends": 0,
      |    "returned": 8,
      |    "time": 0
      |}
    """.stripMargin

  var request: Request[Unit] = _

  def testClient(response: ResponseContent) = new EmptyClient {
    override def apply[A](r: Request[A]): CancellableFuture[Response] = {
      request = r.asInstanceOf[Request[Unit]]
      CancellableFuture.successful(Response(Response.HttpStatus(200, "OK"),response))
    }

    override def close(): Future[Unit] = Future(Unit)
  }

  class TestUserSearchClient(val response: ResponseContent) extends UserSearchClient(testClient(response)) {
    def this(resp: String) = this(JsonObjectResponse(new JSONObject(resp)))
  }

  before {
    request = null
    ZMessaging.context = Robolectric.application
  }

  feature("Response parsing") {
    scenario("graphSearch") {
      val client = new TestUserSearchClient(searchQueryResponse)
      val result = Await.result(client.graphSearch(Named("z"), 10), 5.seconds)

      result should be('right)
      result.right.get.length should be(3)
    }

    scenario("graphSearch 1") {
      val client = new TestUserSearchClient(searchQueryResponse1)
      val result = Await.result(client.graphSearch(Named("z"), 10), 5.seconds)

      result should be('right)
      result.right.get.length should be(6)
    }

    scenario("graphSearch 2") {
      val client = new TestUserSearchClient(searchQueryResponse2)
      val result = Await.result(client.graphSearch(Named("a"), 10), 5.seconds)

      result should be('right)
      result.right.get.length should be(1)
    }

    scenario("loadCommonConnectios") {
      val client = new TestUserSearchClient(commonConnectionsResponse)
      val result = Await.result(client.loadCommonConnections(UserId("12")), 5.seconds)

      result should be('right)
      result.right.get.length should be(8)
    }

    scenario("suggestions with common connections") {
      val client = new TestUserSearchClient(suggestionsWithCommonResponse)
      val result = Await.result(client.graphSearch(RecommendedPeople, 10), 5.seconds)

      result should be('right)
      result.right.get.length should be(4)
      result.right.get.map(_.commonCount) shouldEqual Seq(Some(1), Some(0), Some(2), None)
      result.right.get.map(_.common) shouldEqual Seq(Seq(UserId("3514effc-b813-421f-bf17-9e26e9fece28")), Nil, Seq(UserId("3514effc-b813-421f-bf17-9e26e9fece28"), UserId("855edd40-e735-4960-92d2-951256c5af31")), Nil)
    }
  }

  feature("Requests") {
    scenario("graphSearch request") {
      val client = new TestUserSearchClient(searchQueryResponse)
      Await.result(client.graphSearch(Named("z"), 10), 5.seconds)

      request.httpMethod should be("GET")
      request.resourcePath should be(Some("/search/contacts?q=z&size=10&l=3&d=1"))
    }

    scenario("loadCommonConnections request") {
      val client = new TestUserSearchClient(commonConnectionsResponse)
      Await.result(client.loadCommonConnections(UserId("12")), 5.seconds)

      request.httpMethod should be("GET")
      request.resourcePath should be(Some("/search/common/12"))
    }

    scenario("request for the recommended people") {
      val client = new TestUserSearchClient(suggestionsResponse)
      Await.result(client.graphSearch(RecommendedPeople, 2), 5.seconds)

      request.httpMethod should be("GET")
      request.resourcePath should be(Some("/search/suggestions?size=2"))
    }

    scenario("request for the top people") {
      val client = new TestUserSearchClient(searchQueryResponse)
      Await.result(client.graphSearch(TopPeople, 10), 5.seconds)

      request.httpMethod should be("GET")
      request.resourcePath should be(Some("/search/top?size=10"))
    }

    scenario("request to exclude user from pymk") {
      val client = new TestUserSearchClient(EmptyResponse)
      val uid = UserId()
      Await.result(client.postExcludePymk(uid), 5.seconds)

      request.httpMethod should be("PUT")
      request.resourcePath should be(Some(s"/search/suggestions/$uid/ignore"))
    }
  }
}
