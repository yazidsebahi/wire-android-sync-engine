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

import com.waz.model._
import com.waz.sync.client.EventsClient.{NotificationsResponse, PagedNotificationsResponse}
import com.waz.utils.JsonDecoder
import com.waz.znet.JsonObjectResponse
import org.json
import org.json.JSONObject
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.io.Source

class EventsClientSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests {

  scenario("parse event time") {
    val time = JsonDecoder.parseDate("2015-07-13T13:32:04.584Z").getTime
    JsonDecoder.parseDate("2015-07-13T13:32:04Z").getTime shouldEqual (time / 1000 * 1000)
    JsonDecoder.parseDate("2015-07-13T13:32:04.584285000000Z").getTime shouldEqual (time / 1000 * 1000)
  }

  scenario("parse notifications response") {
    val json = new JSONObject(Source.fromInputStream(getClass.getResourceAsStream("/events/notifications.json")).getLines().mkString("\n"))
    JsonObjectResponse(json) match {
      case PagedNotificationsResponse((notifications, false, _)) => //info(s"found notifications: $notifications")
        notifications foreach { n => n.transient shouldEqual false }
      case _ => fail(s"notifications parsing failed for: $json")
    }
  }

  scenario("parse notifications response 1") {
    val json = new JSONObject(Source.fromInputStream(getClass.getResourceAsStream("/events/notifications1.json")).getLines().mkString("\n"))
    JsonObjectResponse(json) match {
      case PagedNotificationsResponse((notifications, true, _)) => //info(s"found notifications: $notifications")
      case _ => fail(s"notifications parsing failed for: $json")
    }
  }

  scenario("parse notifications response 3") {
    val json = new JSONObject("""{"id":"a0f2b5b2-798d-4fba-8147-32b3dd519e76","payload":[{"time":"2014-05-12T17:13:55.213Z","data":{"last_event_time":"2014-05-12T17:13:52.119Z","id":"3347c16e-08dc-4203-8a68-053c1bd1bd1c","last_event":"1.8001080027b41a72","name":"conn1","type":3,"members":{"others":[],"self":{"id":"bbd6cd39-e5d0-48f5-8da4-a4632a2a2e6a","muted_time":null,"last_read":null,"archived":null,"muted":null,"status":0,"status_ref":"0.0","status_time":"2014-05-12T17:13:53.119Z"}},"creator":"bbd6cd39-e5d0-48f5-8da4-a4632a2a2e6a"},"conversation":"3347c16e-08dc-4203-8a68-053c1bd1bd1c","from":"bbd6cd39-e5d0-48f5-8da4-a4632a2a2e6a","type":"conversation.create"}]}""")
    JsonObjectResponse(json) match {
      case NotificationsResponse(notification) => //info(s"found notification: $notification")
      case _ => fail(s"notifications parsing failed for: $json")
    }
  }

  scenario("Parse transient notification") {
    val n = PushNotification.NotificationDecoder(new JSONObject(TransientNotification))
    n.transient shouldEqual true
    n.events should have size 1
  }

  scenario("Parse contact join notification") {
    val n = PushNotification.NotificationDecoder(new JSONObject(ContactJoinNotification))
    n.transient shouldEqual false
    n.events should have size 1
    n.events match {
      case Seq(ContactJoinEvent(user, name)) =>
        user shouldEqual UserId("13962457-c316-4de1-9962-929c40f8cff4")
        name shouldEqual "Name of the new user"
      case evs => fail(s"unexpected events: $evs")
    }
  }

  scenario("parse user connection notification event") {
    Event.EventDecoder(new JSONObject(UserConnectionEvent)) match {
      case cp: UserConnectionEvent =>
        cp.fromUserName shouldEqual Some("some name")
      case ev => fail(s"unexpected event: $ev")
    }
  }

  scenario("parse call state event") {
    Event.EventDecoder(new json.JSONObject(CallStateEvent)) match {
      case cp: CallStateEvent =>
        cp.convId shouldEqual RConvId("a50acb79-d7a3-4fdc-b885-e779ac0c0689")
        cp.sessionId shouldEqual None
        info(s"participants: ${cp.participants}")
      case ev => fail(s"unexpected event: $ev")
    }
  }

  scenario("parse call state event with session id") {
    Event.EventDecoder(new json.JSONObject(CallStateEventWithSession)) match {
      case cp: CallStateEvent =>
        cp.convId shouldEqual RConvId("a50acb79-d7a3-4fdc-b885-e779ac0c0689")
        cp.sessionId shouldEqual Some(CallSessionId("56357538-6b42-44ba-8de8-12df89af7e65"))
      case ev => fail(s"unexpected event: $ev")
    }
  }

  scenario("ignore call info event") {
    Event.EventDecoder(new json.JSONObject(CallInfoEvent)) match {
      case cp: IgnoredEvent => // expected
      case ev => fail(s"unexpected event: $ev")
    }
  }

  scenario("removed identity event") {
    Event.EventDecoder(new json.JSONObject(userUpdateEventRemovePhone)) match {
      case cp: UserUpdateEvent => cp.removeIdentity shouldEqual true
      case ev => fail("unexpected event: $ev")
    }
  }

  val TransientNotification =
    """
      { "id" : "fbe54fb4-463e-4746-9861-c28c2961bdd0",
        "transient" : true,
        "payload" : [ { "conversation" : "3b45e65a-8bf2-447b-bd8a-03c207deae3f",
              "data" : { "content" : "Test message 2",
                  "nonce" : "47745f9f0-0dab-113c-43ad7ee9-394c562"
                },
              "from" : "13962457-c316-4de1-9962-929c40f8cff4",
              "id" : "f.80011231430865a7",
              "time" : "2014-04-14T09:56:00.185Z",
              "type" : "conversation.message-add"
            } ]
      }
    """

  val ContactJoinNotification =
    """
      |{
      |  "id": "be82150e-d176-4018-9eb4-b29712348790",
      |  "transient": false,
      |  "payload": [
      |      {
      |          "type": "user.contact-join",
      |          "user": {
      |              "name": "Name of the new user",
      |              "id": "13962457-c316-4de1-9962-929c40f8cff4"
      |          }
      |      }
      |  ]
      |}
    """.stripMargin

  val CallInfoEvent = """{
                                |   "type":"call.info",
                                |   "participants":{
                                |      "56357538-6b42-44ba-8de8-12df89af7e65":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      },
                                |      "c43cfe3c-60c6-4f67-8277-8aa44a308684":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      }
                                |   },
                                |   "conversation":"a50acb79-d7a3-4fdc-b885-e779ac0c0689"
                                |}""".stripMargin

  val CallStateEvent = """{
                                |   "type":"call.state",
                                |   "cause":"requested",
                                |   "participants":{
                                |      "56357538-6b42-44ba-8de8-12df89af7e65":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      },
                                |      "c43cfe3c-60c6-4f67-8277-8aa44a308684":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      }
                                |   },
                                |   "conversation":"a50acb79-d7a3-4fdc-b885-e779ac0c0689"
                                |}""".stripMargin

  val CallStateEventWithSession = """{
                                |   "type":"call.state",
                                |   "cause":"requested",
                                |   "participants":{
                                |      "56357538-6b42-44ba-8de8-12df89af7e65":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      },
                                |      "c43cfe3c-60c6-4f67-8277-8aa44a308684":{
                                |         "quality":null,
                                |         "state":"idle"
                                |      }
                                |   },
                                |   "session":"56357538-6b42-44ba-8de8-12df89af7e65",
                                |   "conversation":"a50acb79-d7a3-4fdc-b885-e779ac0c0689"
                                |}""".stripMargin

  val UserConnectionEvent =
    """{
      |  "connection":{
      |    "message":"meep",
      |    "to":"d5a833d6-b3a6-4cd4-b128-5d64cbf214c3",
      |    "last_update":"2015-06-02T08:52:49.293Z",
      |    "status":"pending",
      |    "conversation":"00d994be-7056-4cd4-aa68-51ad4e846edf",
      |    "from":"2b3160e8-bcb0-4000-b93f-f44982925a1c"
      |  },
      |  "type":"user.connection",
      |  "user":{
      |    "name":"some name"
      |  }
      |}""".stripMargin

  val userUpdateEventRemovePhone =
    """{
      |  "type":"user.identity-remove",
      |  "user":{
      |    "phone":"+441787540048",
      |    "id":"7ddb63d0-cd32-408b-a720-f33e5262bfd8"
      |  }
      |}""".stripMargin

}
