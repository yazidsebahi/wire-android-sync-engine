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
package com.waz.messages

import akka.pattern.ask
import com.waz.api._
import com.waz.provision.ActorMessage._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

class LocationMessageSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val otherUser = provisionedUserId("auto2")
  lazy val conv = conversations.get(0)
  lazy val msgs = conv.getMessages

  lazy val otherUserClient = registerDevice("other_user")
  lazy val secondClient = registerDevice("second_client")

  scenario("Initial sync") {
    withDelay(msgs should have size 1)
  }

  scenario("Init remotes") {
    secondClient ? Login(email, password) should eventually(be(Successful))
    secondClient ? AwaitSyncCompleted should eventually(be(Successful))

    otherUserClient ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    otherUserClient ? AwaitSyncCompleted should eventually(be(Successful))
  }

  feature("Send location") {

    scenario("Send location message") {
      val loc = new MessageContent.Location(-10.45f, 23.43f, "location name", 10)
      conv.sendMessage(loc)
      withDelay {
        msgs should have size 2
        msgs.getLastMessage.getMessageType shouldEqual Message.Type.LOCATION
        msgs.getLastMessage.getMessageStatus shouldEqual Message.Status.SENT
      }
      val msg = msgs.getLastMessage
      msg.getLocation shouldEqual loc
    }
  }

  feature("Receive location") {

    scenario("Receive message from other user") {
      val fromBefore = msgs.size()

      otherUserClient ? SendLocation(conv.data.remoteId, 52.5270968f, 13.4020788f, "test location", 9) should eventually(beMatching { case Successful(_) => true })

      withDelay {
        msgs should have size (fromBefore + 1)
        msgs.getLastMessage.getMessageType shouldEqual Message.Type.LOCATION
        msgs.getLastMessage.getMessageStatus shouldEqual Message.Status.SENT
      }
      val msg = msgs.getLastMessage
      msg.getLocation shouldEqual new MessageContent.Location(52.5270968f, 13.4020788f, "test location", 9)
    }

    scenario("Load location map image") {
      val msg = msgs.getLastMessage
      val asset = msg.getImage
      asset.isEmpty shouldEqual false
      val spy = new BitmapSpy(asset)
      withDelay {
        spy.failed shouldEqual false
        spy.result shouldBe defined
      }
      info("image size: " + spy.result.map(b => (b.getWidth, b.getHeight)))
    }

    scenario("Load map image with specified size") {
      val msg = msgs.getLastMessage
      val asset = msg.getImage(640, 240)
      asset.isEmpty shouldEqual false
      val spy = new BitmapSpy(asset)
      withDelay {
        spy.failed shouldEqual false
        spy.result shouldBe defined
        spy.result.map(b => (b.getWidth, b.getHeight)) shouldEqual Some((640, 240))
      }
    }
    
    scenario("Load map image with size bigger than 640") {
      val msg = msgs.getLastMessage
      val asset = msg.getImage(800, 600)
      asset.isEmpty shouldEqual false
      val spy = new BitmapSpy(asset)
      withDelay {
        spy.failed shouldEqual false
        spy.result shouldBe defined
        spy.result.map(b => (b.getWidth, b.getHeight)) shouldEqual Some((640, 480))
      }
    }

    scenario("Receive message sent from second client") {
      val fromBefore = msgs.size()

      secondClient ? SendLocation(conv.data.remoteId, 1f, 2f, "self location", 9) should eventually(beMatching { case Successful(_) => true })

      withDelay {
        msgs should have size (fromBefore + 1)
        msgs.getLastMessage.getMessageType shouldEqual Message.Type.LOCATION
        msgs.getLastMessage.getMessageStatus shouldEqual Message.Status.SENT
      }
      val msg = msgs.getLastMessage
      msg.getLocation shouldEqual new MessageContent.Location(1f, 2f, "self location", 9)
    }
  }
}
