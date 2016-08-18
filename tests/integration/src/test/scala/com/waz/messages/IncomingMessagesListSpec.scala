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

import com.waz.api.IncomingMessagesList.KnockListener
import com.waz.api.MessageContent._
import com.waz.api._
import com.waz.service.RemoteZmsSpec
import com.waz.testutils.Implicits._
import org.scalatest.{BeforeAndAfter, EitherValues, FeatureSpec, Matchers}

import scala.concurrent.duration._
import com.waz.threading.Threading.Implicits.Background

class IncomingMessagesListSpec extends FeatureSpec with Matchers with EitherValues with BeforeAndAfter with ProvisionedApiSpec with RemoteZmsSpec { test =>
  implicit val timeout: Timeout = 10.seconds

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = {
    withDelay {
      conversations.size should be > 0
    }
    conversations.find(_.getId == auto2Id.str).get
  }
  var lastKnock = None: Option[Message]

  lazy val msgs = conv.getMessages
  lazy val incoming = {
    val list = api.getIncomingMessages
    list.addKnockListener(new KnockListener() {
      override def onKnock(knock: Message): Unit = lastKnock = Some(knock)
    })
    list
  }

  lazy val auto2 = createRemoteZms()

  lazy val auto2Id = provisionedUserId("auto2")

  lazy val secondClient = createRemoteZms()

  before {
    lastKnock = None
  }

  scenario("sync all") {
    login()

    withDelay {
      conv.getName shouldEqual "auto2 user"
    }
    awaitUi(5.seconds)
  }

  feature("Syncing") {

    scenario("init auto2") {
      awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))
      awaitUiFuture(secondClient.login(email, password))
    }

    scenario("add incoming message when other user posts") {
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text("test message"))))

      withDelay {
        msgs should not be empty
        incoming should not be empty

        msgs.getLastMessage shouldEqual incoming.last
      }
    }

    scenario("don't add to incoming if message was sent by self") {

      val count = msgs.size()
      conv.sendMessage(new Text("self message"))

      withDelay {
        msgs should have size (count + 1)
        msgs.getLastMessage.data.isLocal shouldEqual false
      }
      Thread.sleep(500)
      incoming should have size 1
    }

    scenario("don't add to incoming if message was sent by self from other client") {

      val count = msgs.size()
      awaitUiFuture(secondClient.findConv(conv.data.remoteId).map(_.sendMessage(new Text("self message 2"))))

      try {
        withDelay { msgs should have size (count + 1) }
      } finally {
        info(s"messages: ${msgs.map(m => m.data.msgType -> m.data.contentString)}")
      }

      Thread.sleep(500)
      incoming should have size 1
    }
  }

  feature("Knocks")  {

    scenario("Call knock listener on new knock") {
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.knock()))

      withDelay {
        lastKnock should be('defined)
        lastKnock.map(_.isHotKnock) shouldEqual Some(false)

        incoming.last.getMessageType shouldEqual Message.Type.KNOCK
      }
    }

    scenario("Call knock listener on hot knock") {
      awaitUi(3.seconds)
      val msg = incoming.last
      msg.getMessageType shouldEqual Message.Type.KNOCK

      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.knock()))

      withDelay {
        msg.isHotKnock shouldEqual true

        lastKnock should be('defined) // FIXME doesn't work, sometimes
        lastKnock.map(_.isHotKnock) shouldEqual Some(true)
      }
    }
  }
}
