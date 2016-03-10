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
package com.waz.service.otr

import java.io.{ByteArrayInputStream, File}

import com.waz.api.MessageContent.Text
import com.waz.api.{ClientRegistrationState, Message, MessageContent, ProvisionedApiSpec}
import com.waz.service.RemoteZmsSpec
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.utils.IoUtils
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.util.Random

class OtrRecoverySpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with RemoteZmsSpec with ScalaFutures { test =>
  import com.waz.threading.Threading.Implicits.Background
  override val provisionFile = "/two_users_connected.json"

  lazy val convs = api.getConversations
  lazy val conv = {
    withDelay(convs should not be empty)
    convs.head
  }
  lazy val msgs = conv.getMessages

  lazy val tag = Random.nextLong().toHexString
  lazy val auto2 = createRemoteZms(tag)
  lazy val cryptoDir = auto2.zmessaging.get.cryptoBox.cryptoBoxDir

  lazy val auto2_1 = createRemoteZms(tag)

  lazy val auto2UserId = provisionedUserId("auto2")
  lazy val auto2User = api.getUser(provisionedUserId("auto2").str)
  lazy val auto2Clients = auto2User.getOtrClients

  feature("recover CryptoBox on login") {

    scenario("init both clients and exchange messages") {
      awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))
      withDelay {
        convs should not be empty
        zmessaging.otrClientsService.getSelfClient should eventually(be('defined))
      }

      conv.sendMessage(new MessageContent.Text("Test message"))

      conv.getId shouldEqual provisionedUserId("auto2").str
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text("Test message 1"))))

      withDelay {
        msgs.map(_.getBody) should contain allOf("Test message", "Test message 1")
        msgs.filter(_.getBody.startsWith("Test message")) foreach { msg =>
          msg.getMessageStatus shouldEqual Message.Status.SENT
          msg.isOtr shouldEqual true
        }

        auto2Clients should have size 1
      }
    }

    scenario("logout auto2") {
      val auto2Self = auto2.getSelf
      auto2Self.isLoggedIn shouldEqual true
      cryptoDir should exist
      val cryptobox = auto2.zmessaging.get.cryptoBox

      info("clientId: " + auto2.zmessaging.get.otrContent.currentClientId.futureValue)

      auto2.logout()

      withDelay {
        auto2Self.isLoggedIn shouldEqual false
      }

      cryptobox.close().futureValue
    }

    scenario("corrupt auto2 otr and login back") {
      val identity = new File(new File(cryptoDir, "identities"), "local")
      identity should exist
      IoUtils.copy(new ByteArrayInputStream("broken".getBytes), identity)

      val auto2Self = auto2.getSelf
      auto2Self.isLoggedIn shouldEqual false

      awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))

      // login fails due to cryptobox failing
      auto2Self.isLoggedIn shouldEqual false
    }

    scenario("second login creates new client") {
      val auto2Self = auto2.getSelf
      auto2Self.isLoggedIn shouldEqual false

      awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))

      // auto2 should be logged in
      withDelay {
        auto2Self.isLoggedIn shouldEqual true
        auto2Self.getClientRegistrationState shouldEqual ClientRegistrationState.REGISTERED
      }

      auto2.zmessaging.get.cryptoBox.cryptoBox.futureValue shouldBe 'defined
      info("clientId: " + auto2.zmessaging.get.otrContent.currentClientId.futureValue)

      val msgs2 = awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.getMessages)).get

      conv.sendMessage(new MessageContent.Text("Test message 2"))

      withDelay {
        auto2Clients should have size 2
        msgs2.getLastMessage.getBody shouldEqual "Test message 2"
      }
    }
  }

}
