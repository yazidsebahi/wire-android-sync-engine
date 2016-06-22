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

import akka.pattern.ask
import com.waz.api.MessageContent.Text
import com.waz.api._
import com.waz.provision.ActorMessage.{Login, Successful}
import com.waz.provision.ProvisionedSuite.{Conversation, User}
import com.waz.service.RemoteZmsSpec
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.threading.Threading
import com.waz.threading.Threading.Implicits.Background
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.duration._

class OtrPerformanceSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with RemoteZmsSpec with ThreadActorSpec { test =>

  override val otrOnly = true
  override val autoLogin: Boolean = false
  val count = 10

  override val provisionFile = "/two_users_connected.json"
  override lazy val provisionUsers: IndexedSeq[User] = (0 to count).map { i => User(s"auto$i", s"auto${i}_pass", s"auto$i user", connected = true) }
  override lazy val provisionConvs: Seq[Conversation] =
    (1 to count) .map (i => Conversation(None, Seq("auto0", s"auto$i"), Nil)) :+ Conversation(Some("group"), provisionUsers.map(_.email), Nil)

  lazy val convs = api.getConversations
  lazy val groupConv = {
    withDelay(convs.map(_.getType) should contain(IConversation.Type.GROUP))
    convs.find(_.getType == IConversation.Type.GROUP).get
  }
  lazy val msgs = groupConv.getMessages

  lazy val remotes = Seq.tabulate(count) { user =>
    (1 to 2) map { dev =>
      createRemoteZms(s"remote_${user}_$dev")
    }
  }
  lazy val allRemotes = remotes.flatten

  lazy val auto1 = registerDevice("otr_auto1_1")

  def getGroupConv(convs: ConversationsList) = {
    withDelay(convs.map(_.getType) should contain(IConversation.Type.GROUP))
    convs.find(_.getType == IConversation.Type.GROUP).get
  }

  feature(s"Otr Performance") {

    scenario("init remote processes") {
      val future = Future.traverse(remotes.zipWithIndex) { case (rs, i) =>
        Future.traverse(rs) { remote =>
          remote.login(provisionedEmail(s"auto$i"), s"auto${i}_pass")
        }
      }

      withDelay(future.isCompleted shouldEqual true)(45.seconds)

      println(s"login completed")

      val convs = allRemotes map (_.getConversations)
      withDelay {
        convs foreach { conv => conv should not be empty }
      }
    }

    scenario("init local api") {
      auto1 ? Login(email, password) should eventually(be(Successful))
      login() shouldEqual true
      withDelay {
        convs should not be empty
        convs should have size count + 1
      }
    }

    scenario("send message to group") {
      withDelay(msgs should not be empty)
      groupConv.sendMessage(new Text("test message 1"))

      withDelay {
        msgs.getLastMessage.getBody shouldEqual "test message 1"
        msgs.getLastMessage.data.state shouldEqual Message.Status.SENT
      }

      val remoteMsgs = awaitUiFuture(Future.traverse(allRemotes) { _.findConv(groupConv.data.remoteId).map(_.getMessages) }).get

      withDelay {
        remoteMsgs foreach { msgs =>
          msgs.getLastMessage.getMessageType shouldEqual Message.Type.TEXT
          msgs.getLastMessage.getBody shouldEqual "test message 1"
        }
      }
    }

    scenario("send messages from remotes") {
      val msgCount = msgs.size
      remotes.zipWithIndex foreach { case (rs, i) =>
        getGroupConv(rs.head.getConversations).sendMessage(new Text(s"remote message $i"))
      }

      withDelay {
        withClue(msgs.map(_.getBody + "\n").sorted) {
          msgs should have size (msgCount + count)
        }
      }
    }

    scenario("write message and check ordering") {
      val msgCount = msgs.size
      for (i <- 0 until 10) {
        groupConv.sendMessage(new Text(s"ordered message $i"))
        awaitUi(1.millis)
      }
      withDelay {
        withClue(msgs.drop(msgCount).map(_.getBody + "\n")) {
          msgs should have size (msgCount + 10)
          msgs.drop(msgCount).zipWithIndex foreach { case (msg, i) =>
            withClue(s"$i ${msg.data}") {
              msg.getBody shouldEqual s"ordered message $i"
              msg.data.state shouldEqual Message.Status.SENT
            }
          }
        }
      }
    }

    scenario("receive large message from remote") {
      val text = "large message: " + new String(Array.fill(10 * 1024)('_'))
      val msgCount = msgs.size
      getGroupConv(remotes.head.head.getConversations).sendMessage(new Text(text))

      withDelay {
        msgs.getLastMessage.getBody shouldEqual text
        msgs.getLastMessage.data.state shouldEqual Message.Status.SENT
      }
    }
  }

  override protected def beforeAll(): Unit = {
    Threading.AssertsEnabled = false
    super.beforeAll()
  }
}
