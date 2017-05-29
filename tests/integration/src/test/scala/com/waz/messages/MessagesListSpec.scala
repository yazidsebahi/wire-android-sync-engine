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

import com.waz.api.Message.Type._
import com.waz.api.MessageContent._
import com.waz.api._
import com.waz.model.ConversationData.ConversationType
import com.waz.service.RemoteZmsSpec
import com.waz.testutils.Implicits._
import com.waz.testutils.UnreliableAsyncClientImpl
import com.waz.utils.RichFuture
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class MessagesListSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with RemoteZmsSpec { test =>
  implicit val timeout: Timeout = 10.seconds

  override val provisionFile = "/two_users_connected.json"

  lazy val auto2 = createRemoteZms()

  override lazy val testClient = new UnreliableAsyncClientImpl

  feature("Syncing") {
    lazy val convs = api.getConversations
    lazy val conv = convs.getConversation(provisionedUserId("auto2").str)
    lazy val remoteConvId = conv.data.remoteId
    lazy val msgs = conv.getMessages

    scenario("init remote client") {
      awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))
    }

    scenario("initial sync") {
      withDelay {
        convs should not be empty
        conv.getType shouldEqual ConversationType.OneToOne
        msgs should have size 1
      }
    }

    scenario("post messages from other side") {
      awaitUiFuture(RichFuture.traverseSequential(1 to 40) { i =>
        auto2.postMessage(remoteConvId, new Text(s"text message $i"))
      })
    }

    scenario("receive messages") {
      withDelay(msgs should have size 41)(60.seconds)
      msgs.get(30)
      withDelay(msgs.getLastReadIndex shouldEqual 30)
    }

    scenario("post messages") {
      info(s"start count: ${msgs.size}")

      for (i <- 1 to 19) {
        conv.sendMessage(new Text(s"test message $i"))
        withDelay {
          msgs should have size (41 + i)
        }
      }

      withDelay {
        msgs should have size 60
        msgs.getLastMessage.data.isLocal shouldEqual false
      }(20.seconds) // all synced
    }

    scenario("post rich media message") {
      conv.sendMessage(new Text("Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI more text https://www.youtube.com/watch?v=c0KYU2j0TM4 and even more"))
      withDelay {
        msgs should have size 61
        msgs.getLastMessage.data.isLocal shouldEqual false
      }(10.seconds) // all synced

      val msg = msgs.get(60)
      msg.getMessageType shouldEqual RICH_MEDIA
      msg.getParts.map(p => (p.getPartType, p.getBody)) shouldEqual Array(
        (Message.Part.Type.TEXT, "Here is some text."),
        (Message.Part.Type.YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"),
        (Message.Part.Type.TEXT, "more text"),
        (Message.Part.Type.YOUTUBE, "https://www.youtube.com/watch?v=c0KYU2j0TM4"),
        (Message.Part.Type.TEXT, "and even more")
      )
    }

    scenario("post more messages from other side") {
      awaitUiFuture(RichFuture.traverseSequential(61 until 100) { i =>
        auto2.postMessage(remoteConvId, new Text(s"text message $i"))
      })(60.seconds)
      withDelay(msgs should have size 100)(60.seconds)
      val msg = msgs.get(99) // post last read
      withDelay {
        conv.data.lastRead should be >= msg.data.time
      }
    }
  }
}
