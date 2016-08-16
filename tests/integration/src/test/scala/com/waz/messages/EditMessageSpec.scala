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
import android.net.Uri
import com.waz.api.Message.Part
import com.waz.api.MessageContent.Text
import com.waz.api._
import com.waz.provision.ActorMessage._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

class EditMessageSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with ThreadActorSpec { test =>

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

  feature("Edit text messages") {

    scenario("Send text message and edit it") {
      conv.sendMessage(new Text("test msg to edit"))
      withDelay {
        msgs should have size 2
        msgs.getLastMessage.getMessageStatus shouldEqual Message.Status.SENT
      }

      val msg = msgs.getLastMessage

      msg.update(new Text("edited message"))

      withDelay {
        msgs should have size 2
        msg.isDeleted shouldEqual true
        val last = msgs.getLastMessage
        last.getEditTime.toEpochMilli should be > msg.getTime.toEpochMilli
        last.getBody shouldEqual "edited message"
        last.getId should not equal msg.getId
        last.getMessageStatus shouldEqual Message.Status.SENT
      }
    }

    scenario("Receive text message and edit from other user") {
      otherUserClient ? SendText(conv.data.remoteId, "test message 1") should eventually(be(Successful))
      withDelay {
        msgs should have size 3
        msgs.getLastMessage.getBody shouldEqual "test message 1"
      }

      val msg = msgs.getLastMessage

      conv.sendMessage(new Text("test"))

      withDelay(msgs should have size 4)

      otherUserClient ? UpdateText(msg.data.id, "updated") should eventually(be(Successful))

      withDelay {
        msg.isDeleted shouldEqual true
        msgs.get(2).getBody shouldEqual "updated"
        msgs.get(2).getMessageStatus shouldEqual Message.Status.SENT
        msgs.get(2).getEditTime.toEpochMilli should be > msg.getTime.toEpochMilli
      }
    }

    scenario("Edit on two devices in the same time") {
      secondClient ? SendText(conv.data.remoteId, "test message 2") should eventually(be(Successful))
      withDelay {
        msgs should have size 5
        msgs.getLastMessage.getBody shouldEqual "test message 2"
      }

      val msg = msgs.getLastMessage
      secondClient ? UpdateText(msg.data.id, "updated on remote") //should eventually(be(Successful))
      msg.update(new Text("updated locally"))

      withDelay {
        msg.isDeleted shouldEqual true
        val last = msgs.getLastMessage
        last.getMessageStatus shouldEqual Message.Status.SENT
        last.getEditTime.toEpochMilli should be > msg.getTime.toEpochMilli
      }

      val remote = (secondClient ? GetMessages(conv.data.remoteId)).await().asInstanceOf[ConvMessages].msgs

      info(s"last msg: ${msgs.getLastMessage.data}")
      remote.last.id shouldEqual msgs.getLastMessage.data.id
      remote.last.time shouldEqual msgs.getLastMessage.data.time
    }
  }

  feature("Edit link message") {
    scenario("Send link with text and edit it") {
      conv.sendMessage(new Text("test wire.com edit"))
      withDelay {
        msgs should have size 6
        msgs.getLastMessage.getMessageStatus shouldEqual Message.Status.SENT
        msgs.getLastMessage.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msgs.getLastMessage.getParts.toSeq.map(_.getPartType) shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK, Part.Type.TEXT)
      }

      val msg = msgs.getLastMessage

      msg.update(new Text("edited message facebook.com"))

      withDelay {
        msgs should have size 6
        msg.isDeleted shouldEqual true
        val last = msgs.getLastMessage
        last.getEditTime.toEpochMilli should be > msg.getTime.toEpochMilli
        last.getBody shouldEqual "edited message facebook.com"
        last.getId should not equal msg.getId
        last.getMessageStatus shouldEqual Message.Status.SENT
        last.getMessageType shouldEqual Message.Type.RICH_MEDIA

        last.getParts.toSeq.map(_.getPartType) shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK)
        last.getParts()(1).getContentUri shouldEqual Uri.parse("http://facebook.com")
      }
    }

    scenario("Receive link with text and edit update") {
      otherUserClient ? SendText(conv.data.remoteId, "test message wire.com") should eventually(be(Successful))
      withDelay {
        msgs should have size 7
        msgs.getLastMessage.getBody shouldEqual "test message wire.com"
      }

      val msg = msgs.getLastMessage

      conv.sendMessage(new Text("test"))

      withDelay(msgs should have size 8)

      otherUserClient ? UpdateText(msg.data.id, "updated facebook.com test") should eventually(be(Successful))

      withDelay {
        msg.isDeleted shouldEqual true
        msgs should have size 8
        val updated = msgs.get(6)
        updated.getId should not equal msg.getId
        updated.getBody shouldEqual "updated facebook.com test"
        updated.getMessageType shouldEqual Message.Type.RICH_MEDIA
        updated.getParts.toSeq.map(_.getPartType) shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK, Part.Type.TEXT)
        updated.getParts()(1).getContentUri shouldEqual Uri.parse("http://facebook.com")
        updated.getMessageStatus shouldEqual Message.Status.SENT
        updated.getEditTime.toEpochMilli should be > msg.getTime.toEpochMilli
      }
    }
  }
}
