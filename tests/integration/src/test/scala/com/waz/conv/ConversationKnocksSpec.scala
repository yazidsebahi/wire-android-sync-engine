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
package com.waz.conv

import java.lang.System.currentTimeMillis

import akka.pattern.ask
import com.waz.api.IncomingMessagesList.KnockListener
import com.waz.api.MessageContent._
import com.waz.api.{Message => ApiMessage, _}
import com.waz.model.RConvId
import com.waz.provision.ActorMessage._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.utils.events.EventContext
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.duration._

class ConversationKnocksSpec extends FeatureSpec with Matchers with ProvisionedApiSpec with ThreadActorSpec {
  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val conv = {
    withDelay {
      conversations should have size 1
    }
    conversations.getConversation(provisionedUserId("auto2").str)
  }
  lazy val self = api.getSelf
  lazy val msgs = conv.getMessages

  lazy val knocks = api.getIncomingMessages
  lazy val inputState = conv.getInputStateIndicator

  lazy val auto2 = registerDevice("ConversationKnocksSpec_auto2")

  feature("Send knock") {
    implicit val ev = EventContext.Global

    scenario("send single knock") {
      withDelay { msgs should have size 1 }

      conv.knock()

      withDelay {
        msgs.getLastMessage.getMessageType shouldEqual ApiMessage.Type.KNOCK
        msgs.getLastMessage.getMessageStatus shouldEqual ApiMessage.Status.SENT
        msgs.getLastMessage.isHotKnock shouldEqual false
      }
    }
  }

  feature("Receive knocks") {

    scenario("Conversation incomingKnock should not contain self knocks") {
      conv.getIncomingKnock should be(null)
    }

    scenario("init remote process") {
      auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
      awaitUi(1.second)
    }

    scenario("Receive incoming knock message") {
      val count = msgs.size()
      auto2 ? Knock(RConvId(self.getUser.getId)) should eventually(be(Successful))

      withDelay {
        msgs should have size (count + 1)
        msgs.getLastMessage.getMessageType shouldEqual ApiMessage.Type.KNOCK
        msgs.getLastMessage.isHotKnock shouldEqual false
        msgs.getLastMessage.getLocalTime.toEpochMilli should be(currentTimeMillis() +- 1000)
        conv.getIncomingKnock should not be null
        conv.getIncomingKnock.isHotKnock shouldEqual false
        conv.getIncomingKnock.getLocalTime.toEpochMilli should be(currentTimeMillis() +- 1000)
      }
    }
  }

  feature("Knock listener") {

    scenario("knocks list should contain only incoming knocks") {
      withDelay(knocks should not be empty)

      knocks.forall(_.getUser != self.getUser) shouldEqual true
    }

    scenario("don't notify on self knocks") {
      val msgCount = msgs.size
      conv.sendMessage(new Text("divider")) // send message so we can knock again
      withDelay {
        msgs should have size (msgCount + 1)
      }

      val count = knocks.size()
      conv.knock()

      withDelay {
        msgs should have size (msgCount + 2)
      }
      knocks should have size count
    }

    scenario("notify on incoming knock") {
      val count = knocks.size()
      val msgCount = msgs.size

      @volatile var knock = None: Option[ApiMessage]
      knocks.addKnockListener(new KnockListener {
        override def onKnock(k: ApiMessage): Unit = knock = Some(k)
      })

      auto2 ? Knock(RConvId(self.getUser.getId)) should eventually(be(Successful))

      withDelay {
        withClue(s"from listener: $knock,\nknocks: ${knocks.map(_.data)}") {
          msgs should have size (msgCount + 1)
          knocks should have size (count + 1)
          knock should be('defined)
          knocks.lastOption shouldEqual knock
        }
      }(20.seconds)
    }
  }
}
