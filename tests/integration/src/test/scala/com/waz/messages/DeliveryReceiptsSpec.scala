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
import com.waz.ZLog._
import com.waz.api.IConversation.Type.{GROUP, ONE_TO_ONE}
import com.waz.api.Message.Status.{DELIVERED, SENT}
import com.waz.api.MessageContent._
import com.waz.api._
import com.waz.model.UserId
import com.waz.provision.ActorMessage._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Inspectors, Matchers}

import scala.concurrent.duration._

class DeliveryReceiptsSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with Inspectors with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/three_users_group_conv.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val friend1 = provisionedUserId("auto2")
  lazy val friend2 = provisionedUserId("auto3")
  lazy val friend1Conv = conversations.find(c => c.getType == ONE_TO_ONE && UserId(c.getId) == friend1).get
  lazy val friend2Conv = conversations.find(c => c.getType == ONE_TO_ONE && UserId(c.getId) == friend1).get
  lazy val groupConv = conversations.find(c => c.getType == GROUP).get
  lazy val (friend1RConv, friend2RConv, groupRConv) = (friend1Conv.data.remoteId, friend2Conv.data.remoteId, groupConv.data.remoteId)
  lazy val (friend1Msgs, friend2Msgs, groupMsgs) = (friend1Conv.getMessages, friend2Conv.getMessages, groupConv.getMessages)

  lazy val friend1Client = registerDevice(logTagFor[DeliveryReceiptsSpec] + "_1")
  lazy val friend2Client = registerDevice(logTagFor[DeliveryReceiptsSpec] + "_2")

  scenario("Initial sync") {
    soon {
      friend1Msgs should have size 1
      friend2Msgs should have size 1
      groupMsgs should have size 1
    }
  }

  scenario("Init other clients") {
    friend1Client ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    friend2Client ? Login(provisionedEmail("auto3"), "auto3_pass") should eventually(be(Successful))
    friend1Client ? AwaitSyncCompleted should eventually(be(Successful))
    friend2Client ? AwaitSyncCompleted should eventually(be(Successful))
  }

  feature("Delivery receipts") {
    scenario("Receipt in 1-on-1 conv") {
      friend1Conv.sendMessage(new Text("meep"))
      (friend1Msgs should have size 2).soon
      (friend1Msgs.getLastMessage.getMessageStatus shouldEqual SENT).soon
      (friend1Msgs.getLastMessage.getMessageStatus shouldEqual DELIVERED).soon
    }

    scenario("No receipt in group conv") {
      groupConv.sendMessage(new Text("moop"))
      (groupMsgs should have size 2).soon
      (groupMsgs.getLastMessage.getMessageStatus shouldEqual SENT).soon
      forAsLongAs(3.seconds)(groupMsgs.getLastMessage.getMessageStatus shouldEqual SENT)
    }
  }
}
