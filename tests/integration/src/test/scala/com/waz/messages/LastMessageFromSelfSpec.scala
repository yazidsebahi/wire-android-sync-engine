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
import com.waz.api.MessageContent._
import com.waz.api._
import com.waz.provision.ActorMessage._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Inspectors, Matchers}

import scala.concurrent.duration._

class LastMessageFromSelfSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with Inspectors with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val friend = provisionedUserId("auto2")
  lazy val conv = conversations.get(0)
  lazy val rconv = conv.data.remoteId
  lazy val msgs = conv.getMessages

  lazy val friendClient = registerDevice(logTagFor[LastMessageFromSelfSpec])

  scenario("Initial sync") {
    (msgs should have size 1).soon
  }

  scenario("Init other clients") {
    friendClient ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    friendClient ? AwaitSyncCompleted should eventually(be(Successful))
  }

  scenario("Only system message") {
    isLastMessageFromSelfShouldEqual(false)
  }

  scenario("One message from self") {
    conv.sendMessage(new Text("whee"))
    isLastMessageFromSelfShouldEqual(false, true)
  }

  scenario("Only system message (again) after deletion") {
    msgs(1).delete()
    isLastMessageFromSelfShouldEqual(false)
  }

  scenario("Only message from system and friend") {
    friendClient ? SendText(rconv, "meep") should eventually(be(Successful))
    isLastMessageFromSelfShouldEqual(false, false)
  }

  scenario("Last message is from self") {
    conv.sendMessage(new Text("moop"))
    isLastMessageFromSelfShouldEqual(false, false, true)
  }

  scenario("Friend sends after self") {
    friendClient ? SendText(rconv, "such texting, much wow") should eventually(be(Successful))
    isLastMessageFromSelfShouldEqual(false, false, true, false)
  }

  scenario("Update last message from self") {
    msgs(2).update(new Text("mööp"))
    isLastMessageFromSelfShouldEqual(false, false, true, false)
  }

  scenario("Last message is from self (again)") {
    conv.sendMessage(new Text("multipas"))
    isLastMessageFromSelfShouldEqual(false, false, false, false, true)
  }

  scenario("Edit last message") {
    msgs(4).update(new Text("MultiPass"))
    isLastMessageFromSelfShouldEqual(false, false, false, false, true)
  }

  scenario("Recall last message") {
    msgs(4).recall()
    isLastMessageFromSelfShouldEqual(false, false, true, false)
  }

  private def isLastMessageFromSelfShouldEqual(flags: Boolean*): Unit = soon {
    msgs should have size flags.size

    forAsLongAs(250.millis) {
      forAll(msgs zip flags) { case (msg, flag) =>
        msg.isLastMessageFromSelf shouldBe flag
      }
    }
  }
}
