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

import com.waz.api.{CauseForCallStateEvent, ProvisionedApiSpec}
import com.waz.model.CallDeviceState
import com.waz.model.ConversationData.ConversationType
import com.waz.testutils.Implicits._
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class VoiceChannelClientSpec extends FeatureSpec with Matchers with ProvisionedApiSpec {

   override val provisionFile = "/two_users_connected.json"

  implicit val timeout = 10.seconds: Timeout

  lazy val conversations = api.getConversations

  lazy val client = zmessaging.voiceClient

  scenario("Load call state") {
     withDelay(conversations.size should be > 0)

     conversations.filter(_.getType == ConversationType.OneToOne) foreach { conv =>
       val result = Await.result(client.loadCallState(conv.data.remoteId), 10.seconds)
       result should be('right)

       info(s"got event: $result")
     }
  }

  scenario("create a call") {
    val conv = conversations.find(_.getType == ConversationType.OneToOne).get

    val result = Await.result(client.updateSelfCallState(conv.data.remoteId, CallDeviceState(joined = true, props = Set.empty), CauseForCallStateEvent.REQUESTED), 10.seconds)
    result should be('right)
    info(s"got result: $result")
    awaitUi(1.second)
  }

  scenario("close call") { // yes, that's right, I've been itching to name a test like that...
    val conv = conversations.find(_.getType == ConversationType.OneToOne).get

    val result = Await.result(client.updateSelfCallState(conv.data.remoteId, CallDeviceState(joined = false, props = Set.empty), CauseForCallStateEvent.REQUESTED), 10.seconds)
    result should be('right)
    info(s"got result: $result")
  }
}
