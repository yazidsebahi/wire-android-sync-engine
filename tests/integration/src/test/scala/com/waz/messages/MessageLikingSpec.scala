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
import com.waz.model.Liking.Action._
import com.waz.model._
import com.waz.provision.ActorMessage.{AwaitSyncCompleted, Login, SetMessageLiking, Successful}
import com.waz.service.BackendConfig
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

class MessageLikingSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val otherUser = provisionedUserId("auto2")
  lazy val conv = conversations.get(0)
  lazy val msgs = conv.getMessages

  lazy val otherUserClient = registerDevice(logTagFor[MessageLikingSpec])

  scenario("Initial sync") {
    withDelay(msgs should have size 1)
  }

  scenario("Init other client") {
    otherUserClient ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    otherUserClient ? AwaitSyncCompleted should eventually(be(Successful))
  }

  scenario("This and other user like and unlike a message") {
    conv.sendMessage(new Text("meep"))
    withDelay(msgs should have size 2)
    val message = msgs.get(1)

    message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes).soon

    otherUserDoes(Like, message)
    message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser)).soon

    message.like()
    message should (beLiked and beLikedByThisUser and haveLikesFrom(otherUser, self)).soon

    otherUserDoes(Unlike, message)
    message should (beLiked and beLikedByThisUser and haveLikesFrom(self)).soon

    otherUserDoes(Like, message)
    message should (beLiked and beLikedByThisUser and haveLikesFrom(self, otherUser)).soon

    message.unlike()
    message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser)).soon

    message.like()
    message should (beLiked and beLikedByThisUser and haveLikesFrom(otherUser, self)).soon

    message.unlike()
    message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser)).soon

    otherUserDoes(Unlike, message)
    message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes).soon
  }

  def otherUserDoes(action: Liking.Action, msg: Message) = otherUserClient ? SetMessageLiking(conv.data.remoteId, msg.data.id, action) should eventually(be(Successful))
}
