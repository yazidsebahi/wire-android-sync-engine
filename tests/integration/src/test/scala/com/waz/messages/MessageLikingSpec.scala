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
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils.TestApplication
import com.waz.testutils.TestApplication.notificationsSpy
import org.robolectric.annotation.Config
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.collection.JavaConverters._

@Config(application = classOf[TestApplication])
class MessageLikingSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val otherUser = provisionedUserId("auto2")
  lazy val conv = conversations.get(0)
  lazy val msgs = conv.getMessages

  lazy val otherUserClient = registerDevice(logTagFor[MessageLikingSpec])

  scenario("Initial sync") {
    (msgs should have size 1).soon
  }

  scenario("Init other client") {
    otherUserClient ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    otherUserClient ? AwaitSyncCompleted should eventually(be(Successful))
  }

  scenario("This and other user like and unlike a message") {
    conv.sendMessage(new Text("meep"))
    withDelay(msgs should have size 2)
    val message = msgs.get(1)

    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon

    otherUserDoes(Like, message)
    (message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser))).soon

    message.like()
    (message should (beLiked and beLikedByThisUser and haveLikesFrom(otherUser, self))).soon

    otherUserDoes(Unlike, message)
    (message should (beLiked and beLikedByThisUser and haveLikesFrom(self))).soon

    otherUserDoes(Like, message)
    (message should (beLiked and beLikedByThisUser and haveLikesFrom(self, otherUser))).soon

    message.unlike()
    (message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser))).soon

    message.like()
    (message should (beLiked and beLikedByThisUser and haveLikesFrom(otherUser, self))).soon

    message.unlike()
    (message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser))).soon

    otherUserDoes(Unlike, message)
    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon
  }

  scenario("User A receives a notification if user B likes a message (written by A)") {
    conv.sendMessage(new Text("moop"))
    (msgs should have size 3).soon
    val message = msgs.get(2)

    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon

    api.onPause()
    (zmessaging.lifecycle.isUiActive shouldBe false).soon

    notificationsSpy.gcms = Nil
    otherUserDoes(Like, message)
    (notificationsSpy.gcms should have size 1).soon
    (notificationsFromUpdate(0) should have size 1).soon

    otherUserDoes(Unlike, message)
    (notificationsSpy.gcms should have size 2).soon
    (notificationsFromUpdate(1) shouldBe empty).soon

    api.onResume()
    (zmessaging.lifecycle.isUiActive shouldBe true).soon
  }

  scenario("Liked message is edited") {
    conv.sendMessage(new Text("whee"))
    (msgs should have size 4).soon
    val message = msgs.get(3)

    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon
    otherUserDoes(Like, message)
    (message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser))).soon

    message.update(new Text("woo"))
    (message.isDeleted shouldBe true).soon
    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon
    soon {
      val editedMessage = msgs.get(3)
      editedMessage.isEdited shouldBe true
      editedMessage should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)
    }
  }

  def notificationsFromUpdate(n: Int) = notificationsSpy.gcms(n).getNotifications.asScala

  def otherUserDoes(action: Liking.Action, msg: Message) = otherUserClient ? SetMessageLiking(conv.data.remoteId, msg.data.id, action) should eventually(be(Successful))
}
