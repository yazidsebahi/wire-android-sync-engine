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
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.MessageContent._
import com.waz.api._
import com.waz.model.Liking.Action._
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.provision.ActorMessage.{AwaitSyncCompleted, Login, SetMessageReaction, Successful}
import com.waz.service.{UserModule, ZMessaging, ZMessagingFactory}
import com.waz.sync.client.MessagesClient
import com.waz.sync.client.MessagesClient.OtrMessage
import com.waz.sync.client.OtrClient.MessageResponse
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils.TestApplication.notificationsSpy
import com.waz.testutils.{TestApplication, UpdateSpy}
import com.waz.threading.CancellableFuture.delay
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.znet.ZNetClient._
import org.robolectric.annotation.Config
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.concurrent.Promise
import scala.concurrent.duration._

@Config(application = classOf[TestApplication])
class MessageReactionsSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with BeforeAndAfter with ProvisionedApiSpec with ThreadActorSpec { test =>

  scenario("Initial sync") {
    (msgs should have size 1).soon
  }

  scenario("Init other client") {
    otherUserClient ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    otherUserClient ? AwaitSyncCompleted should eventually(be(Successful))
  }

  scenario("This and other user like and unlike a message") {
    val (n, message) = addMessage("meep")

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
    val (n, message) = addMessage("moop")

    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon

    api.onPause()
    (zmessaging.lifecycle.isUiActive shouldBe false).soon

    notificationsSpy.gcms = Nil
    otherUserDoes(Like, message)
    (notificationsSpy.gcms should have size 1).soon
    (notificationsSpy.gcms.head should have size 1).soon

    otherUserDoes(Unlike, message)
    (notificationsSpy.gcms should have size 2).soon
    (notificationsSpy.gcms(1) shouldBe empty).soon

    api.onResume()
    (zmessaging.lifecycle.isUiActive shouldBe true).soon
  }

  scenario("Liked message is edited") {
    val (n, message) = addMessage("whee")

    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon
    otherUserDoes(Like, message)
    (message should (beLiked and not(beLikedByThisUser) and haveLikesFrom(otherUser))).soon

    message.update(new Text("woo"))
    (message.isDeleted shouldBe true).soon
    (message should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)).soon
    soon {
      val editedMessage = msgs.get(n)
      editedMessage.isEdited shouldBe true
      editedMessage should (not(beLiked) and not(beLikedByThisUser) and notHaveLikes)
    }
  }

  scenario("Rapidly changing one's reaction") {
    val (n, message) = addMessage("amazeballs")
    soon(returning(UpdateSpy(message))(spy => forAsLongAs(2.seconds)(spy.numberOfTimesCalled shouldEqual 0)))
    val gate = Promise[Unit]()
    Serialized('PostReaction)(gate.future.lift)
    delayMessages = true

    val spy = UpdateSpy(message)
    3.times {
      debug("like")
      message.like()
      (message should beLikedByThisUser).soon
      debug("unlike")
      message.unlike()
      (message should not(beLikedByThisUser)).soon
    }
    debug("last like")
    message.like()
    (message should beLikedByThisUser).soon

    (forAsLongAs(3.seconds)(spy.numberOfTimesCalled shouldEqual 7)).soon

    gate.success(())

    (forAsLongAs(3.seconds)(spy.numberOfTimesCalled shouldEqual 7)).soon
  }

  before {
    delayMessages = false
  }

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = provisionedUserId("auto1")
  lazy val otherUser = provisionedUserId("auto2")
  lazy val conv = conversations.get(0)
  lazy val msgs = conv.getMessages

  lazy val otherUserClient = registerDevice(logTagFor[MessageReactionsSpec])

  def addMessage(text: String): (Int, Message) = {
    val n = msgs.size
    conv.sendMessage(new Text("whee"))
    (msgs should have size (n + 1)).soon
    (n, msgs.get(n))
  }

  def otherUserDoes(action: Liking.Action, msg: Message) = otherUserClient ? SetMessageReaction(conv.data.remoteId, msg.data.id, action) should eventually(be(Successful))

  @volatile var delayMessages = false

  override lazy val zmessagingFactory: ZMessagingFactory = new ZMessagingFactory(globalModule) {
    override def zmessaging(clientId: ClientId, user: UserModule): ZMessaging = new ApiZMessaging(clientId, user) {
      override lazy val messagesClient = new MessagesClient(netClient) {
        override def postMessage(conv: RConvId, content: OtrMessage, ignoreMissing: Boolean, receivers: Option[Set[UserId]] = None): ErrorOrResponse[MessageResponse] =
          if (! delayMessages) super.postMessage(conv, content, ignoreMissing, receivers)
          else Serialized('PostReaction)(delay(555.millis).flatMap(_ => super.postMessage(conv, content, ignoreMissing, receivers))(Threading.Background))
      }
    }
  }
}
