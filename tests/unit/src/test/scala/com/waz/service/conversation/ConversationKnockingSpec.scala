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
package com.waz.service.conversation

import java.util.Date

import android.database.sqlite.SQLiteDatabase
import com.waz.api.Message
import com.waz.content.GlobalDatabase
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Knock
import com.waz.model._
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.Threading
import com.waz.utils.events.EventContext.Implicits.global
import com.waz.utils._
import com.waz.testutils.Matchers._
import com.waz.{RobolectricUtils, testutils}
import org.robolectric.Robolectric
import org.scalatest.matchers.Matcher
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._

class ConversationKnockingSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit lazy val dispatcher = Threading.Background
  lazy val globalStorage = new GlobalDatabase(Robolectric.application)

  lazy val selfUser = UserData("self user")
  lazy val user1 = UserData("user 1")

  lazy val conv = ConversationData(ConvId(), RConvId(), Some("convName"), selfUser.id, ConversationType.Group)

  implicit def db: SQLiteDatabase = service.db.dbHelper.getWritableDatabase

  var messageSync = None: Option[MessageId]

  lazy val service = new MockZMessaging(selfUserId = selfUser.id) {
    override lazy val sync = new EmptySyncService {
      override def postMessage(id: MessageId, conv: ConvId, time: Instant) = {
        messageSync = Some(id)
        super.postMessage(id, conv, time)
      }
    }

    insertUsers(Seq(selfUser, user1))
    insertConv(conv)

    lifecycle.acquireUi()
  }

  before {
    messageSync = None
  }

  after {
    service.messagesStorage.deleteAll(conv.id).await()
  }
  
  def listMessages(conv: ConvId) = service.listMessages(conv)
  def lastMessage(conv: ConvId) = service.lastMessage(conv)
  def lastLocalMessage(conv: ConvId, tpe: Message.Type) = Await.result(service.messagesStorage.lastLocalMessage(conv, tpe), 1.second)

  def updateMessage(id: MessageId)(updater: MessageData => MessageData) = 
    Await.result(service.messagesContent.updateMessage(id)(updater), 1.second)
  
  def addMessage(msg: MessageData) = Await.result(service.messagesStorage.addMessage(msg), 1.second)
  
  feature("Send knocks") {

    scenario("knock once") {
      val msg = knock()
      msg.msgType shouldEqual Message.Type.KNOCK
      msg.hotKnock shouldEqual false
      msg.convId shouldEqual conv.id
      msg.localTime.toEpochMilli should be (System.currentTimeMillis +- 150)

      lastMessage(conv.id) shouldEqual Some(msg.copy(firstMessage = true))
      messageSync shouldEqual Some(msg.id)
    }

    scenario("update knock on event") {
      val msg = knock()
      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg.id.uid, Knock(false))))
      Thread.sleep(250)

      val msg1 = lastMessage(conv.id)
      msg1 should be (defined)
      msg1 foreach { msg1 =>
        msg1 should haveTypeKnock
        msg1 should haveSameId(msg.id)
        msg1 should not(beAHotKnock)
        msg1 should haveConvId
        msg1 should not(beCurrent)
      }

      lastMessage(conv.id) shouldEqual msg1
      messageSync shouldEqual msg1.map(_.id)
    }

    scenario("knock twice with a delay") {
      val msg = knock()

      updateMessage(msg.id)(_.copy(localTime = Instant.now - ConversationsService.KnockTimeout - 10.millis)) // change local time to simulate delay
      messageSync = None

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)
    }

    scenario("hot knock") {
      val msg = knock()

      updateMessage(msg.id)(_.copy(localTime = Instant.now - 10.seconds)) // change local time to simulate delay
      messageSync = None

      var messageNotified = false
      service.messagesStorage.messageChanged { ms => if (ms.exists(_.id == msg.id)) messageNotified = true }

      val msg1 = knock()
      msg1 should beAHotKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)

      withDelay { messageNotified shouldEqual true }
    }

    scenario("send hot knock and handle events") {
      val msg = knock()
      val msg1 = knock()

      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg.id.uid, Knock(false))))
      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg1.id.uid, Knock(true))))
      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg1.id.uid, Knock(true))))
      Thread.sleep(250)

      listMessages(conv.id) should have size 1
      val last = lastMessage(conv.id)
      last.map(_.hotKnock) shouldEqual Some(true)
      messageSync shouldEqual Some(msg1.id)
    }

    scenario("ignore knocks after hot knock") {
      knock()
      knock()
      failToKnock()
      failToKnock()

      listMessages(conv.id) should have size 1
    }

    scenario("hot knock, delay, knock again") {
      knock()
      val msg = knock()

      updateMessage(msg.id)(_.copy(localTime = Instant.now - ConversationsService.KnockTimeout - 10.millis)) // change local time to simulate delay
      messageSync = None

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)

      listMessages(conv.id) should have size 2
    }

    scenario("hot knock, incoming message, knock again") {
      knock()
      val msg = knock()

      val msgId = MessageId()
      addMessage(MessageData(msgId, conv.id, Message.Type.TEXT, user1.id, MessageData.textContent("msg"), time = Instant.now))

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)

      listMessages(conv.id) should have size 3
    }

    scenario("knock, incoming message, knock") {
      val msg = knock()
      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg.id.uid, Knock(false))))
      Thread.sleep(250)

      val msgId = MessageId()
      addMessage(MessageData(msgId, conv.id, Message.Type.TEXT, user1.id, MessageData.textContent("msg"), time = Instant.now))

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)

      listMessages(conv.id) should have size 3
    }

    scenario("hot knock, incoming knock, knock again") {
      knock()
      val msg = knock()

      val msgId = MessageId()
      addMessage(MessageData(msgId, conv.id, Message.Type.KNOCK, user1.id, MessageData.textContent("msg"), time = Instant.now))

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      lastMessage(conv.id) shouldEqual Some(msg1)
      messageSync shouldEqual Some(msg1.id)

      listMessages(conv.id) should have size 3
    }

    scenario("knock, incoming knock, knock") {
      val msg = knock()
      service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, selfUser.id, GenericMessage(msg.id.uid, Knock(false))))
      Thread.sleep(250)


      val msgId = MessageId()
      addMessage(MessageData(msgId, conv.id, Message.Type.KNOCK, user1.id, MessageData.textContent("msg"), time = Instant.now))

      val msg1 = knock()
      msg1 should beAKnock(otherId = msg.id)

      messageSync shouldEqual Some(msg1.id)

      listMessages(conv.id) should have size 3
    }
  }

  def knock(): MessageData = {
    returning(Await.result(service.convsUi.knock(conv.id), 1.second)) { _ should be ('defined) } .get
  }

  def failToKnock(): Unit = Await.result(service.convsUi.knock(conv.id), 1.second) shouldEqual None

  def beAKnock(otherId: MessageId): Matcher[MessageData] =
    haveTypeKnock and not(beAHotKnock) and not(haveSameId(otherId)) and haveConvId and beCurrent
  def beAHotKnock(otherId: MessageId): Matcher[MessageData] =
    haveTypeKnock and beAHotKnock and haveSameId(otherId) and haveConvId and beCurrent

  def haveTypeKnock: Matcher[MessageData] = be(Message.Type.KNOCK) compose (_.msgType)
  def beAHotKnock: Matcher[MessageData] = be(true) compose (_.hotKnock)
  def haveConvId: Matcher[MessageData] = be(conv.id) compose (_.convId)
  def beCurrent: Matcher[MessageData] = be(System.currentTimeMillis() +- 100) compose (_.localTime.toEpochMilli)
  def haveSameId(id: MessageId): Matcher[MessageData] = be(id) compose (_.id)

  feature("receive incoming knocks") {

    scenario("receive single knock") {
      val incoming = service.messagesStorage.getIncomingMessages
      testutils.withUpdate(incoming) {
        service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(Uid(), Knock(false))).withCurrentLocalTime())
      }

      withDelay {
        listMessages(conv.id) should have size 1

        lastMessage(conv.id).map(_.msgType) shouldEqual Some(Message.Type.KNOCK)
        lastMessage(conv.id).map(_.hotKnock) shouldEqual Some(false)
      }
    }

    scenario("receive hotknock") {
      val incoming = service.messagesStorage.getIncomingMessages
      val msgId = MessageId()
      testutils.withUpdate(incoming) {
        service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(msgId.uid, Knock(false))).withCurrentLocalTime())
      }

      testutils.withUpdate(incoming) {
        service.dispatchEvent(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(msgId.uid, Knock(true))).withCurrentLocalTime())
      }

      withDelay {
        listMessages(conv.id) should have size 1

        lastMessage(conv.id).map(_.msgType) shouldEqual Some(Message.Type.KNOCK)
        lastMessage(conv.id).map(_.hotKnock) shouldEqual Some(true)
      }
    }
  }
}
