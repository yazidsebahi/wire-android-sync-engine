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
import android.graphics.BitmapFactory
import com.waz._
import com.waz.api.Message.Status
import com.waz.api.MessageContent.Image
import com.waz.api.{Message, MessageContent}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.StorageModule
import com.waz.testutils.Matchers._
import com.waz.testutils._
import com.waz.threading.Threading
import com.waz.utils.IoUtils.{toByteArray, withResource}
import com.waz.utils._
import com.waz.utils.events.EventContext
import org.robolectric.shadows.ShadowLog
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class MessageSendingSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit lazy val dispatcher = Threading.Background

  lazy val selfUser = UserData("self user")
  lazy val user1 = UserData("user 1")

  lazy val conv = ConversationData(ConvId(), RConvId(), Some("convName"), selfUser.id, ConversationType.Group)

  implicit def db: SQLiteDatabase = service.db.dbHelper.getWritableDatabase

  var messageSync = None: Option[MessageId]
  var assetSync = None: Option[AssetId]
  var lastReadSync = None: Option[(ConvId, Instant)]

  lazy val global = new MockGlobalModule {
    override lazy val factory: MockZMessagingFactory = new MockZMessagingFactory(this) {
      override def baseStorage(accountId: AccountId): StorageModule = new StorageModule(context, accountId, Random.nextInt().toHexString)
    }
  }

  lazy val service = new MockZMessaging(new MockAccountService(new MockAccounts(global)), selfUserId = selfUser.id) {

    override lazy val sync = new EmptySyncService {
      override def postMessage(id: MessageId, conv: ConvId) = {
        messageSync = Some(id)
        super.postMessage(id, conv)
      }

      override def postLastRead(id: ConvId, time: Instant) = {
        test.lastReadSync = Some((id, time))
        super.postLastRead(id, time)
      }
    }

    insertUsers(Seq(selfUser, user1))
  }

  lazy val ui = new MockUiModule(service)

  before {
    messageSync = None
    assetSync = None
    lastReadSync = None
    service.insertConv(conv)
  }

  after {
    ShadowLog.stream = null
    service.deleteAllConvs()
  }
  
  def listMessages = service.listMessages(conv.id)

  def getMessage(id: MessageId) = Await.result(service.messagesStorage.getMessage(id), 1.second)
  
  def sendMessage[A](content: MessageContent[A]): MessageData = {
    val count = listMessages.size
    val msg = Await.result(service.convsUi.sendMessage(conv.id, content), 1.second)
    listMessages should have size (count + 1)
    msg.get
  }

  feature("Text messages") {
    scenario("Add text message") {
      val msg = sendMessage(new MessageContent.Text("test"))
      msg.contentString shouldEqual "test"
      messageSync shouldEqual Some(msg.id)
      msg.state shouldEqual Message.Status.PENDING

      withEvent(service.messagesStorage.messageChanged) { case _ => true } {
        service.dispatchEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, EventId("2.800122000a5b8a02"), new Date(), selfUser.id, "test"))
      }
      getMessage(msg.id).map(_.state) shouldEqual Some(Status.SENT)
      getMessage(msg.id).map(_.source) shouldEqual Some(EventId("2.800122000a5b8a02"))
    }
  }

  feature("Image messages") {
    def imageStream = getClass.getResourceAsStream("/images/penguin.png")

    scenario("send image and access local message") {
      val bitmap = withResource(imageStream)(BitmapFactory.decodeStream)
      info(s"bitmap size: (${bitmap.getWidth}, ${bitmap.getHeight})")

      val msg = sendMessage(new Image(ui.images.getOrCreateImageAssetFrom(toByteArray(imageStream))))
      val asset = Await.result(service.assetsStorage.getImageAsset(msg.assetId), 5.seconds).get

      asset.versions should have size 2
      val im = asset.versions(1)
      im.width shouldEqual im.origWidth
      im.width shouldEqual bitmap.getWidth
      im.height shouldEqual bitmap.getHeight

      Await.result(service.imageLoader.loadRawImageData(im, conv.remoteId), 5.seconds) should be('defined)

      val entry = service.lastMessage(conv.id)
      entry.map(_.id) shouldEqual Some(msg.id)
      entry.map(_.msgType) shouldEqual Some(Message.Type.ASSET)
      entry.flatMap(_.imageDimensions) shouldEqual Some(Dim2(asset.width, asset.height))
    }
  }

  feature("Ordering") {
    import testutils.withUpdate


    scenario("Notify messages list if sent message sequence id is changed") {
      val msgs = service.messagesStorage.getEntries(conv.id)
      val msg = withUpdate(msgs) {
        sendMessage(new MessageContent.Text("test"))
      }
      val eventId = EventId(msg.source.sequence + 1, "800122000a5b8a02")
      withUpdate(msgs) {
        service.convEvents.handlePostConversationEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, eventId, new Date(), selfUser.id, "test"))
      }
      val e = getMessage(msg.id).get
      e.state shouldEqual Message.Status.SENT
      e.source shouldEqual eventId
      getMessage(msg.id).map(_.source) shouldEqual Some(eventId)
    }

    scenario("Don't update messages list if time is unchanged") {
      val msgs = service.messagesStorage.getEntries(conv.id)
      @volatile var updateCount = 0
      msgs { _ => updateCount += 1 } (EventContext.Global)

      val msg = withUpdate(msgs) {
        sendMessage(new MessageContent.Text("test"))
      }
      updateCount = 0

      val eventId = EventId(msg.source.sequence, "800122000a5b81234")
      service.dispatchEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, eventId, msg.time.javaDate, selfUser.id, "test"))
      awaitUi(200.millis)
      val e = getMessage(msg.id).get
      e.state shouldEqual Message.Status.SENT
      e.source shouldEqual eventId
      getMessage(msg.id).map(_.source) shouldEqual Some(eventId)

      updateCount shouldEqual 0
    }

    scenario("Send multiple messages and maintain local ordering") {
      val msgs = service.messagesStorage.getEntries(conv.id)
      val msg = withUpdate(msgs) { sendMessage(new MessageContent.Text("test")) }
      val msg1 = withUpdate(msgs) { sendMessage(new MessageContent.Text("test1")) }
      val msg2 = withUpdate(msgs) { sendMessage(new MessageContent.Text("test2")) }
      val msg3 = withUpdate(msgs) { sendMessage(new MessageContent.Text("test3")) }

      val time = Instant.now()
      withUpdate(msgs) {
        service.messagesSync.messageSent(conv.id, msg, time + 1.second)
      }

      val ms = listMessages
      ms.drop(ms.size - 4).map(_.contentString) shouldEqual Seq("test", "test1", "test2", "test3")

      withUpdate(msgs) {
        service.messagesSync.messageSent(conv.id, msg1, time + 2.seconds)
      }

      val ms1 = listMessages
      withClue(ms1.map(m => (m.contentString, m.time))) {
        ms1.drop(ms1.size - 4).map(_.contentString) shouldEqual Seq("test", "test1", "test2", "test3")
      }
    }
  }

  feature("Last read") {
    scenario("Sent message should be marked as read") {
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), selfUser.id, "test 1"))
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(2), new Date(), selfUser.id, "test 2"))
      awaitUi(100.millis)

      val msg = sendMessage(new MessageContent.Text("test"))
      msg.contentString shouldEqual "test"
      messageSync shouldEqual Some(msg.id)
      msg.state shouldEqual Message.Status.PENDING
      Await.result(service.convsStorage.get(conv.id), 1.second).map(_.lastRead) shouldEqual Some(msg.time)
    }

    scenario("Mark posted message read once it's synced with same id sequence") {
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), selfUser.id, "test 1"))
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(2), new Date(), selfUser.id, "test 2"))
      awaitUi(100.millis)

      val msg = sendMessage(new MessageContent.Text("test"))
      Await.result(service.convsStorage.get(conv.id), 1.second).map(_.lastRead) shouldEqual Some(msg.time)

      lastReadSync shouldEqual None
      val eventId = EventId(msg.source.sequence ,"800122000a5b8a02")
      val time = new Date()
      withEvent(service.messagesStorage.messageChanged) { case _ => true } {
        service.convEvents.handlePostConversationEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, eventId, time, selfUser.id, "test"))
      }
      getMessage(msg.id).get.state shouldEqual Message.Status.SENT
    }

    scenario("Mark posted message read once it's synced and unreadCount = 0") {
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), selfUser.id, "test 1"))
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(2), new Date(), selfUser.id, "test 2"))
      awaitUi(100.millis)

      val msg = sendMessage(new MessageContent.Text("test"))
      awaitUi(1.second)
      Await.result(service.convsStorage.update(conv.id, _.copy(unreadCount = 0)), 1.second)

      val eventId = EventId(msg.source.sequence + 10, "800122000a5b8a02")
      val time = new Date
      withEvent(service.messagesStorage.messageChanged) { case _ => true } {
        service.convEvents.handlePostConversationEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, eventId, time, selfUser.id, "test"))
      }
      getMessage(msg.id).get.state shouldEqual Message.Status.SENT
      Await.result(service.convsStorage.get(conv.id), 1.second).map(_.lastRead) shouldEqual Some(time.instant)
      lastReadSync shouldEqual Some((conv.id, time.instant))
    }

    scenario("Change last read even when unreadCount > 0") {
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), selfUser.id, "test 1"))
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(2), new Date(), selfUser.id, "test 2"))
      awaitUi(100.millis)

      val msg = sendMessage(new MessageContent.Text("test"))
      Await.result(service.convsStorage.update(conv.id, _.copy(unreadCount = 1)), 1.second)

      val eventId = EventId(msg.source.sequence, "800122000a5b8a02")
      val time = new Date
      withEvent(service.messagesStorage.messageChanged) { case _ => true } {
        service.convEvents.handlePostConversationEvent(MessageAddEvent(Uid(msg.id.str), conv.remoteId, eventId, time, selfUser.id, "test"))
      }
      getMessage(msg.id).get.state shouldEqual Message.Status.SENT
      Await.result(service.convsStorage.get(conv.id), 1.second).map(_.lastRead) shouldEqual Some(time.instant)
      lastReadSync shouldEqual None
    }
  }
}
