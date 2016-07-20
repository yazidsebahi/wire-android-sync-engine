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
package com.waz.service

import java.util.Date

import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog.LogTag
import com.waz.api.Message
import com.waz.api.Message.Status
import com.waz.api.impl.ErrorResponse.internalError
import com.waz.content.Mime
import com.waz.model.AssetStatus.{UploadCancelled, UploadDone, UploadInProgress}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Asset.Original
import com.waz.model.GenericContent.{Asset, ImageAsset, Knock}
import com.waz.model.GenericMessage.TextMessage
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.testutils.Matchers._
import com.waz.testutils.Implicits._
import com.waz.testutils._
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.crypto.AESUtils
import com.waz.{api, _}
import org.json.JSONObject
import org.robolectric.shadows.ShadowLog
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

class MessagesServiceSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfter with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig { test =>
  import Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global
  implicit val tag: LogTag = "MessagesServiceSpec"

  implicit def db: SQLiteDatabase = service.db.dbHelper.getWritableDatabase

  var online = true

  lazy val selfUserId = UserId()

  lazy val service = new MockZMessaging(selfUserId = selfUserId) {

    override def network: NetworkModeService = new NetworkModeService(context) {
      override def isOnlineMode: Boolean = online
      override def isOfflineMode: Boolean = !online
    }

    usersStorage.addOrOverwrite(UserData(selfUserId, "selfName"))
  }

  lazy val ui = new MockUiModule(service)
  
  val timeout = 5.seconds

  import service._

  lazy val messages = service.messages

  def addGroup() = insertConv(ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group))
  
  before {
    online = true
  }

  after {
    ShadowLog.stream = null
  }

  def events(resource: String) = {
    val json = readResource(resource)
    JsonDecoder.array[Event](json.getJSONArray("events")).collect { case me: MessageEvent => me }
  }

  def processEventsFromJson(resource: String) = {
    events(resource).map(_.convId).toSet foreach { (convId: RConvId) =>
      Await.result(service.convsStorage.insert(ConversationData(ConvId(convId.str), convId, None, UserId(), ConversationType.Group)), timeout)
    }
    service.dispatchEvents(events(resource).map (_.withCurrentLocalTime()))
  }

  feature("Push events processing") {

    lazy val user2 = insertUser(UserData("user2"))
    lazy val conv = {
      val id = ConvId("b53ae017-b002-4d8c-8540-4887b58d47c7")
      returning(insertConv(ConversationData(id, RConvId(id.str), None, UserId(), ConversationType.Group))) { c =>
        addMember(id, user2.id)
      }
    }
    lazy val convId = conv.id

    scenario("Process add message event") {
      val event = MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), UserId(), "message").withCurrentLocalTime()

      Await.ready(messages.processEvents(conv, Seq(event)), timeout)

      withDelay {
        listMessages(convId) shouldEqual List(MessageData(MessageId(event.id), convId, event.eventId, Message.Type.TEXT, event.from, MessageData.textContent("message"), time = event.time.instant, localTime = event.localTime.instant, state = Status.SENT, firstMessage = true))
      }
    }

    scenario("Process add asset events") {
      val assetId = AssetId()
      val userId = UserId()
      val events = Seq(
        GenericAssetEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), ImageAsset("medium", 100, 100, 100, 100, "image/jpg", 0, None, None)), RAssetDataId(), None).withCurrentLocalTime(),
        GenericAssetEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), ImageAsset("preview", 10, 10, 100, 100, "image/jpg", 0, None, None)), RAssetDataId(), None).withCurrentLocalTime()
      )

      Await.ready(messages.processEvents(conv, events), timeout)

      val msgs = listMessages(convId)
      msgs should have size 2
    }

    scenario("Keep timestamp of a first event for assets") {
      val assetId = AssetId()
      val userId = UserId()
      val events = Seq(
        GenericAssetEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), ImageAsset("medium", 100, 100, 100, 100, "image/jpg", 0, None, None)), RAssetDataId(), None).withCurrentLocalTime()
      )
      Await.ready(messages.processEvents(conv, events), timeout)

      val events1 = Seq(
        GenericAssetEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), ImageAsset("preview", 10, 10, 100, 100, "image/jpg", 0, None, None)), RAssetDataId(), None).withCurrentLocalTime()
      )
      Await.ready(messages.processEvents(conv, events1), timeout)


      val msgs = listMessages(convId)
      msgs should have size 3
      msgs.last.time shouldEqual events.head.time.instant
    }

    scenario("Delete placeholder message for cancelled asset") {
      val assetId = AssetId()
      val userId = UserId()
      val events = Seq(
        GenericMessageEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), Asset(Original(Mime("text/txt"), 100, None), UploadInProgress))).withCurrentLocalTime()
      )

      Await.ready(messages.processEvents(conv, events), timeout)

      val msgs = listMessages(convId)
      msgs should have size 4
      val msg = msgs.last
      msg.msgType shouldEqual Message.Type.ANY_ASSET

      Await.ready(messages.processEvents(conv, Seq(
        GenericMessageEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), Asset(UploadCancelled))).withCurrentLocalTime()
      )), timeout)

      listMessages(convId) should have size 3
      service.assetsStorage.get(assetId).await() shouldEqual None
    }

    scenario("Process event for previously deleted message") {
      val assetId = AssetId()
      val userId = UserId()
      val events = Seq(
        GenericMessageEvent(Uid(), conv.remoteId, new Date(), userId, GenericMessage(Uid(assetId.str), Asset(Original(Mime("text/txt"), 100, None), UploadInProgress))).withCurrentLocalTime()
      )

      messages.processEvents(conv, events).await()

      val msgs = listMessages(convId)
      msgs should have size 4
      val msg = msgs.last

      service.messagesContent.deleteOnUserRequest(Seq(msg.id)).await()
      listMessages(convId) should have size 3

      messages.processEvents(conv, events).await()

      listMessages(convId) should have size 3
    }

    scenario("Process all conversation events in order") {
      delMessages(convId)
      processEventsFromJson("/conv/conv_events5.json")

      val messages = listMessages(convId)

      info(s"got ${messages.length} messages")
      messages should have size 59
    }

    scenario("Process all events multiple times") {
      delMessages(convId)
      awaitUi(1.second)
      processEventsFromJson("/conv/conv_events5.json")

      val messages = listMessages(convId)
      messages should have size 59

      // processing same events multiple times should not affect the messages list at all
      processEventsFromJson("/conv/conv_events5.json")
      processEventsFromJson("/conv/conv_events5.json")

      val current = listMessages(convId)
      current.zip(messages) foreach {
        case (c, m) => c shouldEqual m
      }
      current should have size 59
      current.mkString(",") shouldEqual messages.mkString(",")
      current shouldEqual messages
    }

    scenario("Process missed call event") {
      delMessages(convId)
      val event = VoiceChannelDeactivateEvent(Uid(), RConvId(convId.str), EventId(1), new Date(), UserId(), Some("missed")).withCurrentLocalTime()

      Await.ready(messages.processEvents(conv, Seq(event)), timeout)

      listMessages(convId) shouldEqual List(MessageData(MessageId(event.id), convId, event.eventId, Message.Type.MISSED_CALL, event.from, Nil, time = event.time.instant, localTime = event.localTime.instant, state = Status.SENT))
    }

    lazy val assetId = AssetId()

    scenario("Receive initial file asset event") {
      delMessages(convId)
      val msg = GenericMessage(Uid(assetId.str), Asset(Original(Mime("text/txt"), 100, Some("file")), UploadInProgress))
      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, user2.id, msg)

      messages.processEvents(conv, Seq(event)).futureValue
      listMessages(convId) should have size 1
      listMessages(convId).head shouldEqual MessageData(MessageId(assetId.str), convId, event.eventId, Message.Type.ANY_ASSET, event.from, Nil, protos = Seq(msg), time = event.time.instant, localTime = event.localTime.instant, state = Status.SENT)
      service.assetsStorage.get(assetId).futureValue shouldEqual Some(AnyAssetData(assetId, conv.remoteId, Mime("text/txt"), 100, Some("file"), None, None, None, None, AssetStatus.UploadInProgress, event.time.instant))
    }

    scenario("Receive full file asset event") {
      delMessages(convId)
      val dataId = RAssetDataId()
      val key = AESKey()
      val sha = Sha256(AESUtils.randomKey().bytes)
      val msg = GenericMessage(Uid(assetId.str), Asset(Original(Mime("text/txt"), 100, Some("file")), UploadDone(AssetKey(Left(dataId), None, key, sha))))
      val event = GenericAssetEvent(Uid(), conv.remoteId, new Date(), user2.id, msg, dataId, None)

      messages.processEvents(conv, Seq(event)).futureValue
      listMessages(convId) should have size 1
      listMessages(convId).head shouldEqual MessageData(MessageId(assetId.str), convId, event.eventId, Message.Type.ANY_ASSET, event.from, Nil, protos = Seq(msg), time = event.time.instant, localTime = event.localTime.instant, state = Status.SENT)
      service.assetsStorage.get(assetId).futureValue shouldEqual Some(AnyAssetData(assetId, conv.remoteId, Mime("text/txt"), 100, Some("file"), None, None, None, None, AssetStatus.UploadDone(AssetKey(Left(dataId), None, key, sha)), event.time.instant))
    }
  }

  feature("Local messages") {

    scenario("Add local message at the end") {
      val conv = addGroup()
      val event = MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), UserId(), "message")
      Await.result(messages.processEvents(conv, Seq(event.withCurrentLocalTime())), timeout)
      awaitUi(100.millis)
      val msg = Await.result(messages.addTextMessage(conv.id, "local message"), timeout)

      lastMessage(conv.id) shouldEqual Some(msg)
      msg.source shouldEqual EventId.local(2)
      listMessages(conv.id).map(_.contentString) shouldEqual List("message", "local message")
    }

    scenario("Set incremental ids to local messages") {
      val conv = addGroup()
      val msg1 = Await.result(messages.addTextMessage(conv.id, "local message 1"), timeout)
      val msg2 = Await.result(messages.addTextMessage(conv.id, "local message 2"), timeout)
      val msg3 = Await.result(messages.addTextMessage(conv.id, "local message 3"), timeout)

      lastMessage(conv.id) shouldEqual Some(msg3)

      msg1.source should be < msg2.source
      msg2.source should be < msg3.source
    }

    scenario("Move local message up in history when new event is received") {
      val conv = addGroup()
      Await.ready(messages.processEvents(conv, Seq(MessageAddEvent(Uid(), conv.remoteId, EventId(1), new Date(), UserId(), "message 1").withCurrentLocalTime())), timeout)
      Await.ready(messages.addTextMessage(conv.id, "local message"), timeout)
      val time = System.currentTimeMillis()
      Await.ready(messages.processEvents(conv, Seq(MessageAddEvent(Uid(), conv.remoteId, EventId(2), new Date(time + 2), UserId(), "message 2").withCurrentLocalTime())), timeout)
      Await.ready(messages.processEvents(conv, Seq(MessageAddEvent(Uid(), conv.remoteId, EventId(3), new Date(time + 3), UserId(), "message 3").withCurrentLocalTime())), timeout)

      awaitUi(100.millis)
      listMessages(conv.id).map(_.contentString) shouldEqual List("message 1", "local message", "message 2", "message 3")
    }
  }

  feature("Knocks") {

    lazy val selfId = selfUserId

    scenario("Update local knock message on event") {
      val conv = addGroup()
      val msg = Await.result(messages.addKnockMessage(conv.id, selfId), timeout)

      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, selfId, GenericMessage(msg.id, Knock(false)))
      event.localTime = new Date

      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual false
      }
    }

    scenario("Handle two knock events for the same local message (one from push channel, second from /knock resposne)") {

      val conv = addGroup()
      val msg = Await.result(messages.addKnockMessage(conv.id, selfId), timeout)

      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, selfId, GenericMessage(msg.id, Knock(false)))
      event.localTime = new Date

      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))
      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual false
      }
    }

    scenario("Update local knock message on hot knock event") {
      val conv = addGroup()
      val msg = Await.result(messages.addKnockMessage(conv.id, selfId), timeout)

      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, selfId, GenericMessage(msg.id, Knock(false)))
      event.localTime = new Date

      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual false
      }

      val eventId2 = EventId(2)
      val event2 = GenericMessageEvent(Uid(), conv.remoteId, new Date, selfId, GenericMessage(msg.id, Knock(true)))
      event2.localTime = new Date

      messages.processEvents(conv, Seq(event2.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual true
      }
    }

    scenario("update knock to hotknock for incoming events") {
      val conv = addGroup()
      val user = UserId()
      val msgId = MessageId()
      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, user, GenericMessage(msgId, Knock(false)))

      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual false
      }

      val event2 = GenericMessageEvent(Uid(), conv.remoteId, new Date, user, GenericMessage(msgId, Knock(true)))
      event2.localTime = new Date

      messages.processEvents(conv, Seq(event2.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.last.hotKnock shouldEqual true
      }
    }
  }

  feature("Rich media messages") {
    import Message.Part.Type._

    def addMessage(content: String) = {
      val conv = addGroup()
      val eventId = EventId(1)
      val event = MessageAddEvent(Uid(), RConvId(conv.id.str), eventId, new Date, UserId(), content)

      messages.processEvents(conv, Seq(event.withCurrentLocalTime()))

      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        val m = msgs.head
        m.convId shouldEqual conv.id
        m.source shouldEqual eventId
        m
      }
    }

    scenario("Add youtube link") {
      val msg = addMessage("https://www.youtube.com/watch?v=MWdG413nNkI")
      msg.content should have size 1

      msg.content shouldEqual Seq(MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI", syncNeeded = true))
      msg.msgType shouldEqual Message.Type.RICH_MEDIA
    }

    scenario("Add text message with youtube link") {
      val msg = addMessage("Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI")
      msg.msgType shouldEqual Message.Type.RICH_MEDIA
      msg.content shouldEqual Seq(MessageContent(TEXT, "Here is some text."), MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI", syncNeeded = true))
    }

    scenario("Add text interleaved with multiple links") {
      val msg = addMessage("Here is some text. And a SoundCloud link: https://soundcloud.com/meep and http://www.soundcloud.com/mewp - Also https://www.youtube.com/watch?v=MWdG413nNkI more text https://www.youtube.com/watch?v=c0KYU2j0TM4 and even more")
      msg.msgType shouldEqual Message.Type.RICH_MEDIA
      msg.content should contain theSameElementsInOrderAs Seq(
        MessageContent(TEXT, "Here is some text. And a SoundCloud link:"),
        MessageContent(SOUNDCLOUD, "https://soundcloud.com/meep", syncNeeded = true),
        MessageContent(TEXT, "and"),
        MessageContent(SOUNDCLOUD, "http://www.soundcloud.com/mewp", syncNeeded = true),
        MessageContent(TEXT, "- Also"),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI", syncNeeded = true),
        MessageContent(TEXT, "more text"),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=c0KYU2j0TM4", syncNeeded = true),
        MessageContent(TEXT, "and even more")
      )
    }

    scenario("Add local message with link") {
      val conv = ConvId()

      val msg = Await.result(messages.addTextMessage(conv, "Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI"), 5.seconds)
      msg.msgType shouldEqual Message.Type.RICH_MEDIA
      msg.content shouldEqual Seq(MessageContent(TEXT, "Here is some text."), MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"))
    }

    scenario("Notify messages list changed on message content type change") {
      val conv = ConvId()
      Await.result(convsContent.insertConversation(new ConversationData(conv, RConvId(), None, UserId(), ConversationType.Group)), 1.second)
      val msg = Await.result(messages.addTextMessage(conv, "Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI"), 5.seconds)
      msg.msgType shouldEqual Message.Type.RICH_MEDIA
      msg.content shouldEqual Seq(MessageContent(TEXT, "Here is some text."), MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"))
      awaitUi(100.millis)

      @volatile var updateCount = 0
      val msgCursor = messagesStorage.getEntries(conv)
      msgCursor { _ => updateCount += 1 }

      withDelay(msgCursor.currentValue.value.size shouldEqual 1)
      updateCount = 0

      messagesContent.updateMessage(msg.id) { m => m.copy(content = m.content.map(_.copy(tpe = TEXT)))}

      withDelay(updateCount shouldEqual 1)
    }
  }

  feature("Mentions") {

    lazy val conv = addGroup()

    scenario(s"Add message with mentions") {
      val msg = Await.result(messages.addTextMessage(conv.id, "message @selfName", Map(selfUserId -> "selfName")), timeout)

      withDelay {
        val ms = listMessages(conv.id)
        ms should have size 1
        ms.head.content shouldEqual Seq(MessageContent(Message.Part.Type.TEXT, "message @selfName", None, None, None, 0, 0, syncNeeded = false, Map(selfUserId -> "selfName")))
      }
    }

    scenario(s"Handle generic event with mentions") {
      service.dispatch(GenericMessageEvent(Uid(), conv.remoteId, new Date, UserId(), TextMessage("message @selfName 2", Map(selfUserId -> "selfName"))))

      withDelay {
        val ms = listMessages(conv.id)
        ms should have size 2
        ms(1).content shouldEqual Seq(MessageContent(Message.Part.Type.TEXT, "message @selfName 2", None, None, None, 0, 0, syncNeeded = false, Map(selfUserId -> "selfName")))
      }
    }

    scenario("Add message from api request") {
      val msg = Await.result(convsUi.sendMessage(conv.id, new api.MessageContent.Text("message @selfName 3", new com.waz.api.impl.User(selfUserId)(ui))), timeout)

      withDelay {
        val ms = listMessages(conv.id)
        ms should have size 3
        ms(2).content shouldEqual Seq(MessageContent(Message.Part.Type.TEXT, "message @selfName 3", None, None, None, 0, 0, syncNeeded = false, Map(selfUserId -> "selfName")))
      }
    }
  }

  feature("Conversation messages listing") {

    scenario("List messages for non existent conversation") {
      val future = messagesStorage.msgsIndex(ConvId()).flatMap(_.loadCursor).map(_.size)
      awaitUi(future.isCompleted)
      Await.result(future, 1.second) shouldEqual 0
    }

    scenario("List just synced messages") {
      val convId = ConvId("b53ae017-b002-4d8c-8540-4887b58d47c7")
      delMessages(convId)
      awaitUi(1.second)
      val msgs = messagesStorage.getEntries(convId)
      msgs.disableAutowiring()

      processEventsFromJson("/conv/conv_events5.json")

      withDelay {
        listMessages(convId) should have size 59
        msgs.currentValue should be('defined)
        msgs.currentValue.get.size shouldEqual 59
      }
    }

    scenario("Don't update messages cursor when messages are only updated") {
      val convId = ConvId("b53ae017-b002-4d8c-8540-4887b58d47c7")
      val msgs = messagesStorage.getEntries(convId)
      @volatile var updateCount = 0
      msgs { _ => updateCount += 1 }

      withDelay(updateCount shouldEqual 1)

      processEventsFromJson("/conv/conv_events5.json")

      awaitUi(1.second)
      updateCount shouldEqual 1
    }
  }

  feature("Adding messages") {

    scenario("add asset message") {
      val conv = addGroup()
      val asset = AssetId()
      val msg = Await.result(messages.addImageMessage(conv.id, asset, 10, 20), timeout)
      msg.convId shouldEqual conv.id

      val msgs = listMessages(conv.id)
      msgs should have size 1
      msgs.head shouldEqual msg.copy(firstMessage = true)
      msgs.head.id shouldEqual MessageId(asset.str)
    }
  }

  feature("Failed messages") {

    def addLocalMessage(convId: ConvId) = Await.result(messages.addTextMessage(convId, "message"), timeout)

    scenario("Change state to failed if sync fails") {
      val conv = addGroup()
      val msg = addLocalMessage(conv.id)
      Await.result(messages.messageDeliveryFailed(conv.id, msg, internalError("sync failed")), timeout)
      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.head.state shouldEqual Status.FAILED
        getConv(conv.id).map(_.failedCount) shouldEqual Some(1)
      }
    }

    scenario("Mark read when online") {
      val conv = addGroup()
      val msg = addLocalMessage(conv.id)
      Await.result(messages.messageDeliveryFailed(conv.id, msg, internalError("something failed")), timeout)
      Await.result(messages.markMessageRead(conv.id, msg.id), timeout)
      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.head.state shouldEqual Status.FAILED_READ
        getConv(conv.id).map(_.failedCount) shouldEqual Some(0)
      }
    }

    scenario("Don't mark read when offline") {
      val conv = addGroup()
      val msg = addLocalMessage(conv.id)
      online = false
      Await.result(messages.messageDeliveryFailed(conv.id, msg, internalError("offline")), timeout)
      Await.result(messages.markMessageRead(conv.id, msg.id), timeout)
      withDelay {
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.head.state shouldEqual Status.FAILED
        getConv(conv.id).value.failedCount shouldEqual 1
      }
    }
  }

  feature("Incoming messages list") {

    scenario("receive single knock") {
      val user1 = UserDataDao.insertOrReplace(UserData("test"))
      val conv = insertConv(ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group))

      @volatile var updateCount = 0
      val incoming = messagesStorage.getIncomingMessages
      incoming { _ => updateCount += 1}

      withDelay(updateCount shouldEqual 1)
      service.dispatch(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(MessageId(), Knock(false))).withCurrentLocalTime())

      withDelay {
        updateCount shouldEqual 2
        val last = incoming.currentValue.get.last
        last.msgType shouldEqual Message.Type.KNOCK
        last.convId shouldEqual conv.id

        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.head.msgType shouldEqual Message.Type.KNOCK
        msgs.head.hotKnock shouldEqual false
      }
    }

    scenario("receive hotknock") {
      val user1 = UserDataDao.insertOrReplace(UserData("test"))
      val conv = insertConv(ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group))

      @volatile var updateCount = 0
      val incoming = messagesStorage.getIncomingMessages
      incoming { _ => updateCount += 1 }

      withDelay(updateCount shouldEqual 1)

      val msgId = MessageId()
      service.dispatch(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(msgId, Knock(false))).withCurrentLocalTime())

      withDelay {
        updateCount should be > 1
      }
      val count = updateCount

      service.dispatch(GenericMessageEvent(Uid(), conv.remoteId, new Date, user1.id, GenericMessage(msgId, Knock(true))).withCurrentLocalTime())

      withDelay {
        updateCount should be > count
        val msgs = listMessages(conv.id)
        msgs should have size 1
        msgs.head.msgType shouldEqual Message.Type.KNOCK
        msgs.head.hotKnock shouldEqual true
      }
    }
  }

  feature("Unread count") {

    scenario("count unread messages") {
      val convId = ConvId("b53ae017-b002-4d8c-8540-4887b58d47c7")
      delMessages(convId)
      awaitUi(1.second)

      processEventsFromJson("/conv/conv_events5.json")

      val messages = listMessages(convId)
      messages should have size 59

      Await.result(convsContent.convById(convId), 1.seconds).get.lastRead shouldEqual Instant.EPOCH

      val unreadCount = messagesStorage.unreadCount(convId)
      unreadCount.disableAutowiring()

      convsUi.setLastRead(convId, MessageData(MessageId(), convId, EventId.Zero, Message.Type.TEXT, UserId(), time = Instant.EPOCH))
      withDelay(unreadCount.currentValue shouldEqual Some(59))
      convsUi.setLastRead(convId, messages(40))
      withDelay(unreadCount.currentValue shouldEqual Some(18))
      convsUi.setLastRead(convId, MessageData(MessageId(), convId, EventId.Zero, Message.Type.TEXT, UserId(), time = Instant.EPOCH)) // has no effect
      withDelay(unreadCount.currentValue shouldEqual Some(18))
      convsUi.setLastRead(convId, MessageData(MessageId(), convId, EventId.MaxValue, Message.Type.TEXT, UserId(), time = Instant.now()))
      withDelay(unreadCount.currentValue shouldEqual Some(0))
    }
  }

  def readResource(path: String) = new JSONObject(Source.fromInputStream(getClass.getResourceAsStream(path), "utf8").getLines().mkString("\n"))
}