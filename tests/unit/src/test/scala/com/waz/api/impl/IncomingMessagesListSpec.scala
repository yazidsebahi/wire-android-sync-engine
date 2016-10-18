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
package com.waz.api.impl

import java.util.Date

import com.waz.api.IncomingMessagesList.MessageListener
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Knock
import com.waz.model._
import com.waz.testutils._
import com.waz.ui._
import com.waz.utils._
import com.waz.utils.events.EventStream
import com.waz.{api, _}
import org.scalatest.{Matchers, _}
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._

class IncomingMessagesListSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with RobolectricTests with RobolectricUtils { test =>
  import com.waz.utils.events.EventContext.Implicits.global

  val timeout = 5.seconds

  lazy val selfUser = UserData("FromUser")
  lazy val service = new MockZMessaging(selfUserId = selfUser.id)
  implicit lazy val messages: UiModule = new MockUiModule(service)
  
  lazy val conv = createConv(muted = false)
  lazy val conv1 = createConv(muted = false)
  lazy val mutedConv = createConv(muted = true)

  lazy val msgsSignal = service.messagesStorage.getIncomingMessages

  feature("Incoming events") {

    scenario("Add freshly received message to incoming messages list") {
      val event = textMessageEvent(Uid(), conv.remoteId, new Date, UserId(), "with local time").withCurrentLocalTime()
      withUpdate(msgsSignal) { service.dispatch(event) }
      withDelay {
        val msgs = incoming
        msgs should have size 1
        msgs.head.content shouldEqual MessageData.textContent("with local time")
        msgs.head.localTime shouldEqual event.localTime.instant
        msgs.head.firstMessage shouldEqual true
      }
    }

    scenario("Add knock message to incoming messages list") {
      val msgId = MessageId()
      val event = GenericMessageEvent(Uid(), conv.remoteId, new Date, UserId(), GenericMessage(msgId.uid, Knock())).withCurrentLocalTime()
      withUpdate(msgsSignal) { service.dispatch(event) }
      withDelay {
        val msgs = incoming
        msgs should have size 2
        msgs.last.localTime shouldEqual event.localTime.instant
      }
    }

    scenario("Don't add messages without local time") {
      val event = textMessageEvent(Uid(), conv.remoteId, new Date, UserId(), "no local time")
      incoming should have size 2
    }

    scenario("Don't add messages from muted conversations") {
      val event = textMessageEvent(Uid(), mutedConv.remoteId, new Date, UserId(), "with local time").withCurrentLocalTime()
      service.dispatch(event)
      awaitUi(200.millis)
      incoming should have size 2
    }

    scenario("Add connect request messages") {
      val event = ConnectRequestEvent(Uid(), conv1.remoteId, new Date, UserId(), "with local time", selfUser.id, "meep", None).withCurrentLocalTime()
      withUpdate(msgsSignal) { service.dispatch(event) }
      val msgs = incoming
      msgs should have size 3
      msgs.last.msgType shouldEqual api.Message.Type.CONNECT_REQUEST
      msgs.last.firstMessage shouldEqual false
    }

    scenario("Add more messages to conversation with connect request") {
      val event = textMessageEvent(Uid(), conv1.remoteId, new Date, UserId(), "first").withCurrentLocalTime()
      withUpdate(msgsSignal) { service.dispatch(event) }
      withDelay {
        val msgs = incoming
        msgs should have size 4
        msgs.last.content shouldEqual MessageData.textContent("first")
        msgs.last.localTime shouldEqual event.localTime.instant
        msgs.last.firstMessage shouldEqual true
      }

      val event1 = textMessageEvent(Uid(), conv1.remoteId, new Date, UserId(), "second").withCurrentLocalTime()
      withUpdate(msgsSignal) { service.dispatch(event1) }

      withDelay {
        val msgs = incoming
        msgs should have size 5
        msgs.last.content shouldEqual MessageData.textContent("second")
        msgs.last.localTime shouldEqual event1.localTime.instant
        msgs.last.firstMessage shouldEqual false
      }
    }
  }

  feature("IncomingMessagesList") {

    def md(localTime: Instant = Instant.now) = MessageData(MessageId(), conv.id, api.Message.Type.TEXT, conv.creator, localTime = localTime)

    def loadMessages(msgs: List[MessageData], list: IncomingMessages = new IncomingMessages) = {
      var received = Nil: List[MessageData]
      list.addMessageListener(new MessageListener {
        override def onIncomingMessage(msg: api.Message): Unit = received ::= msg.asInstanceOf[Message].data
      })
      list.onLoaded(msgs)
      received.reverse
    }

    scenario("Invoke listeners on freshly received messages") {
      val msgs = List(md(), md(), md())
      loadMessages(msgs) shouldEqual msgs
    }

    scenario("Invoke listeners only for newer messages on content change") {
      val list = new IncomingMessages
      val time = Instant.now - 10.millis
      val msgs = List(md(time), md(time + 1.milli), md(time + 2.millis))
      loadMessages(msgs, list) shouldEqual msgs

      val added = List(md(time + 3.millis), md(time + 5.millis))
      loadMessages(added ::: msgs, list) shouldEqual added
    }

    scenario("Don't call listeners if received old messages") {
      val time = Instant.now - service.timeouts.messages.incomingTimeout - 10.millis
      loadMessages(List(md(time), md(time + 1.milli))) should be(empty)
    }
  }

  def incoming = Await.result(EventStream.wrap(service.messages.getIncomingMessages).next, timeout)

  def createConv(muted: Boolean): ConversationData = {
    val conv = ConvId()
    Await.result(service.convsContent.insertConversation(ConversationData(conv, RConvId(conv.str), None, UserId(), ConversationType.Group, muted = muted)), timeout)
  }
}
