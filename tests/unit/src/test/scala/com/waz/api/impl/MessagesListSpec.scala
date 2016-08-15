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

import com.waz.api.Message.{Status, Type}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.LastRead
import com.waz.model._
import com.waz.testutils.Implicits._
import com.waz.testutils._
import com.waz.threading.Threading
import com.waz.{RobolectricUtils, api}
import org.robolectric.Robolectric
import org.robolectric.shadows.ShadowLog
import org.scalatest._
import org.threeten.bp.Instant

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class MessagesListSpec extends FeatureSpec with Matchers with Inspectors with BeforeAndAfter with BeforeAndAfterAll with RobolectricTests with RobolectricUtils { test =>
  var lastReadSync = None: Option[Instant]

  private implicit lazy val dispatcher = Threading.Background

  lazy val convId = ConvId()
  lazy val convData = ConversationData(convId, RConvId(convId.str), None, UserId(), ConversationType.Group, lastRead = timeForEvent(990))
  lazy val selfUserId = UserId()

  lazy val zmessaging = new MockZMessaging(selfUserId = this.selfUserId) {
    insertConv(new ConversationData(ConvId(selfUserId.str), RConvId(selfUserId.str), None, selfUserId, ConversationType.Self))
  }

  implicit lazy val ui: MockUiModule = new MockUiModule(zmessaging) {
    override lazy val messages: com.waz.ui.Messages = new com.waz.ui.Messages()(this) {

      override def updateLastRead(conv: ConvId, msg: MessageData) = {
        lastReadSync = Some(msg.time)
        super.updateLastRead(conv, msg)
      }
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    ui.onCreate(Robolectric.application)
    ui.onResume()

    Await.result(zmessaging.convsStorage.insert(convData), 5.seconds)
  }

  after {
    ShadowLog.stream = null
  }

  import zmessaging._

  def conv = getConv(convId).get

  lazy val msgs = new MessagesList(convId)

  def timeForEvent(seq: Long) = Instant.EPOCH.plusSeconds(10 * seq)

  def generateMessages(from: Long, to: Long, conv: ConvId = convId) = {
    Await.result(Future.sequence(
      (from until to) map { seq =>
        zmessaging.messagesStorage.addMessage(MessageData(MessageId("m_" + seq.toString), conv, api.Message.Type.TEXT, UserId("u_" + seq.toString), MessageData.textContent("test " + seq), state = Status.SENT, time = timeForEvent(seq)))
      }
    ), 5.seconds)
  }

  feature("Last read") {

    lazy val conv = zmessaging.insertConv(new ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group, lastRead = timeForEvent(1)))
    lazy val msgs = new MessagesList(conv.id)

    scenario("update last read index with time not in messages list") {
      zmessaging.convsContent.updateConversationLastRead(conv.id, timeForEvent(120).minusMillis(5))
      generateMessages(100, 140, conv.id)

      withDelay {
        msgs.size shouldEqual 40
        msgs.getLastReadIndex shouldEqual 20
        msgs.getUnreadCount shouldEqual 20
      }
    }

    scenario("update unread count on last read change") {
      val m = msgs.get(21)

      withDelay {
        msgs.getUnreadCount shouldEqual 18
        msgs.getLastReadIndex shouldEqual 21
      }
      lastReadSync shouldEqual Some(m.asInstanceOf[com.waz.api.impl.Message].data.time)
    }

    scenario("update last read on notification") {
      zmessaging.dispatch(GenericMessageEvent(Uid(), RConvId(selfUserId.str), new Date, selfUserId, GenericMessage(Uid(), LastRead(conv.remoteId, timeForEvent(122)))))

      withDelay {
        msgs.lastRead shouldEqual timeForEvent(122)
        msgs.getUnreadCount should be < 18
      }
    }

    scenario(s"update last read index on new message") {
      generateMessages(80, 99, conv.id)
      withDelay {
        msgs.getLastReadIndex shouldEqual 41
      }
    }
  }

  feature("Rich media messages") {

    scenario("Add text message with multiple links") {
      import com.waz.api.Message.Part.Type._
      val count = msgs.size

      val text = "Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI more text https://www.youtube.com/watch?v=c0KYU2j0TM4 and even more"
      val msg = Await.result(zmessaging.messages.addTextMessage(convId, text), 1.second)
      msg.content shouldEqual Seq(
        MessageContent(TEXT, "Here is some text."),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"),
        MessageContent(TEXT, "more text"),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=c0KYU2j0TM4"),
        MessageContent(TEXT, "and even more")
      )

      withDelay {
        msgs should have size count + 1
      }

      val m = msgs.get(count)
      m.getMessageType shouldEqual api.Message.Type.RICH_MEDIA
      m.getParts should have size(5)
      forAll(m.getParts.zip(msg.content)) { case (a, b) =>
        a.getPartType shouldEqual b.tpe
        a.getBody shouldEqual b.content
      }
      m.getBody shouldEqual text
    }
  }

  feature("Messages in one-to-one conv") {
    lazy val user = UserId()
    lazy val conv = insertConv(ConversationData(ConvId(user.str), RConvId(user.str), None, user, ConversationType.OneToOne))

    lazy val msgs = new MessagesList(conv.id)(ui)

    def addMessage(data: MessageData) =
      Await.result(zmessaging.messagesStorage.addMessage(data), 5.seconds)

    scenario("Create conv member join message") {
      addMessage(MessageData(MessageId(), conv.id, api.Message.Type.MEMBER_JOIN, user, Nil, members = Set(user), time = Instant.ofEpochMilli(1), firstMessage = true))
      addMessage(MessageData(MessageId(), conv.id, api.Message.Type.CONNECT_REQUEST, user, Seq(MessageContent(api.Message.Part.Type.TEXT, "req")), recipient = Some(selfUserId), time = Instant.ofEpochMilli(2)))
      addMessage(MessageData(MessageId(), conv.id, api.Message.Type.MEMBER_JOIN, selfUserId, Nil, members = Set(selfUserId), time = Instant.ofEpochMilli(3)))
      withDelay {
        msgs should have size 3
        msgs.get(0).isCreateConversation shouldEqual true
        msgs.map(_.getMessageType) shouldEqual Seq(Type.MEMBER_JOIN, Type.CONNECT_REQUEST, Type.MEMBER_JOIN)
      }
    }
  }
}
