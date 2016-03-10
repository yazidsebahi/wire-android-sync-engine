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
import com.waz.RobolectricUtils
import com.waz.api.{ErrorType, Message}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.ErrorData.ErrorDataDao
import com.waz.model._
import com.waz.service.conversation.ConversationsService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.ConversationsClient.ConversationResponse
import com.waz.testutils.Matchers._
import com.waz.testutils.{DefaultPatienceConfig, EmptySyncService, MockZMessaging}
import com.waz.threading.Threading
import com.waz.utils._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._

class ConversationsServiceSpec extends FeatureSpec with OptionValues with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig { test =>
  implicit lazy val dispatcher = Threading.Background

  implicit val timeout: Timeout = 10.seconds

  lazy val user = UserData("user 1")
  lazy val selfUserId = UserId()

  implicit def db: SQLiteDatabase = service.storage.dbHelper.getWritableDatabase

  var convNameSync = None: Option[ConvId]
  var convArchivedSync = None: Option[ConvId]

  lazy val service = new MockZMessaging() {

    override lazy val sync: SyncServiceHandle = new EmptySyncService {
      override def postConversationName(id: ConvId, n: String) = {
        convNameSync = Some(id)
        super.postConversationName(id, n)
      }

      override def postConversationState(id: ConvId, s: ConversationState) = {
        convArchivedSync = Some(id)
        super.postConversationState(id, s)
      }
    }

    users.selfUserId := test.selfUserId
  }

  before {
    convNameSync = None
    convArchivedSync = None
  }

  import service._

  feature("Conversation events") {
    scenario("Generate tempConversationId from conversation member sequence") {
      val randomUserIds_1 = Seq(UserId("g"), UserId("2"), UserId("b"))
      val randomUserIds_2 = Seq(UserId("2"), UserId("g"), UserId("b"))
      val randomUserIds_3 = Seq(UserId("b"), UserId("g"), UserId("2"))

      ConversationsService.generateTempConversationId(randomUserIds_1: _*) should be(RConvId("2bg"))
      ConversationsService.generateTempConversationId(randomUserIds_2: _*) should be(RConvId("2bg"))
      ConversationsService.generateTempConversationId(randomUserIds_3: _*) should be(RConvId("2bg"))
    }

    scenario("Handle CreateConversationEvent tempConversation stored in db") {
      val from = UserId("123")
      val to = UserId("234")

      val conversationData = insertConv(ConversationData(ConvId(), ConversationsService.generateTempConversationId(from, to), None, from, ConversationType.Group))

      val rconvId = RConvId()
      val conversationResponse = ConversationResponse(conversationData.copy(remoteId = rconvId), Seq(ConversationMemberData(from, conversationData.id), ConversationMemberData(to, conversationData.id)))
      service.dispatchEvent(CreateConversationEvent(Uid(), rconvId, new Date(), from, conversationResponse))

      withDelay {
        getConv(conversationData.id).map(_.remoteId) shouldEqual Some(rconvId)
      }
    }
  }

  feature("Last event property") {

    scenario("Update last event and time on message add event") {
      val conv = insertConv(ConversationData(ConvId(), RConvId(), Some("convName"), selfUserId, ConversationType.Group, lastEvent = EventId(2), lastEventTime = Instant.ofEpochMilli(100)))

      val ev = EventId(3)
      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, ev, new Date(1000), UserId(), "test"))
      withDelay {
        convsContent.convById(conv.id).map(_.map(m => (m.lastEvent, m.lastEventTime))) should eventually(be(Some((ev, Instant.ofEpochMilli(1000)))))
      }
    }

    scenario("Update lastRead on message from self") {
      val conv = insertConv(ConversationData(ConvId(), RConvId(), Some("convName"), selfUserId, ConversationType.Group, lastEvent = EventId(2), lastEventTime = Instant.ofEpochMilli(100)))

      service.dispatchEvent(MessageAddEvent(Uid(), conv.remoteId, EventId(3), new Date(1000), selfUserId, "test"))
      withDelay {
        convsContent.convById(conv.id).map(_.map(m => m.lastRead)).futureValue shouldEqual Some(Instant.ofEpochMilli(1000))
      }
    }
  }

  feature("Conversation state") {

    scenario("load missed call info") {
      val oneToOneConv = insertConv(ConversationData(ConvId(), RConvId(), Some("convName"), user.id, ConversationType.Group, lastRead = Instant.now.minusSeconds(1000)))
      service.dispatchEvent(VoiceChannelDeactivateEvent(Uid(), oneToOneConv.remoteId, EventId(10), new Date(), user.id, Some("missed")))

      withDelay {
        Await.result(convsContent.convById(oneToOneConv.id), timeout).flatMap(_.missedCallMessage) should be('defined)
      }
    }

    scenario("after a call ends, the conv updates its last event time") {
      val oneToOneConv = insertConv(ConversationData(ConvId(), RConvId(), Some("convName"), user.id, ConversationType.Group))
      val eventId = EventId(10)
      val eventTime = new Date
      service.dispatchEvent(VoiceChannelDeactivateEvent(Uid(), oneToOneConv.remoteId, eventId, eventTime, user.id, None))
      awaitUi(100.millis)
      Await.result(convsContent.convById(oneToOneConv.id), timeout).map(_.lastEventTime) shouldEqual Some(eventTime.instant)
    }
  }


  feature("Archiving") {

    scenario("archive conversation") {
      val oneToOneConv = insertConv(ConversationData(ConvId(), RConvId(), Some("convName"), user.id, ConversationType.Group))

      val conv = Await.result(convsUi.setConversationArchived(oneToOneConv.id, archived = true), timeout)
      conv should be('defined)
      conv.get.archived shouldEqual true
      conv.get.archiveTime shouldEqual conv.get.lastEventTime

      convArchivedSync shouldEqual Some(oneToOneConv.id)
    }
  }

  feature("Dismiss errors") {

    scenario("Dismiss failed create conversation error") {
      val conv = insertConv(ConversationData(ConvId(), RConvId(), Some("invalid group conv"), user.id, ConversationType.Group))
      addMember(conv.id, UserId())
      addMessage(MessageData(MessageId(), conv.id, EventId(0), EventId(0), Message.Type.TEXT, UserId()))
      val err = ErrorDataDao.insertOrReplace(ErrorData(Uid(), ErrorType.CANNOT_CREATE_GROUP_CONVERSATION_WITH_UNCONNECTED_USER, Nil, Nil, Some(conv.id)))
      Await.result(service.errors.dismissError(err.id), timeout)

      withDelay {
        listConvs.map(_.id) should not contain conv.id
        listMembers(conv.id) should be(empty)
        listMessages(conv.id) should be(empty)
        ErrorDataDao.list should be(empty)
      }
    }
  }
}
