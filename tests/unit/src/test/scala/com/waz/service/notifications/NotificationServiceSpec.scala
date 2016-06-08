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
package com.waz.service.notifications

import java.util.Date

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.NotificationsHandler.GcmNotification
import com.waz.model.AssetStatus.{UploadCancelled, UploadDone}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Asset
import com.waz.model.GenericMessage.TextMessage
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service.NotificationService.Notification
import com.waz.testutils.Matchers._
import com.waz.testutils.MockZMessaging
import com.waz.utils.events.EventContext
import com.waz.zms.GcmHandlerService.EncryptedGcm
import org.json.JSONObject
import org.robolectric.Robolectric
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.threeten.bp.Instant

import scala.concurrent.Await
import scala.concurrent.duration._

class NotificationServiceSpec extends FeatureSpec with Matchers with PropertyChecks with BeforeAndAfter with BeforeAndAfterAll with RobolectricTests with RobolectricUtils { test =>

  lazy val context = Robolectric.application

  @volatile var currentNotifications = Nil: Seq[GcmNotification]

  lazy val selfUserId = UserId()
  lazy val oneToOneConv = ConversationData(ConvId(), RConvId(), None, selfUserId, ConversationType.OneToOne)
  lazy val groupConv = ConversationData(ConvId(), RConvId(), Some("group conv"), selfUserId, ConversationType.Group)

  lazy val zms = new MockZMessaging() { self =>
    users.selfUserId := test.selfUserId

    notifications.getNotifications(Duration.Zero) { currentNotifications = _ } (EventContext.Global)
  }

  lazy val service = zms.notifications

  implicit def db: SQLiteDatabase = zms.storage.dbHelper.getWritableDatabase

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(zms.convsStorage.insert(Seq(oneToOneConv, groupConv)), 5.seconds)
  }

  before {
    clearNotifications()
  }

  feature("Add notifications for events") {

    scenario("Process user connection event for an unsynced user") {
      val userId = UserId()
      val convId = RConvId()
      Await.ready(zms.dispatch(UserConnectionEvent(Uid(), convId, selfUserId, userId, Some("hello"), ConnectionStatus.PendingFromOther, new Date, Some("other user")).withCurrentLocalTime()), 5.seconds)

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, "hello", _, `userId`, GcmNotification.Type.CONNECT_REQUEST, _, _, false, Some("other user"), _, _, None), _, "other user", "other user", false, _, _)) => true
        }
      }
    }

    scenario("Process contact join event") {
      val userId = UserId(oneToOneConv.id.str)
      Await.ready(zms.dispatch(ContactJoinEvent(Uid(), userId, "test name")), 5.seconds)

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, _, _, `userId`, GcmNotification.Type.CONTACT_JOIN, _, _, _, _, _, _, None), _, _, _, false, _, _)) => true
        }
      }
    }

    scenario("Process mentions event") {
      val userId = UserId(oneToOneConv.id.str)
      val convId = oneToOneConv.remoteId
      Await.ready(zms.dispatch(GenericMessageEvent(Uid(), convId, new Date, userId, TextMessage("test name", Map(selfUserId -> "name")))), 5.seconds)

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, "test name", `convId`, `userId`, GcmNotification.Type.TEXT, _, _, _, _, Seq(`selfUserId`), _, None), _, _, _, false, true, _)) => true
        }
      }
    }

    scenario("Process any asset event") {
      val userId = UserId(oneToOneConv.id.str)
      val convId = oneToOneConv.remoteId
      Await.ready(zms.dispatch(GenericMessageEvent(Uid(), convId, new Date, userId, GenericMessage(MessageId(), Asset(UploadCancelled)))), 5.seconds) // this should not generate a notification
      Await.ready(zms.dispatch(GenericAssetEvent(Uid(), convId, new Date, userId, GenericMessage(MessageId(), Asset(UploadDone(AssetKey(RAssetDataId(), AESKey(), Sha256("sha"))))), RAssetDataId(), None)), 5.seconds)

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, _, `convId`, `userId`, GcmNotification.Type.ANY_ASSET, _, _, _, _, _, None, None), _, _, _, false, _, _)) => true
        }
      }
    }

    scenario("Receiving the same message twice") {
      val ev1 = MessageAddEvent(Uid(), groupConv.remoteId, EventId(2), new Date, UserId(), "meep")
      val ev2 = ev1.copy()

      Await.ready(zms.dispatch(ev1, ev2), 5.seconds)

      val groupId = groupConv.id

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, "meep", _, _, GcmNotification.Type.TEXT, _, _, _, _, _, _, None), `groupId`, "group conv", _, true, _, _)) => true
        }
      }
    }

    scenario("Don't show duplicate notification even if notifications were cleared") {
      val ev = MessageAddEvent(Uid(), groupConv.remoteId, EventId(2), new Date, UserId(), "meep")

      zms.dispatch(ev)

      withDelay {
        currentNotifications should have size 1
      }
      clearNotifications()

      zms.dispatch(ev)

      awaitUi(250.millis)
      currentNotifications shouldBe empty
    }

    scenario("Ignore events from self user") {
      val ev = MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date, selfUserId, "meep")

      Await.ready(zms.dispatch(ev), 5.seconds)
      awaitUi(200.millis)

      currentNotifications shouldBe empty
    }

    scenario("Remove older notifications when lastRead is updated") {
      val time = System.currentTimeMillis()
      zms.dispatch(
        MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date(time), UserId(), "meep"),
        MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date(time + 10), UserId(), "meep1"),
        MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date(time + 20), UserId(), "meep2"),
        MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date(time + 30), UserId(), "meep3"),
        MessageAddEvent(Uid(), groupConv.remoteId, EventId.Zero, new Date(time + 40), UserId(), "meep4")
      )

      withDelay {
        currentNotifications should have size 5
      }

      zms.convsContent.updateConversationLastRead(groupConv.id, Instant.ofEpochMilli(time + 30))

      withDelay {
        currentNotifications should have size 1
      }
    }
  }

  feature("List notifications") {

    scenario("List notifications with local conversation ids") {
      Await.ready(zms.dispatch(MessageAddEvent(Uid(), groupConv.remoteId, EventId(2), new Date, UserId(), "test msg")), 5.seconds)

      val groupId = groupConv.id

      withDelay {
        currentNotifications should beMatching {
          case Seq(Notification(NotificationData(_, _, _, _, GcmNotification.Type.TEXT, _, _, _, _, _, _, None), `groupId`, "group conv", _, true, _, _)) => true
        }
      }
    }
  }

  feature("Receive GCM") {

    scenario("parse encrypted notification") {

      val json = """{"data":{"payload":[{"conversation":"0a032ce9-2b6b-4f6c-9825-1ac958eeb94b","time":"2016-01-27T17:29:45.714Z","data":{"text":"owABAaEAWCCYir0PHK4qO8706uRy2aID1MZPjsZd3BCgrVBLZWm6uwJYxQKkAAQBoQBYIKk+iFvwuNS4u8C9kEr5euVnNecXconfHNcUv8BMSuM+AqEAoQBYIDcRmzxai6NPcUeVmcBAeKYl1ikB1zP5IdDBLKootTDdA6UAUGEAzoTIyssU5udKqxR2mDQBAAIAA6EAWCCT1VMZ3KIXptRnH9O2oeW4Z7Ck\/6iwshmRig\/+mpKA8gRYNREviDPKaPYzaUQpqlKWNm3EKyEF+B1eSoEdaOg52+0NIiHthTi22dfZc8MwAKeg5a4xZ\/lA","sender":"30f327954b3a772a","recipient":"ecd0802b04b01fe8"},"from":"b3fe68df-db7f-4a1d-b49c-370e77334af0","type":"conversation.otr-message-add"}],"transient":false,"id":"8eeb8574-c51b-11e5-a396-22000b0a2794"}}"""
      new JSONObject(json) match {
        case EncryptedGcm(notification) => info(s"notification: $notification")
        case resp => fail(resp.toString())
      }
    }
  }

  def clearNotifications(): Unit = Await.result(service.clearNotifications(), 5.seconds)
}
