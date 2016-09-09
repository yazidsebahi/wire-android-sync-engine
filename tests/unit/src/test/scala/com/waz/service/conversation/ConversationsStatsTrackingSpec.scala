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

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.content._
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.tracking.TrackingStats
import com.waz.testutils.MockZMessaging
import com.waz.testutils.Matchers._
import com.waz.threading.Threading
import org.robolectric.Robolectric
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.threeten.bp.Instant

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ConversationsStatsTrackingSpec extends FeatureSpec with Matchers with OptionValues with TableDrivenPropertyChecks with RobolectricTests with RobolectricUtils { test =>
  implicit lazy val dispatcher = Threading.Background
  lazy val globalStorage = new GlobalDatabase(Robolectric.application)

  lazy val selfUser = UserData("self user")

  implicit def db: SQLiteDatabase = service.db.dbHelper.getWritableDatabase

  lazy val service = new MockZMessaging(selfUserId = selfUser.id) {
    override lazy val messagesStorage = new MessagesStorage(context, db, selfUserId, convsStorage, usersStorage, msgAndLikes, timeouts) {
      override def msgsIndex(conv: ConvId): Future[ConvMessagesIndex] = Future.failed(new RuntimeException("for testing purposes, we do not want to update unread and failed count from real message count"))
    }
  }

  feature("Conversation statistics") {

    lazy val times = Array(1000L, 20000L, 300000L) map Instant.ofEpochMilli

    lazy val normalContact = convData(ConversationType.OneToOne).copy(lastEventTime = times(0))
    lazy val blockedContact = convData(ConversationType.OneToOne).copy(hidden = true)
    lazy val archivedContact = convData(ConversationType.OneToOne, archived = true)
    lazy val mutedContact = convData(ConversationType.OneToOne).copy(muted = true)
    lazy val pendingContact = convData(ConversationType.WaitForConnection)
    lazy val archivedPendingContact = convData(ConversationType.WaitForConnection, archived = true)
    lazy val contactWithVoice = convData(ConversationType.OneToOne).copy(hasVoice = true)
    lazy val normalGroup = convData(ConversationType.Group).copy(unreadCount = 10, failedCount = 3, lastEventTime = times(1))
    lazy val archivedGroup = convData(ConversationType.Group, archived = true).copy(unreadCount = 99, failedCount = 13)
    lazy val mutedGroup = convData(ConversationType.Group).copy(muted = true).copy(unreadCount = 2, failedCount = 1)
    lazy val archivedAndMutedGroup = convData(ConversationType.Group, archived = true).copy(muted = true, unreadCount = 200, failedCount = 99, lastEventTime = times(2))
    
    scenario("Adding conversations should update the tracked statistics correctly") {
      val addingConvs = Table(
        ("conversations",        "groups", "archived", "muted", "contacts", "blocked", "voice", "unread", "failed", "pending", "lastEvent"),
        (normalContact,          0,        0,          0,       1,          0,         0,       0,        0,        0,         times(0)),
        (blockedContact,         0,        0,          0,       1,          1,         0,       0,        0,        0,         times(0)),
        (normalGroup,            1,        0,          0,       1,          1,         0,       10,       3,        0,         times(1)),
        (archivedGroup,          2,        1,          0,       1,          1,         0,       10,       3,        0,         times(1)),
        (archivedContact,        2,        2,          0,       2,          1,         0,       10,       3,        0,         times(1)),
        (pendingContact,         2,        2,          0,       2,          1,         0,       10,       3,        1,         times(1)),
        (archivedPendingContact, 2,        3,          0,       2,          1,         0,       10,       3,        1,         times(1)),
        (mutedGroup,             3,        3,          1,       2,          1,         0,       12,       4,        1,         times(1)),
        (mutedContact,           3,        3,          2,       3,          1,         0,       12,       4,        1,         times(1)),
        (archivedAndMutedGroup,  4,        4,          3,       3,          1,         0,       12,       4,        1,         times(1)),
        (contactWithVoice,       4,        4,          3,       4,          1,         1,       12,       4,        1,         times(1))
      )

      forAll(addingConvs) { case (conv, groups, archived, muted, contacts, blocked, voice, unread, failed, pending, lastEvent) =>
        Await.result(service.convsStorage.insert(conv), 5.seconds)

        soon {
          service.tracking.trackingSignal.currentValue.value shouldEqual TrackingStats(groups, archived, muted, contacts, blocked, 0, 0, 0, 0, 0, 0)

          val currentStats = service.convsStats.listStats.currentValue.value
          currentStats.voiceCount shouldEqual voice
          currentStats.unreadCount shouldEqual unread
          currentStats.unsentCount shouldEqual failed
          currentStats.pendingCount shouldEqual pending

          service.convsStats.lastEventTime shouldEqual lastEvent
        }
      }
    }

    scenario("Removing conversations should update the tracked statistics correctly") {
      val removingConvs = Table(
        ("conversations",        "groups", "archived", "muted", "contacts", "blocked", "voice", "unread", "failed", "pending", "lastEvent"),
        (normalContact,          4,        4,          3,       3,          1,         1,       12,       4,        1,         times(1)),
        (contactWithVoice,       4,        4,          3,       2,          1,         0,       12,       4,        1,         times(1)),
        (blockedContact,         4,        4,          3,       2,          0,         0,       12,       4,        1,         times(1)),
        (normalGroup,            3,        4,          3,       2,          0,         0,       2,        1,        1,         times(1)),
        (archivedGroup,          2,        3,          3,       2,          0,         0,       2,        1,        1,         times(1)),
        (pendingContact,         2,        3,          3,       2,          0,         0,       2,        1,        0,         times(1)),
        (archivedPendingContact, 2,        2,          3,       2,          0,         0,       2,        1,        0,         times(1)),
        (archivedContact,        2,        1,          3,       1,          0,         0,       2,        1,        0,         times(1)),
        (mutedGroup,             1,        1,          2,       1,          0,         0,       0,        0,        0,         times(1)),
        (mutedContact,           1,        1,          1,       0,          0,         0,       0,        0,        0,         times(1)),
        (archivedAndMutedGroup,  0,        0,          0,       0,          0,         0,       0,        0,        0,         times(1))
      )

      forAll(removingConvs) { case (conv, groups, archived, muted, contacts, blocked, voice, unread, failed, pending, lastEvent) =>
        Await.result(service.convsStorage.remove(conv.id), 5.seconds)

        soon {
          service.tracking.trackingSignal.currentValue.value shouldEqual TrackingStats(groups, archived, muted, contacts, blocked, 0, 0, 0, 0, 0, 0)

          val currentStats = service.convsStats.listStats.currentValue.value
          currentStats.voiceCount shouldEqual voice
          currentStats.unreadCount shouldEqual unread
          currentStats.unsentCount shouldEqual failed
          currentStats.pendingCount shouldEqual pending

          service.convsStats.lastEventTime shouldEqual lastEvent
        }
      }
    }
  }


  def convData(convType: ConversationType, archived: Boolean = false) =
    ConversationData(ConvId(), RConvId(), Some("name"), UserData("self user").id, convType, archived = archived)
}
