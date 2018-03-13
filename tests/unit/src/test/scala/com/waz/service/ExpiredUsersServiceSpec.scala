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

import com.waz.content.{MembersStorage, UsersStorage}
import com.waz.model.{ConvId, SyncId, UserData, UserId}
import com.waz.service.conversation.ConversationsListStateService
import com.waz.service.push.PushService
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.threading.Threading
import com.waz.utils.events.{EventStream, Signal}
import org.threeten.bp.Duration

import scala.concurrent.Future
import scala.concurrent.duration._
import com.waz.utils.RichInstant

class ExpiredUsersServiceSpec extends AndroidFreeSpec {

  implicit val ec = Threading.Background
  val convState = mock[ConversationsListStateService]
  val push      = mock[PushService]
  val members   = mock[MembersStorage]
  val users     = mock[UsersStorage]
  val sync      = mock[SyncServiceHandle]

  val onDeleted = EventStream[Seq[(UserId, ConvId)]]
  val currentConv = Signal(Option.empty[ConvId])

  scenario("Start timer for user soon to expire") {
    val conv = ConvId("conv")

    currentConv ! Some(conv)

    val wirelessId = UserId("wirelessUser")

    val convUsers = Set(
      UserData("user1").copy(id = UserId("user1")),
      UserData("user2").copy(id = UserId("user2")),
      UserData("wireless").copy(id = wirelessId, expiresAt = Some(clock.instant() + 10500.millis))
    )

    (members.getActiveUsers _).expects(conv).once().returning(Future.successful(convUsers.map(_.id).toSeq))
    (users.getAll _).expects(*).once().onCall { (ids: Traversable[UserId]) =>
      if (ids.toSet != convUsers.map(_.id)) fail("Unexpected user ids")
      Future.successful(convUsers.toSeq.map(Some(_)))
    }

    //    (members.getByUsers _).expects(Set(wirelessId)).once().returning(Future.successful(IndexedSeq.empty))

    val service = getService


    val finished = EventStream[Unit]()
    (sync.syncUsers _).expects(*).once().onCall { (us: Seq[UserId]) =>
      if (!us.contains(wirelessId)) fail("Called sync for wrong user")
      finished ! {}
      Future.successful(SyncId())
    }

    result(finished.next)
  }

  scenario("Start timer for user soon to expire, user is removed elsewhere") {
    val conv = ConvId("conv")

    currentConv ! Some(conv)

    val wirelessId = UserId("wirelessUser")

    val convUsers = Set(
      UserData("user1").copy(id = UserId("user1")),
      UserData("user2").copy(id = UserId("user2")),
      UserData("wireless").copy(id = wirelessId, expiresAt = Some(clock.instant() + 10500.millis))
    )

    (members.getActiveUsers _).expects(conv).once().returning(Future.successful(convUsers.map(_.id).toSeq))
    (users.getAll _).expects(*).once().onCall { (ids: Traversable[UserId]) =>
      if (ids.toSet != convUsers.map(_.id)) fail("Unexpected user ids")
      Future.successful(convUsers.toSeq.map(Some(_)))
    }

    (members.getByUsers _).expects(Set(wirelessId)).once().returning(Future.successful(IndexedSeq.empty))

    val service = getService

    onDeleted ! Seq((wirelessId, conv))

    awaitAllTasks

    Thread.sleep(500)
    (sync.syncUsers _).expects(*).never()
  }

  def getService = {
    (members.onDeleted _).expects().once().returning(onDeleted)
    (convState.selectedConversationId _).expects().once().returning(currentConv)
    (push.beDrift _).expects().anyNumberOfTimes().returning(Signal.const(Duration.ZERO))

    new ExpiredUsersService(convState, push, members, users, sync)
  }

}
