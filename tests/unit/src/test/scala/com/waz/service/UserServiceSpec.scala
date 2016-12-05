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

import com.waz.RobolectricUtils
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.utils.events.EventContext.Implicits.global
import com.waz.utils.returning
import org.robolectric.Robolectric
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class UserServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils {

  lazy val users = Seq(UserData("other user 1"), UserData("other user 2"), UserData("some name"),
    UserData("related user 1"), UserData("related user 2"), UserData("other related"),
    UserData("friend user 1"), UserData("friend user 2"), UserData("some other friend")
  )

  val timeout = 5.seconds

  var zms: MockZMessaging = _
  def storage = zms.storage.db
  def service = zms.users

  var syncRequest = None: Option[UserId]

  before {
    ZMessaging.context = Robolectric.application

    zms = new MockZMessaging(selfUserId = users.head.id) {
      override lazy val sync = new EmptySyncService {
        override def syncUsers(id: UserId*) = {
          syncRequest = id.headOption
          super.syncUsers(id: _*)
        }
      }

      kvStorage.lastSlowSyncTimestamp = System.currentTimeMillis()
    }
  }

  after {
    syncRequest = None
    Await.result(storage.close(), 10.seconds)
    Robolectric.application.getDatabasePath(storage.dbHelper.getDatabaseName).delete()
  }

  scenario("load user") {
    zms.insertUsers(users)

    users foreach { u =>
      Await.result(service.getUser(u.id), timeout).map(_.copy(displayName = u.displayName)) shouldEqual Some(u)
    }

    Await.result(service.getUser(UserId()), timeout) shouldEqual None
  }

  scenario("update self user") {
    zms.insertUsers(users)
    val id = users.head.id
    Await.result(service.updateSyncedUsers(Seq(UserInfo(id))), timeout)
    Await.result(service.getSelfUser, timeout).map(_.connection) shouldEqual Some(ConnectionStatus.Self)
  }

  scenario("update other user") {
    zms.insertUsers(users)
    val id = users(2).id
    zms.dispatch(UserUpdateEvent(Uid(id.str), UserInfo(id)))
    withDelay {
      val user = zms.getUser(id)
      user should be (defined)
      user.get.connection shouldEqual ConnectionStatus.Unconnected
    }
  }

  scenario("schedule sync for stale user data") {
    zms.insertUsers(users)

    users foreach { u =>
      service.getUser(u.id)
      withDelay {
        syncRequest shouldEqual Some(u.id)
      }
    }
  }

  scenario("schedule sync for not existing user (when it's requested)") {
    val uid = UserId()
    Await.result(service.getUser(uid), timeout)
    syncRequest shouldEqual Some(uid)
  }

  feature("Update display name") {

    def getDisplayName(id: UserId) = zms.getUser(id).fold("")(_.getDisplayName)
    
    scenario("The display name of a newly created (unconnected) user should be the full name.") {
      val id = createUserNamed("some user")
      withDelay(getDisplayName(id) shouldEqual "some user")
    }

    scenario("Updating the name should update the display name, too.") {
      val id = createUserNamed("some user")
      service.updateOrCreateUser(id, _.copy(name = "other name"), UserData(id, "some user"))
      withDelay {
        getDisplayName(id) shouldEqual "other name"
      }
    }

    scenario("Updating from unconnected to connected should update the display name.") {
      val id = createUserNamed("some user")
      service.updateOrCreateUser(id, _.copy(connection = ConnectionStatus.Accepted), UserData(id, "some user"))
      withDelay { getDisplayName(id) shouldEqual "some" }
    }

    scenario("Same first name for multiple connected users should update both their display names.") {
      val id1 = connect(createUserNamed("some user"))
      val id2 = connect(createUserNamed("some other"))

      withDelay {
        getDisplayName(id2) shouldEqual "some o"
        getDisplayName(id1) shouldEqual "some u"
      }
    }

    scenario("Same first name and initials for multiple connected users should update both their display names.") {
      val id1 = connect(createUserNamed("some user"))
      val id2 = connect(createUserNamed("some other user"))

      withDelay {
        getDisplayName(id2) shouldEqual "some other user"
        getDisplayName(id1) shouldEqual "some user"
      }
    }

    scenario("Changing back from full names.") {
      val id1 = connect(createUserNamed("some user"))
      val id2 = connect(createUserNamed("some other user"))
      val id3 = connect(createUserNamed("some people"))

      service.updateUserData(id1, _.copy(name = "yeti girl"))

      withDelay {
        getDisplayName(id1) shouldEqual "yeti"
        getDisplayName(id3) shouldEqual "some p"
        getDisplayName(id2) shouldEqual "some u"
      }
    }

    scenario("multiple connected users with same first names and initials in a complex scenario") {
      createUserNamed("Meep Meep Unconnected")
      val id1 = connect(createUserNamed("bleep"))
      val id2 = connect(createUserNamed("some other"))
      createUserNamed("Another Unconnected")
      val id3 = connect(createUserNamed("some other user"))
      val id4 = connect(createUserNamed("yet another"))
      createUserNamed("Final Unconnected")

      withDelay {
        getDisplayName(id1) shouldEqual "bleep"
        getDisplayName(id2) shouldEqual "some o"
        getDisplayName(id3) shouldEqual "some u"
        getDisplayName(id4) shouldEqual "yet"
      }

      service.updateOrCreateUser(id1, _.copy(name = "some user"), UserData(id1, ""))

      withDelay {
        getDisplayName(id1) shouldEqual "some user"
        getDisplayName(id2) shouldEqual "some o"
        getDisplayName(id3) shouldEqual "some other user"
        getDisplayName(id4) shouldEqual "yet"
      }
    }
  }

  feature("Search results updating users") {

    def isConnected(id: UserId) = zms.getUser(id).map(_.isConnected)

    def createUserIds(): Seq[UserId] = Seq(connect(createUserNamed("connected")), createUserNamed("unconnected"), connect(createUserNamed("connected, too")))
    def createSearchEntries(ids: Seq[UserId]): Seq[UserSearchEntry] = ids zip Seq(Some(false), Some(true), None) map { case (id, connected) =>
        UserSearchEntry(id, id.str, None, None, -1, connected, blocked = false, Relation.Other, handle = None)
    }

    scenario("Update users from search, but keep the connection status.") {
      val userIds = createUserIds()
      Await.result(service.updateUsers(createSearchEntries(userIds)), timeout)
      withDelay { userIds map isConnected shouldEqual Seq(Some(true), Some(false), Some(true)) }
    }
  }

  def createUserNamed(name: String): UserId = returning(UserId()) { id => Await.result(service.updateOrCreateUser(id, identity[UserData], UserData(id, name)), timeout) }
  def connect(id: UserId): UserId = {
    Await.result(service.updateOrCreateUser(id, _.copy(connection = ConnectionStatus.Accepted), UserData(id, "")), timeout)
    id
  }

  feature("Update status") {

    scenario("Update user connection status") {
      val user = UserData("Some User").copy(connection = UserData.ConnectionStatus.Unconnected)
      zms.insertUsers(Seq(user))
      var changed = Seq.empty[UserData]
      zms.usersStorage.onChanged { changed = _ }

      Await.result(service.updateConnectionStatus(user.id, UserData.ConnectionStatus.PendingFromOther), timeout) should be('defined)

      zms.getUser(user.id).map(_.connection) shouldEqual Some(ConnectionStatus.PendingFromOther)
      changed.map(_.id) shouldEqual Seq(user.id)
    }

    scenario("Don't update the status if it was not changed") {
      val user = UserData("Some User").copy(connection = UserData.ConnectionStatus.PendingFromOther)
      zms.insertUsers(Seq(user))
      var changed = Seq.empty[UserData]
      zms.usersStorage.onChanged { changed = _ }

      Await.result(service.updateConnectionStatus(user.id, UserData.ConnectionStatus.PendingFromOther), timeout) should be('empty)

      zms.getUser(user.id).map(_.connection) shouldEqual Some(ConnectionStatus.PendingFromOther)
      changed should be('empty)
    }
  }

}
