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

import com.waz.ZLog._
import com.waz.api.impl.AccentColor
import com.waz.content._
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service.UserService._
import com.waz.service.assets.AssetService
import com.waz.service.push.PushService.SlowSyncRequest
import com.waz.service.push.PushServiceSignals
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.sync.client.UsersClient
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import com.waz.utils.events.{AggregatingSignal, EventContext, Signal}

import scala.collection.breakOut
import scala.concurrent.{Awaitable, Future}

class UserService(val selfUserId: UserId, usersStorage: UsersStorage, keyValueService: KeyValueStorage, push: PushServiceSignals,
                  assets: AssetService, usersClient: UsersClient, sync: SyncServiceHandle, assetsStorage: AssetsStorage) {

  private implicit val logTag: LogTag = logTagFor[UserService]
  import Threading.Implicits.Background
  private implicit val ec = EventContext.Global
  import keyValueService._

  val selfUser: Signal[UserData] = usersStorage.optSignal(selfUserId) flatMap {
    case Some(data) => Signal.const(data)
    case None =>
      sync.syncSelfUser()
      Signal.empty
  }

  val userUpdateEventsStage = EventScheduler.Stage[UserUpdateEvent]((c, e) => updateSyncedUsers(e.map(_.user)(breakOut)))
  val userDeleteEventsStage = EventScheduler.Stage[UserDeleteEvent]((c, e) => updateUserDeleted(e.map(_.user)(breakOut)))

  push.onSlowSyncNeeded { case SlowSyncRequest(time, _) =>
    verbose(s"onSlowSyncNeeded, updating timestamp to: $time")
    lastSlowSyncTimestamp = time

    sync.syncSelfUser().map(dependency => sync.syncConnections(Some(dependency)))
  }

  lazy val acceptedOrBlockedUsers: Signal[Map[UserId, UserData]] = new AggregatingSignal[Seq[UserData], Map[UserId, UserData]](usersStorage.onChanged, usersStorage.listUsersByConnectionStatus(acceptedOrBlocked), { (accu, us) =>
    val (toAdd, toRemove) = us.partition(u => acceptedOrBlocked(u.connection))
    accu -- toRemove.map(_.id) ++ toAdd.map(u => u.id -> u)
  })

  private lazy val acceptedOrBlocked = Set(ConnectionStatus.Accepted, ConnectionStatus.Blocked)

  def withSelfUser[A](f: UserId => CancellableFuture[A]) = f(selfUserId)

  def withSelfUserFuture[A](f: UserId => Future[A]) = f(selfUserId)

  def selfUserOrFail: Future[UserId] = withSelfUserFuture(Future(_))

  def getOrCreateUser(id: UserId) = usersStorage.getOrElseUpdate(id, {
    sync.syncUsers(id)
    UserData(id, defaultUserName, None, None, connection = ConnectionStatus.Unconnected, searchKey = SearchKey(defaultUserName))
  })

  def getSelfUserId: Future[Option[UserId]] = Future successful Some(selfUserId)

  def updateOrCreateUser(id: UserId, update: UserData => UserData, create: => UserData) =
    usersStorage.updateOrCreate(id, update, create)

  def updateUserConversation(id: UserId, convId: RConvId) = usersStorage.update(id, _.copy(conversation = Some(convId)))

  def updateConnectionStatus(id: UserId, status: UserData.ConnectionStatus, time: Option[Date] = None, message: Option[String] = None): Future[Option[UserData]] =
    usersStorage.update(id, { user => returning(user.updateConnectionStatus(status, time, message))(u => verbose(s"updateConnectionStatus($u)")) }) map {
      case Some((prev, updated)) if prev != updated => Some(updated)
      case _ => None
    }

  def updateUserData(id: UserId, updater: UserData => UserData) = usersStorage.update(id, updater)

  def updateUsers(entries: Seq[UserSearchEntry]) = {
    def updateOrAdd(entry: UserSearchEntry) = (_: Option[UserData]).fold(UserData(entry))(_.updated(entry))
    usersStorage.updateOrCreateAll(entries.map(entry => entry.id -> updateOrAdd(entry)).toMap)
  }

  def getUser(id: UserId): Future[Option[UserData]] = {
    debug(s"getUser($id)")

    usersStorage.get(id) map {
      case Some(data) =>
        syncIfNeeded(data)
        Some(data)
      case _ =>
        sync.syncUsers(id)
        None
    }
  }

  def userSignal(id: UserId) =
    usersStorage.optSignal(id) flatMap {
      case None =>
        sync.syncUsers(id)
        Signal.empty[UserData]
      case Some(data) =>
        syncIfNeeded(data)
        Signal const data
    }

  def syncSelfNow: Future[Option[UserData]] = Serialized.future("syncSelfNow", selfUserId) {
    usersClient.loadSelf().future.flatMap {
      case Right(info) =>
        //TODO Dean - remove after v2 transition time
        info.picture.filter(_.convId.isDefined).fold(Future.successful(())) { asset =>
            verbose("User has v2 picture - re-uploading as v3")
            for {
              _ <- sync.postSelfPicture(Some(asset.id))
              _ <- assetsStorage.updateAsset(asset.id, _.copy(convId = None)) //mark asset as v3
              _ <- usersClient.updateSelf(info)
            } yield (())
        }.flatMap (_ => updateSyncedUsers(Seq(info)) map { _.headOption })
      case Left(err) =>
        error(s"loadSelf() failed: $err")
        Future.successful(None)
    }
  }

  def deleteAccount(): Future[SyncId] = sync.deleteAccount()

  def withSelfUserId[T <: Awaitable[Option[UserData]]](f: UserId => T) = f(selfUserId)

  def getSelfUser: Future[Option[UserData]] =
    usersStorage.get(selfUserId) flatMap {
      case Some(userData) => Future successful Some(userData)
      case _ => syncSelfNow
    }

  def updateAndSync(userId: UserId, updater: UserData => UserData, sync: UserData => Future[_]) =
    updateUserData(userId, updater) flatMap {
      case Some((p, u)) if p != u => sync(u) map (_ => Some(u))
      case _ => Future successful None
    }

  def updateSelfAndSync(updater: UserData => UserData, sync: UserData => Future[_]) =
    updateAndSync(selfUserId, updater, sync)

  // called from ui to update user
  def updateSelf(name: Option[String] = None, phone: Option[PhoneNumber] = None, accent: Option[AccentColor] = None): Future[Option[UserData]] =
    updateSelfAndSync(_.updated(name, None, phone, accent), data => sync.postSelfUser(UserInfo(data.id, name, accent.map(_.id), phone = phone)))

  def clearSelfPicture(): Future[Option[UserData]] =
    updateSelfAndSync(_.copy(picture = None), _ => sync.postSelfPicture(None))

  def updateSelfPicture(image: com.waz.api.ImageAsset): Future[Option[UserData]] =
    assets.addImageAsset(image, RConvId(selfUserId.str), isSelf = true) flatMap { asset =>
      updateAndSync(selfUserId, _.copy(picture = Some(asset.id)), _ => sync.postSelfPicture(Some(asset.id)))
    }

  def getUsers(ids: Seq[UserId]): Future[Seq[UserData]] =
    usersStorage.listAll(ids) map { users =>
      syncIfNeeded(users: _*)
      users
    }

  /**
   * Schedules user data sync if user with given id doesn't exist or has old timestamp.
   */
  def syncNotExistingOrExpired(users: Seq[UserId]): Future[Unit] = usersStorage.listAll(users) flatMap { found =>
    val toSync = (users.toSet -- found.map(_.id)).toSeq
    if (toSync.nonEmpty) sync.syncUsers(toSync: _*) flatMap (_ => syncIfNeeded(found: _*)) else syncIfNeeded(found: _*)
  }

  /**
    * Schedules user data sync if stored user timestamp is older than last slow sync timestamp.
   */
  def syncIfNeeded(users: UserData*): Future[Unit] =
    lastSlowSyncTimestamp flatMap {
      case Some(time) => sync.syncUsersIfNotEmpty(users.filter(_.syncTimestamp < time).map(_.id))
      case _ => Future.successful(())
    }

  def updateSyncedUsersPictures(users: UserInfo*): Future[_] = assets.updateAssets(users.flatMap(_.picture) ++ users.flatMap(_.picture))

  def updateSyncedUsers(users: Seq[UserInfo], timestamp: Long = System.currentTimeMillis()): Future[Set[UserData]] = {
    debug(s"update synced users: $users, service: $this")
    updateSyncedUsersPictures(users: _*) flatMap { _ =>
      def updateOrCreate(info: UserInfo): Option[UserData] => UserData = {
        case Some(user: UserData) => user.updated(info).copy(syncTimestamp = timestamp, connection = if (selfUserId == info.id) ConnectionStatus.Self else user.connection)
        case None => UserData(info).copy(syncTimestamp = timestamp, connection = if (selfUserId == info.id) ConnectionStatus.Self else ConnectionStatus.Unconnected)
      }
      usersStorage.updateOrCreateAll(users.map { info => info.id -> updateOrCreate(info) }(breakOut))
    }
  }

  def updateUserDeleted(userIds: Vector[UserId]): Future[Any] =
    usersStorage.updateAll2(userIds, _.copy(deleted = true))
}

object UserService {
  val defaultUserName: String = ""
}
