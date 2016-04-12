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
import com.waz.service.PushService.SlowSyncRequest
import com.waz.service.UserService._
import com.waz.service.images.ImageAssetService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.sync.client.{CredentialsUpdateClient, UsersClient}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{AggregatingSignal, EventContext, Signal}
import com.waz.utils._
import com.waz.znet.ZNetClient.ErrorOrResponse

import scala.collection.breakOut
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

class UserService(user: ZUserService, usersStorage: UsersStorage, keyValueService: KeyValueService, push: PushService, instance: InstanceService,
                  assets: ImageAssetService, client: CredentialsUpdateClient, usersClient: UsersClient, sync: SyncServiceHandle, userCallbacks: UserDataCallbacks, lifecycle: ZmsLifecycle) {

  private implicit val logTag: LogTag = logTagFor[UserService]
  import Threading.Implicits.Background
  private implicit val ec = EventContext.Global
  import CancellableFuture.lift
  import keyValueService._

  val selfUserId = keyValuePref[UserId](KeyValueService.SelfUserIdKey, SelfUserId)

  val selfUser: Signal[UserData] = selfUserId.signal flatMap {
    case SelfUserId => Signal.empty[UserData]
    case id => usersStorage.signal(id)
  }

  val userUpdateEventsStage = EventScheduler.Stage[UserUpdateEvent]((c, e) => updateSyncedUsers(e.map(_.user)(breakOut)))
  val userDeleteEventsStage = EventScheduler.Stage[UserDeleteEvent]((c, e) => updateUserDeleted(e.map(_.user)(breakOut)))

  push.onSlowSyncNeeded { case SlowSyncRequest(time, _) =>
    verbose(s"onSlowSyncNeeded, updating timestamp to: $time")
    lastSlowSyncTimestamp = time

    sync.syncSelfUser().map(dependency => sync.syncConnections(Some(dependency)))
  }

  lazy val acceptedUsers: Signal[Map[UserId, UserData]] = new AggregatingSignal[Seq[UserData], Map[UserId, UserData]](usersStorage.onChanged, usersStorage.listAcceptedUsers, { (accu, us) =>
    val (toAdd, toRemove) = us.partition(_.connection == ConnectionStatus.Accepted)
    accu -- toRemove.map(_.id) ++ toAdd.map(u => u.id -> u)
  })

  def withSelfUser[A](f: UserId => CancellableFuture[A])(implicit ec: ExecutionContext) = getSelfUserId .flatMap {
    case Some(id) => f(id)
    case None =>
      error("withSelfUser called, but no self user id is available")
      CancellableFuture.failed(new NoSuchElementException("No self user id available") with NoStackTrace)
  } (ec)

  def withSelfUserFuture[A](f: UserId => Future[A])(implicit ec: ExecutionContext) = getSelfUserId .flatMap {
    case Some(id) => f(id)
    case None =>
      error("withSelfUser called, but no self user id is available")
      Future.failed(new NoSuchElementException("No self user id available") with NoStackTrace)
  } (ec)

  def selfUserOrFail: Future[UserId] = withSelfUserFuture(Future(_))

  def getOrCreateUser(id: UserId) = usersStorage.getOrElseUpdate(id, {
    sync.syncUsers(id)
    UserData(id, defaultUserName, None, None, connection = ConnectionStatus.Unconnected, searchKey = SearchKey(defaultUserName))
  })

  def getSelfUserId: Future[Option[UserId]] = selfUserId() map {
    case SelfUserId => None
    case id => Some(id)
  }

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

    def loadUser(id: UserId) =
      lift(usersStorage.get(id)) map {
        case Some(data) =>
          syncIfNeeded(data)
          Some(data)
        case _ =>
          if (id == SelfUserId) sync.syncSelfUser() else sync.syncUsers(id)
          None
      }

    def loadSelf() = getSelfUserId flatMap {
      case Some(selfId) => loadUser(selfId)
      case None =>
        sync.syncSelfUser() map { _ => None }
    }

    if (id == SelfUserId) loadSelf() else loadUser(id)
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

  def syncSelfNow: Future[Option[UserId]] = {
    syncSelfFuture.getOrElse {
      val res = usersClient.loadSelf().future.flatMap {
        case Right(info) =>
          updateSelf(info, emailVerified = info.email.isDefined, phoneVerified = info.phone.isDefined) map (Some(_))
        case Left(err) =>
          error(s"loadSelf() failed: $err")
          Future.successful(None)
      }
      res.onComplete { res => syncSelfFuture = None }
      syncSelfFuture = Some(res)
      res
    }
  }

  def deleteAccount(): Future[SyncId] = sync.deleteAccount()

  @volatile private var syncSelfFuture: Option[Future[Option[UserId]]] = None

  def withSelfUserId(body: UserId => CancellableFuture[Option[UserData]])(implicit ec: ExecutionContext) =
    getSelfUserId .flatMap {
      case Some(id) => body(id)
      case _ => lift(syncSelfNow) .flatMap {
        case Some(id) => body(id)
        case None =>
          error("syncSelf failed")
          CancellableFuture.successful(None)
      } (ec)
    } (ec)

  def getSelfUser: Future[Option[UserData]] =
    withSelfUserId { selfUser =>
      lift(usersStorage.get(selfUser)) flatMap {
        case Some(userData) => CancellableFuture.successful(Some(userData))
        case _ => lift(syncSelfNow) flatMap {
          case Some(id) => lift(usersStorage.get(id))
          case _ => CancellableFuture.successful(None)
        }
      }
    }

  def updateAndSync(userId: UserId, updater: UserData => UserData, sync: UserData => Future[_]) =
    updateUserData(userId, updater) flatMap {
      case Some((p, u)) if p != u => sync(u) map (_ => Some(u))
      case _ => Future successful None
    }

  def updateSelfAndSync(updater: UserData => UserData, sync: UserData => Future[_]) =
    withSelfUserId { id => lift(updateAndSync(id, updater, sync)) }

  // called from ui to update user
  def updateSelf(name: Option[String] = None, phone: Option[PhoneNumber] = None, accent: Option[AccentColor] = None): Future[Option[UserData]] =
    updateSelfAndSync(_.updated(name, None, phone, accent), data => sync.postSelfUser(UserInfo(data.id, name, accent.map(_.id), phone = phone)))

  def clearSelfPicture(): Future[Option[UserData]] =
    updateSelfAndSync(_.copy(picture = None), _ => sync.postSelfPicture(None))

  def requestVerificationEmail(email: EmailAddress) = usersClient.requestVerificationEmail(email)

  def updateSelfPicture(image: com.waz.api.ImageAsset): Future[Option[UserData]] =
    withSelfUserFuture { userId =>
      assets.addImageAsset(AssetId(), image, RConvId(userId.str), isSelf = true) flatMap { asset =>
        updateAndSync(userId, _.copy(picture = Some(asset.id)), _ => sync.postSelfPicture(Some(asset.id)))
      }
    }

  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit] = client.updateEmail(email)
  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit] = client.updatePhone(phone)
  def updatePassword(newPassword: String, currentPassword: Option[String]): ErrorOrResponse[Unit] = client.updatePassword(newPassword, currentPassword)

  def updateSelf(data: UserInfo, emailVerified: Boolean, phoneVerified: Boolean): Future[UserId] =
    updateSyncedSelfUser(data, emailVerified, phoneVerified) map { _ =>
      selfUserId := data.id
      data.id
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

  def updateSyncedUsersPictures(users: UserInfo*): Future[_] = assets.updateImageAssets(users.flatMap(_.picture.filter(_.nonEmpty)))

  private def updateSyncedSelfUser(selfInfo: UserInfo, emailVerified: Boolean, phoneVerified: Boolean, timestamp: Long = System.currentTimeMillis()): Future[UserData] = {
    debug(s"updateSyncedUser $selfInfo, pic: ${if (selfInfo.picture.contains(ImageAssetData.Empty)) "Empty" else selfInfo.picture.toString}")
    updateSyncedUsersPictures(selfInfo) flatMap { _ =>
      val updater: UserData => UserData  = { _.copy(syncTimestamp = timestamp, connection = ConnectionStatus.Self) }
      updateOrCreateUser(selfInfo.id, u => updater(u.updated(selfInfo)), updater(UserData(selfInfo))) map { data =>
        selfInfo.email foreach (userCallbacks.updateZUserEmail(user.user, _, verified = emailVerified))
        selfInfo.phone foreach (userCallbacks.updateZUserPhone(user.user, _, verified = phoneVerified))
        data
      }
    }
  }

  def updateSyncedUsers(users: IndexedSeq[UserInfo], timestamp: Long = System.currentTimeMillis()): Future[Set[UserData]] = {
    debug(s"update synced users: $users, service: $this")
    updateSyncedUsersPictures(users: _*) flatMap { _ =>
      def updateOrCreate(info: UserInfo): Option[UserData] => UserData = {
        case Some(user: UserData) => user.updated(info).copy(syncTimestamp = timestamp)
        case None => UserData(info).copy(syncTimestamp = timestamp)
      }
      for {
        userData <- usersStorage.updateOrCreateAll(users.map { info => info.id -> updateOrCreate(info) }.toMap)
        maybeSelf <- getSelfUserId
        _ = maybeSelf flatMap (selfId => users.find(_.id == selfId)) foreach { self =>
          self.email foreach (userCallbacks.updateZUserEmail(user.user, _, verified = true))
          self.phone foreach (userCallbacks.updateZUserPhone(user.user, _, verified = true))
        }
      } yield userData
    }
  }

  def updateUserDeleted(userIds: Vector[UserId]): Future[Any] = withSelfUserFuture { selfId =>
    Future.traverse(userIds) { userId =>
      if (userId == selfId) Future {
        debug(s"self account deleted, deleting cookie and logging out...")
        user.updateCookie(None)
        instance.logout(user.userId)
      } else {
        debug(s"contact account deleted, setting deleted flag...")
        usersStorage.update(userId, _.copy(deleted = true))
      }
    }
  }
}

object UserService {
  val SelfUserId = UserId(Uid(0, 0).str)

  val defaultUserName: String = ""
}
