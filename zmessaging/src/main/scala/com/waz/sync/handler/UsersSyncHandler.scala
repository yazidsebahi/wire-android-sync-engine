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
package com.waz.sync.handler

import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.content.{AssetsStorage, UsersStorage}
import com.waz.model._
import com.waz.service.UserService
import com.waz.sync.SyncResult
import com.waz.sync.client.UsersClient
import com.waz.threading.Threading
import com.waz.utils.events.EventContext

import scala.concurrent.Future

class UsersSyncHandler(assetSync: AssetSyncHandler, userService: UserService, usersStorage: UsersStorage, assets: AssetsStorage, usersClient: UsersClient) {
  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[UsersSyncHandler]
  private implicit val ec = EventContext.Global

  def syncUsers(ids: UserId*): Future[SyncResult] = usersClient.loadUsers(ids).future flatMap {
    case Right(users) =>
      userService.updateSyncedUsers(users) map { _ => SyncResult.Success }
    case Left(error) =>
      warn(s"load user request failed for: $ids")
      Future.successful(SyncResult(error))
  }

  def syncSelfUser(): Future[SyncResult] = usersClient.loadSelf().future flatMap {
    case Right(user) =>
      userService.updateSyncedUsers(IndexedSeq(user)) map { _ => SyncResult.Success }
    case Left(error) =>
      warn(s"load self request failed")
      Future.successful(SyncResult(error))
  }

  def postSelfUser(info: UserInfo): Future[SyncResult] = updatedSelfToSyncResult(usersClient.updateSelf(info))

  def postSelfPicture(): Future[SyncResult] = userService.getSelfUser flatMap {
    case Some(UserData(id, _, _, _, _, Some(assetId), _, _, _, _, _, _, _, _, _, _, _)) =>
      for {
        _ <- assetSync.postSelfImageAsset(RConvId(id.str), assetId).recover {case _ => warn("Failed to upload v2 picture")} //TODO Dean: stop posting to v2 after transition period
        res <- assetSync.uploadAssetData(assetId, public = true).future flatMap {
          case Right(uploaded) =>
            for {
              asset <- assets.get(assetId)
              res   <- updatedSelfToSyncResult(usersClient.updateSelf(UserInfo(id, picture = asset)))
            } yield res

          case Left(err) =>
            error(s"self picture upload asset $assetId failed: $err")
            Future.successful(SyncResult.failed())
        }
      } yield res
    case Some(UserData(id, _, _, _, _, None, _, _, _, _, _, _, _, _, _, _, _)) =>
      updatedSelfToSyncResult(usersClient.updateSelf(UserInfo(id, picture = None)))
    case _ => Future.successful(SyncResult.failed())
  }

  def syncConnectedUsers(): Future[SyncResult] = {
    usersStorage.contactNameParts.future flatMap { cs =>
      usersClient.loadUsers(cs.keys.toSeq)
    } flatMap {
      case Right(users) => userService.updateSyncedUsers(users).map {_ => SyncResult.Success }
      case Left(error) =>
        warn(s"UsersClient.loadUsers failed")
        Future.successful(SyncResult(error))
    }
  }

  def deleteAccount(): Future[SyncResult] = usersClient.deleteAccount() map {
    case Right(()) => SyncResult.Success
    case Left(error) =>
      warn(s"Account deletion failed: $error")
      SyncResult(error)
  }

  private def updatedSelfToSyncResult(updatedSelf: Future[Either[ErrorResponse, Unit]]): Future[SyncResult] =
    updatedSelf map (_.fold[SyncResult](SyncResult(_), _ => SyncResult.Success))
}
