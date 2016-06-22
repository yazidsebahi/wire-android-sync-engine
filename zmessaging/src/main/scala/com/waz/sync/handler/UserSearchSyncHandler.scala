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
import com.waz.content.ZmsDatabase
import com.waz.model.SearchQueryCache.SearchQueryCacheDao
import com.waz.model.UserId
import com.waz.service.UserSearchService
import com.waz.sync.SyncResult
import com.waz.sync.client.UserSearchClient
import com.waz.threading.Threading

import scala.concurrent.Future

object UserSearchSyncHandler {
  val DefaultLimit = 50
}

class UserSearchSyncHandler(storage: ZmsDatabase, userSearchService: UserSearchService, client: UserSearchClient) {
  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[UserSearchSyncHandler]

  def syncSearchQuery(cacheId: Long): Future[SyncResult] = storage.read {
    SearchQueryCacheDao.getById(cacheId)(_)
  } flatMap {
    case None =>
      error(s"SearchQueryCache not found with id: $cacheId")
      Future successful SyncResult.Failure(None, shouldRetry = false)
    case Some(cache) =>
      debug(s"starting sync for: $cache")
      client.graphSearch(cache.query, cache.limit.getOrElse(UserSearchSyncHandler.DefaultLimit)).future flatMap {
        case Right(results) =>
          debug(s"searchSync, got: $results")
          userSearchService.updateSearchResults(cache, results) map (SyncResult(_))
        case Left(error) =>
          warn("graphSearch request failed")
          Future successful SyncResult(error)
      }
  }

  def syncCommonConnections(user: UserId): Future[SyncResult] = {
    debug(s"starting sync commonConnections for UserId: $user")
    client.loadCommonConnections(user).future flatMap {
      case Right(results) => userSearchService.updateCommonConnections(user, results) map (_ => SyncResult.Success)
      case Left(error) =>
        warn("loadCommonConnections request failed")
        Future successful SyncResult(error)
    }
  }
  
  def postExcludePymk(user: UserId): Future[SyncResult] =
    client.postExcludePymk(user).future map {
      case None => SyncResult.Success
      case Some(error) => SyncResult(error)
    }
}
