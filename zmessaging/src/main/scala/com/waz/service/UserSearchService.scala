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

import android.content.Context
import com.waz.ZLog._
import com.waz.content.{CommonConnectionsStorage, SearchQueryCacheStorage, UsersStorage}
import com.waz.model.SearchQuery.{Recommended, TopPeople}
import com.waz.model.UserData.{ConnectionStatus, UserDataDao}
import com.waz.model.{SearchQuery, _}
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.Future.traverse

class UserSearchService(context: Context, queryCache: SearchQueryCacheStorage, commonConnsStorage: CommonConnectionsStorage,
    userService: UserService, usersStorage: UsersStorage, timeouts: Timeouts, sync: SyncServiceHandle) {

  import Threading.Implicits.Background
  import com.waz.service.UserSearchService._
  import timeouts.search._

  val onCommonConnectionsChanged = EventStream[UserId]()

  def searchUserData(query: SearchQuery, limit: Option[Int] = None): Signal[SeqMap[UserId, UserData]] =
    queryCache.optSignal(query).flatMap {
      case r if r.forall(cached => (cacheExpiryTime elapsedSince cached.timestamp) || cached.entries.isEmpty) =>
        verbose(s"no cached entries for query $query")

        Signal.future(refresh(query).flatMap(_ => query match {
          case TopPeople =>
            usersStorage.find[UserData, Vector[UserData]](u => ! u.deleted && u.connection == ConnectionStatus.Accepted, db => UserDataDao.topPeople(db), identity)
          case Recommended(prefix) =>
            val key = SearchKey(prefix)
            usersStorage.find[UserData, Vector[UserData]](u => ! u.deleted && (u.relation == Relation.Second || u.relation == Relation.Third) && key.isAtTheStartOfAnyWordIn(u.searchKey), db => UserDataDao.recommendedPeople(key)(db), identity)
        }).logFailure())
      case Some(cached) =>
        verbose(s"query $query cached")
        if ((cacheRefreshInterval elapsedSince cached.timestamp)) refresh(query)
        cached.entries.fold2(Signal.const(Vector.empty[UserData]), ids => usersStorage.listSignal(ids))
    }.map { users =>
      query match {
        case TopPeople =>
          users filter (u => ! u.deleted && u.connection == ConnectionStatus.Accepted)
        case Recommended(prefix) =>
          val key = SearchKey(prefix)
          users filter (u => ! u.deleted && ! u.isConnected && key.isAtTheStartOfAnyWordIn(u.searchKey))
      }
    }.map(users => SeqMap(limit.fold2(users, users.take))(_.id, identity))

  private def refresh(query: SearchQuery): Future[SyncId] = {
    verbose(s"refresh $query")
    queryCache.updateOrCreate(query, old => old, SearchQueryCache(query, Instant.now, None)).flatMap(cache => sync.syncSearchQuery(cache))
  }

  def updateSearchResults(query: SearchQuery, results: Seq[UserSearchEntry]): Future[Unit] =
    for {
      updated <- userService.updateUsers(results)
      _       <- userService.syncIfNeeded(updated.toSeq: _*)
      _       <- traverse(results)(u => u.commonCount.mapFuture(c => updateCommonConnections(u.id, c, u.common)))
      _       <- queryCache.update(query, _.copy(timestamp = Instant.now, entries = Some(results.map(_.id)(breakOut))))
    } yield ()

  def commonConnections(userId: UserId, fullList: Boolean): Signal[Option[CommonConnectionsData]] = {
    def shouldRefreshCommonConnections(conn: CommonConnectionsData) = {
      def forceRefresh = conn.totalCount > conn.connections.size && (fullList || conn.connections.size < MinCommonConnections)
      cacheRefreshInterval.elapsedSince(conn.timestamp) || forceRefresh
    }
    commonConnsStorage.optSignal(userId).map { conns =>
      if (conns.forall(shouldRefreshCommonConnections)) sync.syncCommonConnections(userId)
      conns
    }
  }
  def updateCommonConnections(user: UserId, connections: Seq[UserSearchEntry]): Future[CommonConnectionsData] =
    for {
      updated <- userService.updateUsers(connections)
      _       <- userService.syncIfNeeded(updated.toSeq: _*)
      conns   <- updateCommonConnections(user, connections.size, connections.map(_.id))
    } yield conns

  private def updateCommonConnections(user: UserId, totalCount: Int, connections: Seq[UserId]): Future[CommonConnectionsData] =
    commonConnsStorage.updateOrCreate(user, { prev =>
      val merged = if (totalCount == connections.size) connections else {
        val cs = connections.toSet
        connections ++ prev.connections.filterNot(cs)
      }
      prev.copy(totalCount = totalCount, connections = merged, timestamp = Instant.now)
    }, CommonConnectionsData(user, totalCount, connections))
}

object UserSearchService {
  private implicit val tag: LogTag = logTagFor[UserSearchService]
  val MinCommonConnections = 4
}
