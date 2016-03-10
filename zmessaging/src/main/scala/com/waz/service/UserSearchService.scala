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

import android.content.{ContentResolver, Context}
import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog._
import com.waz.api.impl.SearchQuery
import com.waz.api.impl.SearchQuery.{Named, RecommendedPeople, TopPeople}
import com.waz.content.{UsersStorage, ZStorage}
import com.waz.model.CommonConnectionsData.CommonConnectionsDataDao
import com.waz.model.SearchEntry.SearchEntryDao
import com.waz.model.SearchQueryCache.SearchQueryCacheDao
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events._

import scala.collection.breakOut
import scala.collection.immutable.Queue
import scala.concurrent.{Future, TimeoutException}

/**
 * Performs a search for users.
 * Searches for cached results and returns them immediately if found, very old results are discarded.
 * Schedules search sync from backend if needed (no cached result or stale cache).
 * If no cached result is found it waits a bit, and falls back to local search if the sync doesn't complete during wait.
 */
class UserSearchService(context: Context, storage: ZStorage, userService: UserService, usersStorage: UsersStorage, timeouts: Timeouts, sync: SyncServiceHandle) {
  import com.waz.service.UserSearchService._
  import timeouts.userSearch._
  import userService._
  import CancellableFuture.lift
  val contentResolver = context.getContentResolver

  val onSearchResultChanged = new SourceStream[SearchQuery.Query] with BgEventSource

  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[UserSearchService]

  private object dismissalLock
  private var recentlyDismissedPeopleYouMayKnow = Queue.empty[Long]

  val onCommonConnectionsChanged = EventStream[UserId]()

  def searchUserData(query: SearchQuery.DbQuery, limit: Int = -1): CancellableFuture[Seq[UserData]] = {
    searchUsers(query, limit) flatMap { ids =>
      CancellableFuture.traverse(ids) { id => CancellableFuture.lift(userService.getOrCreateUser(id)) } // TODO load in batch
    }
  }

  def searchUsers(query: SearchQuery.DbQuery, limit: Int = -1): CancellableFuture[Seq[UserId]] = {
    debug(s"search: '$query'")

    val queryLimit = computeQueryLimit(query, limit)

    loadCached(query, queryLimit) flatMap {
      case (cache, users) =>
        if (shouldRefresh(cache)) {
          storage.withTransaction { SearchQueryCacheDao.updateById(cache.id)(_.copy(limit = queryLimit))(_) }
          sync.syncSearchQuery(cache)
        }

        if (! cache.expired) CancellableFuture.successful(users)
        else { // was never synced, lets wait a bit and then try again with local search fallback
          waitForSyncOrLocalDelay(query, contentResolver) flatMap { _ =>
            CancellableFuture.lift(getSelfUserId) flatMap (self => loadCached(query, queryLimit, query match {
              case TopPeople         => UserDataDao.topPeople(queryLimit)(_)
              case RecommendedPeople => UserDataDao.recommendedPeople(queryLimit)(_)
              case Named(prefix)     => db => UserDataDao.search(SearchKey(prefix), self, queryLimit)(db).acquire(_.map(_.id).to[Vector])
            }).map(_._2))
          }
        }
    }
  }

  private def computeQueryLimit(query: SearchQuery.DbQuery, limit: Int): Option[Int] = if (limit <= -1) None else Some {
    val dismissedPeopleLeeway = if (query == RecommendedPeople) {
      val current = System.currentTimeMillis
      dismissalLock.synchronized {
        recentlyDismissedPeopleYouMayKnow = recentlyDismissedPeopleYouMayKnow.dropWhile { current - _ > cacheRefreshInterval.toMillis }
        recentlyDismissedPeopleYouMayKnow.size
      }
    } else 0
    verbose(s"compute query limit: $limit, leeway: $dismissedPeopleLeeway")
    limitQueryLimit(limit + dismissedPeopleLeeway)
  }

  def setCachedResults(query: SearchQuery.Query, results: Seq[SearchEntry]) = storage.withTransaction { implicit db =>
    val cache = SearchQueryCacheDao.get(query).getOrElse(SearchQueryCacheDao.add(query))
    val updated = SearchQueryCacheDao.update(cache)(_.copy(timestamp = System.currentTimeMillis()))

    verbose("setCachedResults: " + updated)

    SearchEntryDao.deleteEntries(cache)
    SearchEntryDao.insertOrReplace(results.map(_.copy(queryId = cache.id)))

    onSearchResultChanged ! cache.query
  }

  def updateSearchResults(cache: SearchQueryCache, results: Seq[UserSearchEntry]): Future[Boolean] =
    updateUsers(results) flatMap { updated =>
      syncIfNeeded(updated.toSeq: _*)

      storage.withTransaction { implicit db =>
        results foreach { u =>
          u.commonCount foreach { count =>
            updateCommonConnections(u.id, count, u.common)
          }
        }

        val c = SearchQueryCacheDao.update(cache)(_.copy(timestamp = System.currentTimeMillis()))
        debug(s"updated cache: $c")

        SearchEntryDao.deleteEntries(cache)
        SearchEntryDao.insertOrReplace(results.zipWithIndex.map { case (e, i) =>
          SearchEntry(cache.id, e.id, e.level.id, i) // TODO: do we need connection level here?? we have it in UserData already
        })

        onSearchResultChanged ! cache.query
        true
      }
    }

    def commonConnections(userId: UserId, fullList: Boolean): Signal[Option[CommonConnectionsData]] =
      Signal.wrap(0L, onCommonConnectionsChanged.filter(_ == userId).map(_ => System.currentTimeMillis())) flatMap { _ => Signal.future(getCommonConnections(userId, fullList)) }

    def getCommonConnections(userId: UserId, fullList: Boolean): CancellableFuture[Option[CommonConnectionsData]] = {
    def shouldRefreshCommonConnections(conn: CommonConnectionsData) = {
      def forceRefresh = conn.totalCount > conn.connections.size && (fullList || conn.connections.size < MinCommonConnections)
      conn.timestamp + cacheRefreshInterval.toMillis < System.currentTimeMillis() || forceRefresh
    }

    storage.withTransaction { implicit db =>
      val connections = CommonConnectionsDataDao.getById(userId)
      if (connections.isEmpty || shouldRefreshCommonConnections(connections.get)) sync.syncCommonConnections(userId)
      connections
    }
  }

  def excludeFromPymk(user: UserId): Future[Unit] =
    updateUserData(user, _.copy(excludeFromPymk = true)) map {
      _ foreach { _ =>
        dismissalLock.synchronized {
          recentlyDismissedPeopleYouMayKnow :+= System.currentTimeMillis
          verbose(s"recently dismissed: $recentlyDismissedPeopleYouMayKnow")
        }

        sync.postExcludePymk(user)
        onSearchResultChanged ! RecommendedPeople
      }
    }

  def searchQueryChangeSignal(query: SearchQuery.Query): Signal[Long] = Signal.wrap(0L, onSearchResultChanged.filter(_ == query).map(_ => System.currentTimeMillis()))

  private def loadCached(query: SearchQuery.Query, queryLimit: Option[Int], fallbackToDb: SQLiteDatabase => Seq[UserId] = _ => Nil): CancellableFuture[(SearchQueryCache, Seq[UserId])] = {
    debug(s"loadCached: '$query'")

    lift(storage.read { SearchQueryCacheDao.get(query, queryLimit)(_) }) flatMap (c => (query, c) match {
      case (RecommendedPeople, None) =>
        fallbackForRecommendedPeople(query, queryLimit, fallbackToDb)

      case (RecommendedPeople, Some(cache)) if cache.expired =>
        debug(s"found expired recommended people query cache: $cache")
        loadAddressBookUsers(queryLimit, fallbackToDb) map { (cache, _) }

      case (RecommendedPeople, Some(cache)) =>
        debug(s"found cached recommended people search query: $cache")
        lift(storage.read { SearchEntryDao.usersForQuery(cache)(_) }) flatMap { users =>
          (if (users.isEmpty) loadAddressBookUsers(queryLimit, fallbackToDb) else filterRecommendedPeople(users)) map { filtered =>
            storage { SearchQueryCacheDao.updateById(cache.id)(_.copy(limit = Some(limitQueryLimit(users.size))))(_) } // update cache entry limit to actual number of unfiltered retrieved users
            (cache, filtered)
          }
        }

      case (_, None) =>
        debug(s"search query not cached yet for query: '$query'")
        storage { db =>
          (SearchQueryCacheDao.add(query, queryLimit)(db), fallbackToDb(db))
        }

      case (_, Some(cache)) if cache.expired =>
        debug(s"found expired search query cache: $cache")
        storage { fallbackToDb } map { (cache, _) }

      case (_, Some(cache)) =>
        debug(s"found cached search query: $cache")
        storage { SearchEntryDao.usersForQuery(cache)(_) } map { (cache, _) }
    })
  }

  private def fallbackForRecommendedPeople(query: SearchQuery.Query, queryLimit: Option[Int], fallbackToDb: SQLiteDatabase => Seq[UserId]): CancellableFuture[(SearchQueryCache, Seq[UserId])] = {
    debug(s"recommended people search query not cached yet for limit $queryLimit")
    CancellableFuture.lift(storage.read { SearchQueryCacheDao.getLatest(query)(_) }) flatMap { // try again without limit
      case Some(cacheWithLowerLimit) => // some users found for a lower limit query, return users of lower limit query for now
        verbose(s"found query with lower limit in cache: ${cacheWithLowerLimit.limit}")
        lift(storage.read { SearchEntryDao.usersForQuery(cacheWithLowerLimit)(_) }) flatMap { users =>
          if (users.isEmpty || cacheWithLowerLimit.expired) {
            loadAddressBookUsers(queryLimit, fallbackToDb) map { (cacheWithLowerLimit, _) }
          } else {
            // depending on difference between actual size and originally requested limit, create new (expired) cache entry to trigger reloading
            // TODO only if not recently done this already (due to not enough results available on backend...)
            val shouldCreateNewCacheEntry = System.currentTimeMillis - cacheWithLowerLimit.timestamp > dismissedPeopleRefreshThrottling.toMillis &&
              queryLimit .map { limit => users.size + DismissedPeopleBuffer <= limit } .getOrElse(true)
            verbose(s"should create new cache entry: expected limit: $queryLimit, num results: ${users.size}, cached limit: ${cacheWithLowerLimit.limit}, buffer: $DismissedPeopleBuffer")
            if (shouldCreateNewCacheEntry) storage { SearchQueryCacheDao.add(query, queryLimit)(_) } map { cache => (cache, users) }
            else filterRecommendedPeople(users) map { filtered => (cacheWithLowerLimit, filtered) }
          }
        }
      case None => // fall back to address book search
        verbose("no query with any (lower) limit found, falling back to address book search")
        storage { SearchQueryCacheDao.add(query, queryLimit)(_) } flatMap { cache =>
          loadAddressBookUsers(queryLimit, fallbackToDb) map { (cache, _) }
        }
    }
  }

  private def limitQueryLimit(limit: Int) = math.min(100, math.max(1, limit))

  private def loadAddressBookUsers(queryLimit: Option[Int], fallbackToDb: SQLiteDatabase => Seq[UserId]): CancellableFuture[Seq[UserId]] = lift {
    storage.read {
      SearchQueryCacheDao.get(SearchQuery.AddressBook)(_)
    } flatMap {
      case None =>
        verbose("address book upload result not yet cached")
        storage.read(fallbackToDb)
      case Some(cache) if cache.expired =>
        verbose("address book upload result cache expired")
        storage.read(fallbackToDb)
      case Some(cache) =>
        verbose("found cached address book upload result")
        storage.read(SearchEntryDao.usersForQuery(cache)(_)).flatMap(filterRecommendedPeople).map(_.take(queryLimit.getOrElse(Int.MaxValue)))
    }
  }

  private def filterRecommendedPeople(ids: Seq[UserId]): CancellableFuture[Seq[UserId]] =
    lift(usersStorage.listAll(ids)) map { users =>
      val excluded: Set[UserId] = users.collect { case u if u.isConnected || u.excludeFromPymk => u.id }(breakOut)
      ids filterNot excluded
    }

  private def shouldRefresh(cache: SearchQueryCache) = cache.timestamp + cacheRefreshInterval.toMillis < System.currentTimeMillis()

  private def waitForSyncOrLocalDelay(query: SearchQuery.Query, cr: ContentResolver): CancellableFuture[Any] = {
    import com.waz.utils.events.EventContext.Implicits.global
    onSearchResultChanged.filter(_ == query).next.withTimeout(localSearchDelay).recover { case _ : TimeoutException => () }
  }

  def updateCommonConnections(user: UserId, connections: Seq[UserSearchEntry]): Future[CommonConnectionsData] =
    updateUsers(connections) flatMap { users =>
      syncIfNeeded(users.toSeq: _*)

      storage { updateCommonConnections(user, connections.size, connections.map(_.id))(_) }
    }

  private def updateCommonConnections(user: UserId, totalCount: Int, connections: Seq[UserId])(implicit db: SQLiteDatabase): CommonConnectionsData = {
    val conns = CommonConnectionsDataDao.getById(user).fold(CommonConnectionsData(user, totalCount, connections)) { prev =>
      val merged = if (totalCount == connections.size) connections else {
        val cs = connections.toSet
        connections ++ prev.connections.filterNot(cs)
      }
      prev.copy(totalCount = totalCount, connections = merged, timestamp = System.currentTimeMillis())
    }
    CommonConnectionsDataDao.insertOrReplace(conns)
    onCommonConnectionsChanged ! user
    conns
  }
}

object UserSearchService {
  val MinCommonConnections = 4
  val DismissedPeopleBuffer = 10 // number of people that have to be dismissed before a new query is sent to the backend
}
