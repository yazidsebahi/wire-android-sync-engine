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
package com.waz.model

import android.content.ContentValues
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import com.waz.api.impl.SearchQuery
import com.waz.db.Col._
import com.waz.db.{BaseDao, Dao}

import scala.concurrent.duration._

case class SearchQueryCache(id: Long, query: SearchQuery.Query, limit: Option[Int], timestamp: Long) {
  def expired = timestamp + SearchQueryCache.CacheExpiryTime.toMillis < System.currentTimeMillis()
}

case class SearchEntry(queryId: Long, userId: UserId, level: Int, order: Int = 1)

object SearchQueryCache {
  val CacheExpiryTime = 7.days // time after which the cache expires (results are considered invalid, and will be ignored)

  implicit object SearchQueryCacheDao extends Dao[SearchQueryCache, Long] {
    val Id = long('_id, "PRIMARY KEY")(_.id)
    val Query = text('query)(_.query.cacheKey)
    val Limit = opt(int('query_limit))(_.limit)
    val Timestamp = long('timestamp)(_.timestamp)

    override val idCol = Id

    override val table = Table("SearchQueries", Id, Query, Limit, Timestamp)

    override def apply(implicit cursor: Cursor): SearchQueryCache = SearchQueryCache(Id, SearchQuery.fromCacheKey(Query), Limit, Timestamp)

    def add(query: SearchQuery.Query, limit: Option[Int] = None)(implicit db: SQLiteDatabase): SearchQueryCache = {
      val values = new ContentValues()
      Query.col.save(query.cacheKey, values)
      Limit.col.save(limit, values)
      Timestamp.col.save(0, values)
      SearchQueryCache(db.insert(table.name, null, values), query, limit, 0)
    }

    def get(query: SearchQuery.Query, limit: Option[Int] = None)(implicit db: SQLiteDatabase): Option[SearchQueryCache] = {
      def emptyLimitQuery = single(db.query(table.name, null, s"${Query.name} = ? AND ${Limit.name} IS NULL", Array(query.cacheKey), null, null, s"${Timestamp.name} DESC"))
      def limitQuery(limit: Int) = single(db.query(table.name, null, s"${Query.name} = ? AND (${Limit.name} IS NULL OR ${Limit.name} >= $limit)", Array(query.cacheKey), null, null, s"${Timestamp.name} DESC"))

      limit.fold(emptyLimitQuery)(limit => limitQuery(limit))
    }

    def getLatest(query: SearchQuery.Query)(implicit db: SQLiteDatabase): Option[SearchQueryCache] = {
      val expiredThreshold = System.currentTimeMillis() - CacheExpiryTime.toMillis
      single(db.query(table.name, null, s"${Query.name} = ? AND ${Timestamp.name} > $expiredThreshold", Array(query.cacheKey), null, null, s"${Timestamp.name} DESC"))
    }
  }
}

object SearchEntry {

  implicit object SearchEntryDao extends BaseDao[SearchEntry] {
    val QueryId = long('query_id)(_.queryId)
    val UserId = id[UserId]('user_id).apply(_.userId)
    val Level = int('level)(_.level)
    val Order = int('entry_order)(_.order)

    val DefaultLimit = "1000" // huge number, in practice will not be limiting at all
    
    override val table = Table("SearchEntries", QueryId, UserId, Level, Order)

    override def apply(implicit cursor: Cursor): SearchEntry = SearchEntry(QueryId, UserId, Level, Order)

    def findEntries(q: SearchQueryCache, limit: Option[Int] = None)(implicit db: SQLiteDatabase): IndexedSeq[SearchEntry] =
      list(db.query(table.name, null, s"${QueryId.name} = ?", Array(q.id.toString), null, null, s"${Order.name} ASC", limit.fold(DefaultLimit)(_.toString)))

    def deleteEntries(q: SearchQueryCache)(implicit db: SQLiteDatabase) = delete(QueryId, q.id)
    
    def usersForQuery(q: SearchQueryCache, limit: Option[Int] = None)(implicit db: SQLiteDatabase): IndexedSeq[UserId] =
      list(db.query(table.name, null, s"${QueryId.name} = ?", Array(q.id.toString), null, null, s"${Order.name} ASC", limit.fold(DefaultLimit)(_.toString))) map (_.userId)
  }
}
