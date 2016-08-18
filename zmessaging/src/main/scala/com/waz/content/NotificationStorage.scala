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
package com.waz.content

import android.content.Context
import com.waz.content.ContentChange.{Added, Removed, Updated}
import com.waz.model.NotificationData
import com.waz.model.NotificationData.NotificationDataDao
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.{AggregatingSignal, EventStream}
import com.waz.utils.{CachedStorage, TrimmingLruCache}

import scala.collection._

class NotificationStorage(context: Context, storage: Database) extends CachedStorage[String, NotificationData](new TrimmingLruCache(context, Fixed(128)), storage)(NotificationDataDao, "NotificationStorage") {
  import com.waz.threading.Threading.Implicits.Background

  val changesStream = EventStream.union[Seq[ContentChange[Any]]]( // FIXME: improve types, Any looks ugly
    onAdded.map(_.map(Added(_))),
    onUpdated.map(_.map { case (v, v1) => Updated(v, v1) }),
    onDeleted.map(_.map(Removed(_)))
  )

  // signal with all data
  val notifications = new AggregatingSignal[Seq[ContentChange[Any]], Map[String, NotificationData]](changesStream, list().map(_.map { n => n.id -> n } (breakOut)), { (values, changes) =>
    val added = new mutable.HashMap[String, NotificationData]
    val removed = new mutable.HashSet[String]
    changes foreach {
      case Added(v: NotificationData) =>
        removed -= v.id
        added += v.id -> v
      case Updated(_, v: NotificationData) =>
        removed -= v.id
        added += v.id -> v
      case Removed(id: String) =>
        removed += id
        added -= id
      case _ => // ignore, shouldn't happen
    }
    values -- removed ++ added
  })
}
