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
package com.waz.service.assets

import scala.collection.mutable

class DownloadQueue {

  import DownloadQueue._

  val entries = new mutable.HashMap[DownloadKey, Entry]
  val queue = new mutable.PriorityQueue[Entry]()(EntryOrdering)

  def put(key: DownloadKey, uiWaiting: Boolean = true, timestamp: Long = System.currentTimeMillis()) = {
    val entry = Entry(key, uiWaiting, timestamp)
    entries.put(key, entry) foreach { _.deleted = true }
    queue += entry
  }

  private def dropDeleted() = while (queue.nonEmpty && queue.head.deleted) queue.dequeue() // remove deleted

  def isEmpty = {
    dropDeleted()
    queue.isEmpty
  }

  def poll(): Option[DownloadKey] = {
    dropDeleted()
    if (queue.isEmpty) None
    else Some(queue.dequeue().key)
  }

  def peek(): Option[DownloadKey] = {
    dropDeleted()
    queue.headOption.map(_.key)
  }
}

object DownloadQueue {

  case class Entry(key: DownloadKey, uiWaiting: Boolean, timestamp: Long) {
    private[assets] var deleted = false
  }

  implicit object EntryOrdering extends Ordering[Entry] {
    override def compare(x: Entry, y: Entry): Int = {
      if (x.uiWaiting == y.uiWaiting) {
        // if ui is waiting then choose older entry, otherwise choose newer
        // the ide is that if ui is no longer waiting for download then
        // it's more probable that last recently used item will requested again sooner than some older item
        if (x.uiWaiting) Ordering.Long.compare(y.timestamp, x.timestamp)
        else Ordering.Long.compare(x.timestamp, y.timestamp)
      } else Ordering.Boolean.compare(x.uiWaiting, y.uiWaiting)
    }
  }
}
