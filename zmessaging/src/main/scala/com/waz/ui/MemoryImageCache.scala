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
package com.waz.ui

import android.content.Context
import android.graphics.Bitmap
import com.waz.ZLog._
import com.waz.bitmap
import com.waz.model.AssetId
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache.{BitmapEntry, EmptyEntry, Entry, Key}
import com.waz.utils.TrimmingLruCache
import com.waz.utils.TrimmingLruCache.CacheSize

class MemoryImageCache(val context: Context) {
  private implicit val logTag: LogTag = logTagFor[MemoryImageCache]

  /**
   * In memory image cache.
   */
  private val lru = new TrimmingLruCache[Key, Entry](context, CacheSize(total => math.max(5 * 1024 * 1024, (total - 30 * 1024 * 1024) / 2))) {
    override def sizeOf(id: Key, value: Entry): Int = value.size
  }

  def get(id: AssetId, tag: String): Option[Bitmap] =
    Option(lru.get(Key(id, tag))) flatMap {
      case BitmapEntry(bitmap) => Some(bitmap)
      case _ => None
    }

  def add(id: AssetId, tag: String, bitmap: Bitmap): Unit = if (bitmap != null && bitmap != Images.EmptyBitmap) {
    lru.put(Key(id, tag), BitmapEntry(bitmap))
  }

  def remove(id: AssetId, tag: String): Unit = lru.remove(Key(id, tag))

  def reserve(id: AssetId,  tag: String, width: Int, height: Int): Unit = reserve(id, tag, width * height * 4 + 256)

  def reserve(id: AssetId, tag: String, size: Int): Unit = lru.synchronized {
    val key = Key(id, tag)
    Option(lru.get(key)) getOrElse lru.put(key, EmptyEntry(size))
  }

  def apply(id: AssetId, tag: String, load: => CancellableFuture[Bitmap]): CancellableFuture[Bitmap] =
    get(id, tag) match {
      case Some(bitmap) => CancellableFuture.successful(bitmap)
      case None =>
        val future = load
        future.onSuccess {
          case bitmap.EmptyBitmap => // ignore
          case img => add(id, tag, img)
        }(Threading.Ui)
        future
    }
}

object MemoryImageCache {

  case class Key(id: AssetId, tag: String)

  sealed trait Entry {
    def size: Int
  }

  case class BitmapEntry(bitmap: Bitmap) extends Entry {
    override def size = bitmap.getByteCount
  }

  // used to reserve space
  case class EmptyEntry(size: Int) extends Entry {
    require(size > 0)
  }
}
