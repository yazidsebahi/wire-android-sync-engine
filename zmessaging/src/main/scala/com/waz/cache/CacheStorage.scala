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
package com.waz.cache

import java.io.File

import android.content.Context
import com.waz.ZLog._
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.Database
import com.waz.model.Uid
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.TrimmingLruCache.{Fixed, Relative}
import com.waz.utils.{SerialProcessingQueue, ThrottledProcessingQueue, TrimmingLruCache}

import scala.collection.mutable
import scala.concurrent.duration._

class CacheStorage(storage: Database, context: Context) {
  import com.waz.cache.CacheStorage._
  private implicit val logTag: LogTag = logTagFor[CacheStorage]
  private implicit val dispatcher = new SerialDispatchQueue(name = "CacheStorage")

  private val entries = new TrimmingLruCache[String, CacheEntryData](context, Fixed(1024 * 1024) min Relative(.05f)) {
    override def sizeOf(key: String, value: CacheEntryData): Int = value.data.fold(0)(_.length) + key.length + value.path.fold(0)(_.getPath.length) + 56 // data plus some object overhead
  }

  val saveQueue = new ThrottledProcessingQueue[DbCmd](500.millis, { cmds =>
    storage.withTransaction { implicit db =>
      val items = new mutable.HashMap[String, CacheEntryData]
      val removed = new mutable.HashSet[String]

      cmds.foreach {
        case Insert(entry) => items(entry.key) = entry
        case Delete(id) =>
          items -= id
          removed += id
      }

      removed foreach { CacheEntryDao.deleteByKey }
      CacheEntryDao.insertOrReplace(items.values)
    }
  }, "CacheSaveQueue")

  val fileCleanupQueue = new SerialProcessingQueue[(File, Uid)]({ entries =>
    Threading.IO {
      verbose(s"deleting cache files: $entries")
      entries foreach { case (path, uid) => entryFile(path, uid).delete() }
    }
  }, "CacheFileCleanupQueue")


  private def getLocal(key: String): Option[CacheEntryData] = entries.get(key) match {
    case null => None
    case entry if entry eq EmptyEntry => Some(EmptyEntry)
    case entry if expired(entry) || dataMissing(entry) =>
      debug(s"cache entry expired or data missing: $entry, deleting...")
      entries.put(key, EmptyEntry)
      cleanup(entry)
      Some(EmptyEntry)
    case entry => Some(entry)
  }

  def get(key: String): CancellableFuture[Option[CacheEntryData]] = {
    getLocal(key).fold {
      storage { CacheEntryDao.getByKey(key)(_) } .map { fromDb =>
        getLocal(key).filter(_ ne EmptyEntry).orElse {
          fromDb.flatMap { entry =>
            if (expired(entry) || dataMissing(entry)) {
              saveQueue ! Delete(key)
              cleanup(entry)
              None
            } else {
              val updated = updateExpires(entry)
              saveQueue ! Insert(updated)
              Some(updated)
            }
          }
        }
      }
    } { entry =>
      CancellableFuture.successful(if (entry eq EmptyEntry) None else Some(updateExpires(entry)))
    }
  }

  def updateExpires(entry: CacheEntryData): CacheEntryData = {
    val updated = entry.copy(lastUsed = System.currentTimeMillis)
    entries.put(entry.key, updated)
    updated
  }

  def add(entry: CacheEntryData): CacheEntryData = {
    val previous = Option(entries.get(entry.key))
    dispatcher {
      previous foreach { prev =>
        if ((prev ne EmptyEntry) && prev.fileId != entry.fileId) cleanup(prev)
      }
      saveQueue ! Insert(entry)
    }
    entries.put(entry.key, entry)
    entry
  }

  def remove(key: String): CancellableFuture[Unit] = dispatcher {
    Option(entries.get(key)) foreach { prev =>
      if (prev ne EmptyEntry) cleanup(prev)
    }
    entries.put(key, EmptyEntry)
    saveQueue ! Delete(key)
  }

  def remove(entry: CacheEntryData): CancellableFuture[Unit] = dispatcher {
    cleanup(entry)
    entries.put(entry.key, EmptyEntry)
    saveQueue ! Delete(entry.key)
  }

  def cleanup(entry: CacheEntryData): Unit = entry.path foreach { path => fileCleanupQueue ! (path, entry.fileId) }

  def expired(entry: CacheEntryData) = entry.lastUsed + entry.timeout <= System.currentTimeMillis()

  def dataMissing(entry: CacheEntryData) = entry.data.isEmpty && !entry.path.exists(path => entryFile(path, entry.fileId).exists())
}

object CacheStorage {
  val EmptyEntry = CacheEntryData("EMPTY", path = None) // Empty entry used in local storage to mark removed item (so that we don't try to load it from db later - this could cause race conditions)

  sealed trait DbCmd
  case class Insert(entry: CacheEntryData) extends DbCmd
  case class Delete(key: String) extends DbCmd

  def entryFile(cacheDir: File, uid: Uid) = new File(cacheDir, uid.str.take(2) + File.separator + uid.str)
}
