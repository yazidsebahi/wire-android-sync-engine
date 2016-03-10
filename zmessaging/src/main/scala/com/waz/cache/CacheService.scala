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

import java.io._
import java.lang.System._

import android.content.Context
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.Database
import com.waz.model.Uid
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.IoUtils

import scala.concurrent.Future
import scala.concurrent.duration._

class CacheEntry(val data: CacheEntryData, service: CacheService) extends LocalData {
  private implicit val logTag: LogTag = logTagFor[CacheEntry]

  override def inputStream: InputStream = content.fold[InputStream](new FileInputStream(cacheFile))(new ByteArrayInputStream(_))

  override def length: Int = content.fold(cacheFile.length().toInt)(_.length)

  override def file = content.fold(Option(cacheFile))(_ => None)

  override def byteArray = content

  def content = data.data

  def cacheFile = service.entryFile(data.path.getOrElse(service.cacheDir), data.fileId)

  def outputStream = {
    cacheFile.getParentFile.mkdirs()
    new FileOutputStream(cacheFile)
  }

  def copyDataToFile() = {
    content foreach { data =>
      IoUtils.copy(new ByteArrayInputStream(data), outputStream)
    }
    cacheFile
  }

  override def delete(): Unit = service.remove(this)

  override def toString: LogTag = s"CacheEntry($data)"
}

object CacheEntry {
  def unapply(entry: CacheEntry): Option[(String, Option[Array[Byte]], File)] = Some((entry.data.key, entry.content, entry.cacheFile))
}

case class Expiration(timeout: Long)

object Expiration {
  import scala.language.implicitConversions

  implicit def in(d: Duration) : Expiration = if (d.isFinite()) Expiration(d.toMillis) else Expiration(1000L * 3600L * 24L * 365L * 1000L) // 1000 years (don't use Long.MaxValue due to overflow dangers)
}

class CacheService(context: Context, storage: Database) {
  private implicit val logTag: LogTag = logTagFor[CacheService]
  import Threading.Implicits.Background

  lazy val cacheStorage = new CacheStorage(storage, context)

  def createForFile(key: String = Uid().str, cacheLocation: Option[File] = None)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = add(key, Right(cacheLocation))

  def addData(key: String, data: Array[Byte])(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = {
    add(key, Left(data))(timeout)
  }

  def addStream(key: String, in: => InputStream, cacheLocation: Option[File] = None, length: Int = -1)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CancellableFuture[CacheEntry] = CancellableFuture {
    val path = cacheLocation.getOrElse(cacheDir)
    try {
      if (length > 0 && length <= CacheService.DataThreshold) {
        addData(key, IoUtils.toByteArray(in))
      } else {
        val data = CacheEntryData(key, timeout = timeout.timeout, path = Some(path))
        val file = entryFile(path, data.fileId)
        file.getParentFile.mkdirs()
        IoUtils.copy(in, new FileOutputStream(file))
        add(data)
      }
    } catch {
      case e: IOException =>
        error(s"addStream($key) failed, will return expired cache entry", e)
        HockeyApp.saveException(e, s"addStream($key) failed, returning expired cache entry")
        add(CacheEntryData(key, path = Some(path), lastUsed = 0L)) // already expired
    }
  }

  def addFile(key: String, src: File, moveFile: Boolean = false, cacheLocation: Option[File] = None)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CancellableFuture[CacheEntry] = CancellableFuture {
    val path = cacheLocation.getOrElse(cacheDir)
    val data = CacheEntryData(key, timeout = timeout.timeout, path = Some(path))
    val file = entryFile(path, data.fileId)
    try {
      file.getParentFile.mkdirs()
      IoUtils.copy(new FileInputStream(src), new FileOutputStream(file))
      if (moveFile) src.delete()
      add(data)
    } catch {
      case e: IOException =>
        error(s"addFile($key) failed, will return expired cache entry", e)
        HockeyApp.saveException( e, s"addFile($key, moveFile = $moveFile) failed, returning expired cache entry")
        add(data.copy(lastUsed = 0L)) // already expired
    }
  }

  def move(key: String, entry: LocalData, cacheLocation: Option[File] = None)(implicit timeout: Expiration = CacheService.DefaultExpiryTime) = {
    verbose(s"move($key, $entry)")
    addStream(key, entry.inputStream, cacheLocation, entry.length) map { current =>
      verbose(s"moved $current, file exists: ${current.cacheFile.exists()}, deleting entry: $entry")
      entry.delete()
      current
    }
  }

  // You can either add byte data directly (for previews or otherwise very small entries), or you can add files.
  // When adding files, you can optionally specify a parent location under which to put them.
  private def add(key: String, data: Either[Array[Byte], Option[File]])(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry = {
    val path = data.fold[Option[File]](_ => None, _ orElse Some(cacheDir))
    add(CacheEntryData(key, data.left.toOption, timeout = timeout.timeout, path = path))
  }

  private def add(entry: CacheEntryData) = {
    cacheStorage.add(entry)
    entry.path foreach { entryFile(_, entry.fileId).getParentFile.mkdirs() }
    new CacheEntry(entry, this)
  }

  def cacheDir: File = Option(context.getExternalCacheDir).filter(_.isDirectory).getOrElse(context.getCacheDir)

  def getEntry(key: String): CancellableFuture[Option[CacheEntry]] = cacheStorage.get(key) map {
    case Some(e) => Some(new CacheEntry(e, this))
    case None => None
  }

  def getOrElse(key: String, default: => CancellableFuture[CacheEntry]) = getEntry(key) flatMap {
    case Some(entry) => CancellableFuture.successful(entry)
    case _ => default
  }

  def remove(key: String): CancellableFuture[Unit] = cacheStorage.remove(key)

  def remove(entry: CacheEntry): CancellableFuture[Unit] = {
    verbose(s"remove($entry)")
    cacheStorage.remove(entry.data)
  }

  def deleteExpired(): CancellableFuture[Unit] = {
    val currentTime = currentTimeMillis()
    storage { implicit db =>
      val entries = CacheEntryDao.findAllExpired(currentTime)
      CacheEntryDao.deleteExpired(currentTime)
      entries
    }.map { entries =>
      entries foreach cacheStorage.remove
      entries
    }.map { _ foreach (entry => entry.path foreach { path => entryFile(path, entry.fileId).delete() }) }
  }

  private[cache] def entryFile(path: File, fileId: Uid) = CacheStorage.entryFile(path, fileId)

  // util method to perform local data processing with caching
  def processWithCaching(cacheKey: String, process: (InputStream, OutputStream) => Unit, data: LocalData): Future[LocalData] = {
    def processedByteData = CancellableFuture {
      val bos = new ByteArrayOutputStream()
      process(data.inputStream, bos)
      bos.toByteArray
    }

    def processedFile = CancellableFuture {
      val file = File.createTempFile("temp", ".enc", context.getCacheDir)
      process(data.inputStream, new BufferedOutputStream(new FileOutputStream(file)))
      file
    }

    def saveToCache =
      if (data.byteArray.isDefined) processedByteData.map { addData(cacheKey, _) }
      else processedFile.flatMap { addFile(cacheKey, _, moveFile = true) }

    getOrElse(cacheKey, saveToCache).future
  }
}

object CacheService {
  val DataThreshold = 4 * 1024 // amount of data stored in db instead of a file
  val DefaultExpiryTime = 7.days
}
