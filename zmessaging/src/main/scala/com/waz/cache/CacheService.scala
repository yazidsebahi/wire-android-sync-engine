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
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{BadPaddingException, Cipher, CipherInputStream, CipherOutputStream}

import android.content.Context
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.Database
import com.waz.model.Uid
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.{IoUtils, LoggedTry, returning}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class CacheEntry(val data: CacheEntryData, service: CacheService) extends LocalData {
  private implicit val logTag: LogTag = logTagFor[CacheEntry]

  override def inputStream: InputStream =
    content.fold[InputStream](CacheService.inputStream(data.encKey, new FileInputStream(cacheFile)))(new ByteArrayInputStream(_))

  override def length: Int = content.fold(cacheFile.length().toInt)(_.length)

  override def file = content.fold(Option(cacheFile))(_ => None)

  override def byteArray = content

  def content = data.data

  // direct access to this file is not advised, it's content will be encrypted when on external storage, it's better to use stream api
  private[waz] def cacheFile = service.entryFile(data.path.getOrElse(service.cacheDir), data.fileId)

  def outputStream = {
    cacheFile.getParentFile.mkdirs()
    CacheService.outputStream(data.encKey, new FileOutputStream(cacheFile))
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
  import CacheService._
  import Threading.Implicits.Background

  lazy val cacheStorage = new CacheStorage(storage, context)

  def createForFile(key: String = Uid().str, cacheLocation: Option[File] = None)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry =
    add(CacheEntryData(key, None, timeout = timeout.timeout, path = cacheLocation.orElse(Some(intCacheDir)))) // use internal storage for this files as those won't be encrypted

  def addData(key: String, data: Array[Byte])(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CacheEntry =
    add(CacheEntryData(key, Some(data), timeout = timeout.timeout))

  def addStream(key: String, in: => InputStream, cacheLocation: Option[File] = None, length: Int = -1)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CancellableFuture[CacheEntry] = CancellableFuture {
    if (length > 0 && length <= CacheService.DataThreshold) {
      addData(key, IoUtils.toByteArray(in))
    } else {
      addFileToStorage(IoUtils.copy(in, _), cacheLocation) match {
        case Success((fileId, path, encKey)) =>
          add(CacheEntryData(key, timeout = timeout.timeout, path = Some(path), fileId = fileId, encKey = encKey))
        case Failure(e) =>
          HockeyApp.saveException(e, s"addStream($key) failed")
          throw new Exception(s"addStream($key) failed", e)
      }
    }
  }

  def addFile(key: String, src: File, moveFile: Boolean = false, cacheLocation: Option[File] = None)(implicit timeout: Expiration = CacheService.DefaultExpiryTime): CancellableFuture[CacheEntry] = CancellableFuture {
    addFileToStorage(IoUtils.copy(new FileInputStream(src), _), cacheLocation) match {
      case Success((fileId, path, encKey)) =>
        if (moveFile) src.delete()
        add(CacheEntryData(key, timeout = timeout.timeout, path = Some(path), fileId = fileId, encKey = encKey))
      case Failure(e) =>
        HockeyApp.saveException(e, s"addFile($key) failed")
        throw new Exception(s"addFile($key) failed", e)
    }
  }

  private def addFileToStorage(writer: OutputStream => Unit, location: Option[File]): Try[(Uid, File, Option[AES128Key])] = {

    def write(dir: File, enc: Option[AES128Key]) = {
      val id = Uid()
      val file = entryFile(dir, id)
      file.getParentFile.mkdirs()
      LoggedTry.local {
        writer(outputStream(enc, new FileOutputStream(file)))
        (id, dir, enc)
      }
    }

    location match {
      case Some(dir) => write(dir, None)
      case None =>
        extCacheDir match {
          case Some(dir) => write(dir, Some(AES128Key())).orElse(write(intCacheDir, None))
          case None => write(intCacheDir, None)
        }
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

  private def add(entry: CacheEntryData) = {
    cacheStorage.add(entry)
    entry.path foreach { entryFile(_, entry.fileId).getParentFile.mkdirs() }
    new CacheEntry(entry, this)
  }

  def extCacheDir = Option(context.getExternalCacheDir).filter(_.isDirectory)
  def intCacheDir: File = context.getCacheDir
  def cacheDir: File = extCacheDir.getOrElse(intCacheDir)

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
  private implicit val logTag: LogTag = logTagFor[CacheService]

  val DataThreshold = 4 * 1024 // amount of data stored in db instead of a file
  val DefaultExpiryTime = 7.days

  val iv = Array.fill[Byte](16)(0) // we are using random key every time, IV can be constant

  def symmetricCipher(key: Array[Byte], mode: Int) =
    returning(Cipher.getInstance("AES/CBC/PKCS5Padding")) { _.init(mode, new SecretKeySpec(key, "AES"), new IvParameterSpec(iv)) }

  def outputStream(key: Option[AES128Key], os: OutputStream) =
    key.fold(os) { k =>
      new CipherOutputStream(os, symmetricCipher(k.bytes, Cipher.ENCRYPT_MODE))
    }

  def inputStream(key: Option[AES128Key], is: InputStream) =
    key.fold(is) { k =>
      new CipherInputStream(is, symmetricCipher(k.bytes, Cipher.DECRYPT_MODE)) {
        override def close(): Unit = try {
          super.close()
        } catch {
          case io: IOException =>
            io.getCause match {
              case _: BadPaddingException => //ignore
              case e => throw e
            }
        }
      }
    }
}
