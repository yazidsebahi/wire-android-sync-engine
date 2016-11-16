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
import java.lang.System.currentTimeMillis

import android.content.ContentValues
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.db.DbTranslator.FileTranslator
import com.waz.model._
import com.waz.utils.returning

case class CacheEntryData(key: CacheKey,
                          data: Option[Array[Byte]] = None,
                          lastUsed: Long = currentTimeMillis(),
                          timeout: Long = CacheService.DefaultExpiryTime.toMillis,
                          path: Option[File] = None,
                          encKey: Option[AESKey] = None,
                          fileName: Option[String] = None,
                          mimeType: Mime = Mime.Unknown,
                          fileId: Uid = Uid(),
                          length: Option[Long] = None)

object CacheEntryData {

  implicit object CacheEntryDao extends Dao[CacheEntryData, CacheKey] with CacheEntryDataUpgrades {
    val Key = id[CacheKey]('key, "PRIMARY KEY").apply(_.key)
    val Uid = uid('file)(_.fileId)
    val Data = opt(blob('data))(_.data)
    val LastUsed = long('lastUsed)(_.lastUsed)
    val Timeout = long('timeout)(_.timeout)
    val Path = opt(file('path))(_.path)
    val Name = opt(text('file_name))(_.fileName)
    val MimeType = text[Mime]('mime, _.str, Mime(_)).apply(_.mimeType)
    val EncKey = opt(text[AESKey]('enc_key, _.str, AESKey(_)))(_.encKey)
    val Length = opt(long('length))(_.length)

    override val idCol = Key
    override val table = Table("CacheEntry", Key, Uid, Data, LastUsed, Timeout, EncKey, Path, MimeType, Name, Length)

    override def apply(implicit cursor: Cursor): CacheEntryData =
      new CacheEntryData(Key, Data, LastUsed, Timeout, Path, EncKey, Name, MimeType, Uid, Length)

    def getByKey(key: CacheKey)(implicit db: SQLiteDatabase): Option[CacheEntryData] = getById(key)

    def deleteByKey(key: CacheKey)(implicit db: SQLiteDatabase): Int = delete(key)

    def findAll(implicit db: SQLiteDatabase): Seq[CacheEntryData] = list

    def findAllWithData(implicit db: SQLiteDatabase): Seq[CacheEntryData] = {
      list(db.query(table.name, null, s"${Data.name} IS NOT NULL", Array.empty, null, null, null))
    }

    def findAllExpired(currentTime: Long)(implicit db: SQLiteDatabase): Seq[CacheEntryData] = {
      list(db.query(table.name, null, s"${LastUsed.name} + ${Timeout.name} < $currentTime", null, null, null, null))
    }

    def deleteExpired(currentTime: Long)(implicit db: SQLiteDatabase): Unit = {
      db.delete(table.name, s"${LastUsed.name} + ${Timeout.name} < $currentTime", null)
    }
  }

  trait CacheEntryDataUpgrades {
    def setPathForFileEntries(path: File)(implicit db: SQLiteDatabase): Unit = {
      val values = returning(new ContentValues) { _.put("path", FileTranslator.literal(path)) }
      db.update("CacheEntry", values, s"data IS NULL AND path IS NULL", null)
    }

    def moveToTimeouts()(implicit db: SQLiteDatabase): Unit = {
      db.execSQL(s"UPDATE CacheEntry SET lastUsed = created, timeout = 604800000") // default to a timeout of one week after the upgrade
    }
  }
}
