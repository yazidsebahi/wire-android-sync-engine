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
import java.security.SecureRandom

import android.content.ContentValues
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import android.util.Base64
import com.waz.ZLog
import com.waz.ZLog._
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.db.DbTranslator.FileTranslator
import com.waz.model.Uid
import com.waz.utils.returning

case class AES128Key(str: String) {
  def bytes = Base64.decode(str, Base64.NO_WRAP | Base64.NO_CLOSE)
}

object AES128Key {
  lazy val random = new SecureRandom()

  def apply(): AES128Key = AES128Key(Base64.encodeToString(returning(Array.ofDim[Byte](16))(random.nextBytes), Base64.NO_WRAP | Base64.NO_CLOSE))
}

case class CacheEntryData(
                           key: String,
                           data: Option[Array[Byte]] = None,
                           lastUsed: Long = currentTimeMillis(),
                           timeout: Long = CacheService.DefaultExpiryTime.toMillis,
                           path: Option[File] = None,
                           encKey: Option[AES128Key] = None,
                           fileId: Uid = Uid()) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case CacheEntryData(k, d, lu, t, p, e, i) =>
      ZLog.warn("Comparing CacheEntryData with equals - can be slow", new IllegalStateException(""))(logTagFor[CacheEntryData])
      k == key && lu == lastUsed && t == timeout && p == path && i == fileId && d.map(_.toSeq) == data.map(_.toSeq)
    case _ => false
  }
}

object CacheEntryData {

  implicit object CacheEntryDao extends Dao[CacheEntryData, String] with CacheEntryDataUpgrades {
    val Key = text('key, "PRIMARY KEY")(_.key)
    val Uid = uid('file)(_.fileId)
    val Data = opt(blob('data))(_.data)
    val LastUsed = long('lastUsed)(_.lastUsed)
    val Timeout = long('timeout)(_.timeout)
    val Path = opt(file('path))(_.path)
    val EncKey = opt(text[AES128Key]('enc_key, _.str, AES128Key(_)))(_.encKey)

    override val idCol = Key
    override val table = Table("CacheEntry", Key, Uid, Data, LastUsed, Timeout, EncKey, Path)

    override def apply(implicit cursor: Cursor): CacheEntryData =
      new CacheEntryData(Key, Data, LastUsed, Timeout, Path, EncKey, Uid)

    def getByKey(key: String)(implicit db: SQLiteDatabase): Option[CacheEntryData] = getById(key)

    def deleteByKey(key: String)(implicit db: SQLiteDatabase): Int = delete(key)

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
