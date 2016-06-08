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
package com.waz.db

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog._
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.model.ZUser.ZUserDao

class ZGlobalDB(context: Context, dbNameSuffix: String = "") extends DaoDB(context.getApplicationContext, ZGlobalDB.DbName + dbNameSuffix, null, ZGlobalDB.DbVersion) {
  private implicit val logTag: LogTag = logTagFor[ZGlobalDB]

  override val daos = Seq(ZUserDao, CacheEntryDao)

  override val migrations = Seq(
    Migration(5, 6)(addCacheEntry_Path_LastUsed_Timeout),
    Migration(6, 7)(addPhoneNumber),
    Migration(7, 8)(addPhoneNumberVerified),
    Migration(8, 9){ implicit db => db.execSQL("ALTER TABLE CacheEntry ADD COLUMN enc_key TEXT") },
    Migration(9, 10){ implicit db =>
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN mime TEXT default ''")
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN file_name TEXT")
    },
    Migration(10, 11) { implicit db =>
      db.execSQL("DELETE FROM CacheEntry WHERE enc_key IS NOT NULL") // encryption handling was changed, so it's easiest to just drop old cache entries
    },
    Migration(11, 12){ implicit db =>
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN length INTEGER")
    }
  )

  override def onUpgrade(db: SQLiteDatabase, from: Int, to: Int): Unit = {
    if (from < 5) clearAllData(db)
    else super.onUpgrade(db, from, to)
  }

  def clearAllData(db: SQLiteDatabase) = {
    debug("wiping global db...")
    dropAllTables(db)
    onCreate(db)
  }

  lazy val addCacheEntry_Path_LastUsed_Timeout: SQLiteDatabase => Unit = { implicit db =>
    import CacheEntryDao._

    debug("Upgrading global db to accommodate for upgraded image caching...")
    Seq("path", "lastUsed", "timeout") foreach { col => db.execSQL(s"ALTER TABLE CacheEntry ADD COLUMN $col TEXT") }
    setPathForFileEntries(context.getCacheDir)
    moveToTimeouts()
  }

  lazy val addPhoneNumber: SQLiteDatabase => Unit = { implicit db =>
    debug("Upgrading global db to prepare for phone number verification...")
    db.execSQL(s"ALTER TABLE ZUsers ADD COLUMN phone TEXT")
  }

  lazy val addPhoneNumberVerified: SQLiteDatabase => Unit = { implicit db =>
    debug("Upgrading global DB to remember if phone number was verified…")
    db.execSQL(s"ALTER TABLE ZUsers ADD COLUMN phone_verified INTEGER")
    db.execSQL(s"UPDATE ZUsers SET phone_verified = phone is not null")
  }
}

object ZGlobalDB {
  val DbName = "ZGlobal.db"
  val DbVersion = 12
}
