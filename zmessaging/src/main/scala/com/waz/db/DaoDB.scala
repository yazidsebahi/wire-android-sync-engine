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
import android.database.sqlite.SQLiteDatabase.CursorFactory
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.waz.utils.returning

abstract class DaoDB(context: Context, name: String, factory: CursorFactory, version: Int) extends SQLiteOpenHelper(context, name, factory, version) {

  val daos: Seq[BaseDao[_]]
  val migrations: Seq[Migration]

  setWriteAheadLoggingEnabled(true)

  override def getWritableDatabase: SQLiteDatabase = synchronized(returning(super.getWritableDatabase)(db => if (! db.isWriteAheadLoggingEnabled) db.enableWriteAheadLogging()))

  override def onCreate(db: SQLiteDatabase): Unit =
    daos.foreach { dao =>
      dao.onCreate(db)
    }

  override def onUpgrade(db: SQLiteDatabase, from: Int, to: Int): Unit =
    new Migrations(migrations: _*).migrate(this, from, to)(db)

  def dropAllTables(db: SQLiteDatabase): Unit =
    daos.foreach { dao =>
      db.execSQL(s"DROP TABLE IF EXISTS ${dao.table.name};")
    }
}
