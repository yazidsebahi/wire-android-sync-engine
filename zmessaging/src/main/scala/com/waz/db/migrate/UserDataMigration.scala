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
package com.waz.db.migrate

import android.database.sqlite.SQLiteDatabase

object UserDataMigration {
  lazy val v61 = (_: SQLiteDatabase).execSQL("ALTER TABLE Users ADD COLUMN deleted INTEGER DEFAULT 0")
  lazy val v78 = (_: SQLiteDatabase).execSQL("ALTER TABLE Users ADD COLUMN handle TEXT DEFAULT ''")
  lazy val v79 = { implicit db: SQLiteDatabase =>
    db.execSQL(s"CREATE INDEX IF NOT EXISTS Conversation_id on Users (_id)")
    db.execSQL(s"CREATE INDEX IF NOT EXISTS UserData_search_key on Users (skey)")
  }
}
