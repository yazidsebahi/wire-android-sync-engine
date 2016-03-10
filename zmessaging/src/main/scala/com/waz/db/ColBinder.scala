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

import android.content.ContentValues
import android.database.Cursor
import android.database.sqlite.SQLiteProgram

case class ColBinder[A, B](col: Col[A], extractor: B => A, var index: Int = 0) {
  def apply(value: A): String = col.sqlLiteral(value)
  def load(cursor: Cursor, index: Int): A = col.load(cursor, index)
  def save(value: B, values: ContentValues): Unit = col.save(extractor(value), values)
  def bind(value: B, stmt: SQLiteProgram): Unit = col.bind(extractor(value), index + 1, stmt)
  def bindCol(value: A, stmt: SQLiteProgram): Unit = col.bind(value, index + 1, stmt)
  def name: String = col.name
}
