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
package com.waz.utils.wrappers

import android.content.ContentValues

import scala.collection.mutable
import scala.language.implicitConversions

abstract class DBContentValues {
  import DBContentValues.SupportedType._

  private val typeMap = mutable.HashMap[String, SupportedType]()

  def containsKey(key: String) = typeMap.contains(key)
  def size = typeMap.size
  def keySet = typeMap.toMap.keySet
  def remove(key: String) = { typeMap.remove(key); _remove(key) }
  def clear() = { typeMap.clear(); _clear() }
  def put(key: String, value: Int) = { typeMap.put(key, INT); _put(key, value) }
  def put(key: String, value: Long) = { typeMap.put(key, LONG); _put(key, value) }
  def put(key: String, value: String) = { typeMap.put(key, STRING); _put(key, value) }
  def put(key: String, value: Boolean) = { typeMap.put(key, BOOLEAN); _put(key, value) }
  def put(key: String, value: Double) = { typeMap.put(key, DOUBLE); _put(key, value) }
  def put(key: String, value: Float) = { typeMap.put(key, FLOAT); _put(key, value) }

  protected def _put(key: String, value: Int): Unit
  protected def _put(key: String, value: Long): Unit
  protected def _put(key: String, value: String): Unit
  protected def _put(key: String, value: Boolean): Unit
  protected def _put(key: String, value: Double): Unit
  protected def _put(key: String, value: Float): Unit

  protected def _remove(key: String): Unit
  protected def _clear(): Unit

  def getAsInt(key: String): Int
  def getAsLong(key: String): Long
  def getAsString(key: String): String
  def getAsBoolean(key: String): Boolean
  def getAsDouble(key: String): Double
  def getAsFloat(key: String): Float
}

class DBContentValuesWrapper(val values: ContentValues) extends DBContentValues {
  override protected def _put(key: String, value: Int) = values.put(key, java.lang.Integer.valueOf(value))
  override protected def _put(key: String, value: Long) = values.put(key, java.lang.Long.valueOf(value))
  override protected def _put(key: String, value: String) = values.put(key, java.lang.String.valueOf(value))
  override protected def _put(key: String, value: Boolean) = values.put(key, java.lang.Boolean.valueOf(value))
  override protected def _put(key: String, value: Double) = values.put(key, java.lang.Double.valueOf(value))
  override protected def _put(key: String, value: Float) = values.put(key, java.lang.Float.valueOf(value))

  override def getAsInt(key: String) = values.getAsInteger(key)
  override def getAsLong(key: String) = values.getAsLong(key)
  override def getAsString(key: String) = values.getAsString(key)
  override def getAsBoolean(key: String) = values.getAsBoolean(key)
  override def getAsDouble(key: String) = values.getAsDouble(key)
  override def getAsFloat(key: String) = values.getAsFloat(key)

  override def _remove(key: String) = values.remove(key)
  override def _clear() = values.clear()
}

class DBContentValuesMap(private val map: mutable.HashMap[String, Any]) extends DBContentValues {
  override protected def _put(key: String, value: Int) = map.put(key, value)
  override protected def _put(key: String, value: Long) = map.put(key, value)
  override protected def _put(key: String, value: String) = map.put(key, value)
  override protected def _put(key: String, value: Boolean) = map.put(key, value)
  override protected def _put(key: String, value: Double) = map.put(key, value)
  override protected def _put(key: String, value: Float) = map.put(key, value)

  override def getAsInt(key: String) = map.get(key).asInstanceOf[Int]
  override def getAsLong(key: String) = map.get(key).asInstanceOf[Long]
  override def getAsString(key: String) = map.get(key).asInstanceOf[String]
  override def getAsBoolean(key: String) = map.get(key).asInstanceOf[Boolean]
  override def getAsDouble(key: String) = map.get(key).asInstanceOf[Double]
  override def getAsFloat(key: String) = map.get(key).asInstanceOf[Float]

  override protected def _remove(key: String) = map.remove(key)
  override protected def _clear() = map.clear()
}

object DBContentValues {
  object SupportedType extends Enumeration {
    type SupportedType = Value
    val INT, LONG, STRING, BOOLEAN, DOUBLE, FLOAT = Value
  }
  import SupportedType._

  def apply(values: ContentValues) = new DBContentValuesWrapper(values)
  def apply() = new DBContentValuesMap(mutable.HashMap())

  implicit def fromAndroid(values: ContentValues): DBContentValues = apply(values)
  implicit def toAndroid(values: DBContentValues): ContentValues = {
    val androidValues = new ContentValues()
    values.typeMap.foreach {
      case (key, INT) => androidValues.put(key, java.lang.Integer.valueOf(values.getAsInt(key)))
      case (key, LONG) => androidValues.put(key, java.lang.Long.valueOf(values.getAsLong(key)))
      case (key, STRING) => androidValues.put(key, java.lang.String.valueOf(values.getAsString(key)))
      case (key, BOOLEAN) => androidValues.put(key, java.lang.Boolean.valueOf(values.getAsBoolean(key)))
      case (key, DOUBLE) => androidValues.put(key, java.lang.Double.valueOf(values.getAsDouble(key)))
      case (key, FLOAT) => androidValues.put(key, java.lang.Float.valueOf(values.getAsFloat(key)))
    }
    androidValues
  }
}
