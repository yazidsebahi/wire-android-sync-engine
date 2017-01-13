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
package com.waz.utils

import android.support.v4.util.LruCache

import scala.collection.mutable

class MultiKeyLruCache[K1, K2, V](maxSize: Int) {

  private val cache = new LruCache[(K1, K2), V](maxSize)
  private val map = new mutable.HashMap[K1, mutable.HashMap[K2, V]]()

  def get(k1: K1, k2: K2): Option[V] ={
    Option(cache.get((k1,k2)))
  }

  def get(k1: K1): Map[K2, V] = {
    val subMap = map.getOrElse(k1, mutable.HashMap[K2, V]())
    subMap.foreach(item => cache.get((k1, item._1)))
    subMap.toMap
  }

  def put(k1: K1, k2: K2, v: V): Unit ={
    map.getOrElseUpdate(k1, new mutable.HashMap[K2, V]()).put(k2, v)
    cache.put((k1,k2), v)
  }

  def remove(k1: K1, k2: K2): Unit ={
    map.get(k1).foreach(_.remove(k2))
    if (map.get(k1).exists(_.isEmpty)) {
      map.remove(k1)
    }
    cache.remove((k1,k2))
  }
}
