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
package com.waz.testutils

import com.waz.content.Preferences.Preference.PrefCodec
import com.waz.content.Preferences.{PrefKey, Preference}
import com.waz.content.{GlobalPreferences, UserPreferences}
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.returning

class TestGlobalPreferences extends GlobalPreferences(null, null) {
  override implicit val dispatcher = new SerialDispatchQueue(name = "TestGlobalPreferenceQueue")

  private var values = Map.empty[String, String]
  private var prefs = Map.empty[String, Preference[_]]

  override def preference[A: PrefCodec](key: PrefKey[A]) = returning(new Preference[A](this, key))(p => prefs += (key.str -> p))

  override protected def getValue[A: PrefCodec](key: PrefKey[A]) = dispatcher {
    val codec = implicitly[PrefCodec[A]]
    values.get(key.str).map(codec.decode).getOrElse(key.default)
  }

  override protected def setValue[A: PrefCodec](key: PrefKey[A], value: A) =
    dispatcher {
      values += (key.str -> implicitly[PrefCodec[A]].encode(value))
      //need to update any other preferences of the change
      prefs.collect { case (k, p) if k == key.str => p.asInstanceOf[Preference[A]] }.foreach(_.signal ! value)
    }

  def reset() = this.values = Map.empty

  def print() = dispatcher(println(values))
}

class TestUserPreferences extends UserPreferences(null, null) {

  override implicit val dispatcher = new SerialDispatchQueue(name = "TestGlobalPreferenceQueue")

  private var values = Map.empty[String, String]
  private var prefs = Map.empty[String, Preference[_]]

  override def preference[A: PrefCodec](key: PrefKey[A]) = returning(new Preference[A](this, key))(p => prefs += (key.str -> p))

  override protected def getValue[A: PrefCodec](key: PrefKey[A]) = dispatcher {
    val codec = implicitly[PrefCodec[A]]
    values.get(key.str).map(codec.decode).getOrElse(key.default)
  }

  override protected def setValue[A: PrefCodec](key: PrefKey[A], value: A) =
    dispatcher {
      values += (key.str -> implicitly[PrefCodec[A]].encode(value))
      //need to update any other preferences of the change
      prefs.collect { case (k, p) if k == key.str => p.asInstanceOf[Preference[A]] }.foreach(_.signal ! value)
    }

  def reset() = this.values = Map.empty

  def print() = dispatcher(println(values))
}