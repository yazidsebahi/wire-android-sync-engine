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

import com.waz.content.Preferences.{PrefKey, Preference}
import com.waz.content.Preferences.Preference.PrefCodec
import com.waz.content.{GlobalPreferences, UserPreferences}
import com.waz.threading.SerialDispatchQueue

class TestGlobalPreferences extends GlobalPreferences(null) {
  override implicit val dispatcher = new SerialDispatchQueue(name = "TestGlobalPreferenceQueue")

  private var values = Map.empty[String, String]

  override protected val prefs = null

  override def preference[A: PrefCodec](key: PrefKey[A]) = new Preference[A](this, key)

  override def getFromPref[A: PrefCodec](key: PrefKey[A]) = {
    val codec = implicitly[PrefCodec[A]]
    values.get(key.str).map(codec.decode).getOrElse(key.default)
  }

  override protected def setValue[A: PrefCodec](key: PrefKey[A], value: A) =
    dispatcher(values += (key.str -> implicitly[PrefCodec[A]].encode(value)))
}

class TestUserPreferences extends UserPreferences(null, null) {

  override val dispatcher = new SerialDispatchQueue(name = "TestUserPreferenceQueue")

  private var values = Map.empty[String, String]

  override protected def getValue[A: PrefCodec](key: PrefKey[A]) = dispatcher {
    val codec = implicitly[PrefCodec[A]]
    values.get(key.str).map(codec.decode).getOrElse(key.default)
  }

  override protected def setValue[A: PrefCodec](key: PrefKey[A], value: A) =
    dispatcher(values += (key.str -> implicitly[PrefCodec[A]].encode(value)))

}