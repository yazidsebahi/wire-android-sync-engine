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
package com.waz.service

import android.content.SharedPreferences.OnSharedPreferenceChangeListener
import android.content.{Context, SharedPreferences}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.events.SourceSignal
import com.waz.zms.R

import scala.concurrent.Future
import scala.util.Try

class PreferenceService(context: Context) {
  import com.waz.service.PreferenceService._

  private implicit val dispatcher = preferenceDispatcher

  lazy val analyticsEnabledPrefKey = Try(context.getResources.getString(R.string.zms_analytics_preference_key)).getOrElse("PREF_KEY_AVS_METRICS")
  lazy val analyticsEnabledPref = preferenceBooleanSignal(analyticsEnabledPrefKey)

  lazy val uiPreferences = uiPreferencesFrom(context)

  lazy val preferences = preferencesFrom(context)

  def withPreferences[A](body: SharedPreferences => A, prefs: => SharedPreferences = preferences): CancellableFuture[A] = dispatcher(body(prefs))

  def editPreferences(body: SharedPreferences.Editor => Unit, prefs: => SharedPreferences = preferences): CancellableFuture[Boolean] = dispatcher {
    val editor = prefs.edit()
    body(editor)
    editor.commit()
  }

  def intPreference(key: String, prefs: => SharedPreferences = preferences, defaultValue: Int = 0) = new Pref[Int](prefs, key, prefs.getInt(key, defaultValue), _.putInt(key, _))
  def preferenceStringSignal(key: String, prefs: => SharedPreferences = preferences, defaultValue: String = "") = new Pref[String](prefs, key, prefs.getString(key, defaultValue), _.putString(key, _))
  def preferenceBooleanSignal(key: String, prefs: => SharedPreferences = preferences, defaultValue: Boolean = false) = new Pref[Boolean](prefs, key, prefs.getBoolean(key, defaultValue), _.putBoolean(key, _))

  def withUiPreferences[A](body: SharedPreferences => A) = withPreferences(body, uiPreferences)

  def editUiPreferences(body: SharedPreferences.Editor => Unit) = editPreferences(body, uiPreferences)

  def uiPreferenceStringSignal(key: String, defaultValue: String = "") = preferenceStringSignal(key, uiPreferences, defaultValue)
  def uiPreferenceBooleanSignal(key: String, defaultValue: Boolean = false) = preferenceBooleanSignal(key, uiPreferences, defaultValue)
}

object PreferenceService {
  def uiPreferencesFrom(context: Context) = context.getSharedPreferences("com.waz.zclient.user.preferences", Context.MODE_PRIVATE)
  def preferencesFrom(context: Context) = context.getSharedPreferences("zmessaging", Context.MODE_PRIVATE)

  implicit lazy val preferenceDispatcher = new SerialDispatchQueue()

  class Pref[A](prefs: => SharedPreferences, key: String, load: => A, save: (SharedPreferences.Editor, A) => Unit) extends Preference[A] {

    override def default: A = load

    override def apply() = Future { load }

    override def :=(value: A): Future[Unit] = Future {
      val editor = prefs.edit()
      save(editor, value)
      editor.commit()
    }

    override lazy val signal: SourceSignal[A] = new SourceSignal[A](Some(load)) {

      private val listener = new OnSharedPreferenceChangeListener {
        override def onSharedPreferenceChanged(sharedPreferences: SharedPreferences, k: String): Unit =
          if (key == k) publish(load, Threading.Ui)
      }

      override def onWire():Unit = {
        prefs.registerOnSharedPreferenceChangeListener(listener)
        value = Some(load)
      }

      override def onUnwire(): Unit = {
        prefs.unregisterOnSharedPreferenceChangeListener(listener)
      }
    }
  }
}
