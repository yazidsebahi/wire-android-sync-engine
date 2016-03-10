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

import android.content.Context
import android.net.Uri
import com.waz.ZLog._
import com.waz.media.manager.MediaManager
import com.waz.media.manager.config.Configuration
import com.waz.media.manager.context.IntensityLevel
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.EventContext
import com.waz.utils.{IoUtils, LoggedTry}
import com.waz.zms.R
import org.json.JSONObject

import scala.collection.JavaConverters._
import scala.util.Try

class MediaManagerService(context: Context, prefs: PreferenceService) {
  import com.waz.service.MediaManagerService._

  private implicit val dispatcher = new SerialDispatchQueue
  private implicit val ev = EventContext.Global
  private implicit val logTag: LogTag = logTagFor[MediaManagerService]

  lazy val mediaManager = {
    val manager = MediaManager.getInstance(context.getApplicationContext)
    audioConfig.foreach(manager.registerMediaFromConfiguration)
    manager
  }

  private lazy val prefAll = Try(context.getResources.getString(R.string.pref_sound_value_all)).getOrElse("all")
  private lazy val prefSome = Try(context.getResources.getString(R.string.pref_sound_value_some)).getOrElse("some")
  private lazy val prefNone = Try(context.getResources.getString(R.string.pref_sound_value_none)).getOrElse("none")
  private lazy val soundsPrefKey = Try(context.getResources.getString(R.string.pref_sound_option_key)).getOrElse("PREF_KEY_SOUND") // hardcoded value used in tests

  private lazy val intensityMap = Map(prefAll -> IntensityLevel.FULL, prefSome -> IntensityLevel.SOME, prefNone -> IntensityLevel.NONE)

  private lazy val soundsPref = prefs.uiPreferenceStringSignal(soundsPrefKey)

  soundsPref.signal { value =>
    val intensity = intensityMap.getOrElse(value, IntensityLevel.FULL)
    verbose(s"setting intensity to: $intensity")
    mediaManager.setIntensity(intensity)
  }

  lazy val audioConfig =
    LoggedTry(new JSONObject(IoUtils.asString(context.getAssets.open(AudioConfigAsset)))).toOption

  lazy val audioConfigUris =
    audioConfig.map(new Configuration(_).getSoundMap.asScala.mapValues { value =>
      val packageName = context.getPackageName
      Uri.parse(s"android.resource://$packageName/${context.getResources.getIdentifier(value.getPath, "raw", packageName)}")
    }).getOrElse(Map.empty[String, Uri])

  def getSoundUri(name: String): Option[Uri] = audioConfigUris.get(name)
}

object MediaManagerService {
  private val AudioConfigAsset = "android.json"
}
