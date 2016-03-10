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
package com.waz.api.impl

import com.waz.ZLog._
import com.waz.api
import com.waz.model.VoiceChannelData
import com.waz.threading.Threading
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.EventContext

class ActiveVoiceChannels(implicit ui: UiModule) extends com.waz.api.ActiveVoiceChannels with UiObservable with SignalLoading {
  private implicit val dispatcher = Threading.Ui
  private implicit val ec: EventContext = EventContext.Global
  private implicit val logTag: LogTag = logTagFor[ActiveVoiceChannels]

  private var ongoingCall = Option.empty[VoiceChannel]
  private var incomingCall = Option.empty[VoiceChannel]

  private var upToDate = false

  ui.onResumed {
    case Some(context) => verbose("resumed")
    case None => upToDate = false; verbose("paused, won't stay up-to-date")
  }

  addLoader(_.voiceContent.ongoingAndTopIncomingChannel, (Option.empty[VoiceChannelData], Option.empty[VoiceChannelData])) { data =>
    ongoingCall = data._1 map (data => ui.channels.getVoiceChannel(data))
    incomingCall = data._2 map (data => ui.channels.getVoiceChannel(data))

    debug(s"onLoaded($data)")
    upToDate = true
    notifyChanged()
  }

  override def isUpToDate: Boolean = upToDate

  override def hasOngoingCall: Boolean = ongoingCall.isDefined
  override def getOngoingCall: api.VoiceChannel = ongoingCall.orNull

  override def hasIncomingCall: Boolean = incomingCall.isDefined
  override def getIncomingCall: api.VoiceChannel = incomingCall.orNull
}
