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
package com.waz.service.call

import com.waz.ZLog._
import com.waz.api.{VideoSendState, VoiceChannelState, CauseForCallStateEvent}
import com.waz.model._
import com.waz.service.call.VoiceChannelService.CallJoinResult
import com.waz.threading.Threading

import scala.concurrent.Future

trait VoiceChannelUiService { self: VoiceChannelService =>

  private implicit val tag: LogTag = logTagFor[VoiceChannelUiService]
  import Threading.Implicits.Background

  def getVoiceChannel(id: ConvId): Future[VoiceChannelData] = voiceChannel(id).flatMap(_.getData)

  def getVoiceVolume(id: ConvId, user: UserId): Future[Float] = voiceChannel(id).flatMap(_.getVolume(user))

  def joinVoiceChannel(id: ConvId, withVideo: Boolean = false): Future[CallJoinResult] = voiceChannel(id).flatMap(_.join(withVideo))

  def leaveVoiceChannel(id: ConvId): Future[VoiceChannelState] = voiceChannel(id).flatMap(_.leave(CauseForCallStateEvent.REQUESTED))

  def muteVoiceChannel(id: ConvId): Future[VoiceChannelHandle] = setVoiceChannelMuted(id, muted = true)

  def unmuteVoiceChannel(id: ConvId): Future[VoiceChannelHandle] = setVoiceChannelMuted(id, muted = false)

  def setVoiceChannelMuted(id: ConvId, muted: Boolean): Future[VoiceChannelHandle] = voiceChannel(id).flatMap(_.setMuted(muted))

  def silenceVoiceChannel(id: ConvId): Future[VoiceChannelHandle] = voiceChannel(id).flatMap(_.setSilenced(silenced = true))

  def unsilenceVoiceChannel(id: ConvId): Future[VoiceChannelHandle] = voiceChannel(id).flatMap(_.setSilenced(silenced = false))

  def setVideoSendState(id: ConvId, state: VideoSendState) = voiceChannel(id).map(_.setVideoSendState(state))

  def setVideoCaptureDevice(id: ConvId, deviceId: String) = voiceChannel(id).flatMap(_.setVideoCaptureDevice(deviceId))
}
