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
package com.waz.ui

import android.os.Parcel
import com.waz.Control.getOrUpdate
import com.waz.ZLog._
import com.waz.api.VoiceChannel.JoinCallback
import com.waz.api.impl.{ErrorResponse, VoiceChannel}
import com.waz.api.{CallDirection, KindOfCall, VideoSendState}
import com.waz.model.VoiceChannelData.{ChannelState, ConnectionState}
import com.waz.model._
import com.waz.service.UserService
import com.waz.service.call.VoiceChannelService._
import com.waz.threading.Threading
import com.waz.utils.{returning, JsonDecoder}

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

class Channels(implicit ui: UiModule) {

  private implicit val logTag: LogTag = logTagFor[Channels]

  val channels = new UiCache[ConvId, VoiceChannel]

  def getVoiceChannel(id: ConvId): VoiceChannel = getVoiceChannel(VoiceChannelData(
    id, ChannelState.Idle, ConnectionState.Idle, lastSequenceNumber = None, caller = None,
    tracking = CallTrackingData(initiated = None, joined = None, established = None, duration = Duration.Zero, maxNumParticipants = 0, kindOfCall = KindOfCall.UNKNOWN, callDirection = CallDirection.INCOMING),
    video = VideoCallData.Empty, selfId = UserService.SelfUserId, revision = Revision(0)))

  def getVoiceChannel(data: VoiceChannelData): VoiceChannel = returning(getOrUpdate(channels)(data.id, new VoiceChannel(data.id, data))) { ch =>
    if (ch.data.revision < data.revision) {
      debug(s"cached voice channel data out of date, overriding... ($data instead of ${ch.data})")
      ch.set(data)
    }
  }

  def getVoiceChannel(p: Parcel): VoiceChannel = getVoiceChannel(JsonDecoder.decode[VoiceChannelData](p.readString()))

  def getParticipantVolume(id: ConvId, user: UserId): Future[Float] = ui.zms.flatMapFuture(_.voice.getVoiceVolume(id, user))

  def setSpeaker(id: ConvId, speaker: Boolean) = ui.zms(_.voice.setVoiceChannelSpeaker(id, speaker))

  def joinCall(id: ConvId, withVideo: Boolean, callback: Option[JoinCallback]): Unit = ui.zms.flatMapFuture(_.voice.joinVoiceChannel(id, withVideo)).onComplete {
    case Success(CallJoined) => callback foreach (_.onCallJoined())
    case Success(Unchanged) => callback foreach (_.onAlreadyJoined())
    case Success(ConversationTooBig(members, max)) => callback foreach (_.onConversationTooBig(members, max))
    case Success(VoiceChannelFull(max)) => callback foreach (_.onVoiceChannelFull(max))
    case Success(CallJoinError(ErrorResponse(status, message, label))) => callback foreach (_.onCallJoinError(message))
    case Failure(e) =>
      error("unable to join call", e)
      callback.foreach(_.onCallJoinError(e.getMessage))
  } (Threading.Ui)

  def leaveCall(id: ConvId): Unit = ui.zms(_.voice.leaveVoiceChannel(id))

  def muteCall(id: ConvId): Unit = ui.zms(_.voice.muteVoiceChannel(id))

  def unmuteCall(id: ConvId): Unit = ui.zms(_.voice.unmuteVoiceChannel(id))

  def silenceCall(id: ConvId): Unit = ui.zms(_.voice.silenceVoiceChannel(id))

  def unsilenceCall(id: ConvId): Unit = ui.zms(_.voice.unsilenceVoiceChannel(id))

  def setVideoSendState(id: ConvId, state: VideoSendState): Unit = ui.zms(_.voice.setVideoSendState(id, state))

  def setVideoCaptureDevice(id: ConvId, deviceId: String): Unit = ui.zms(_.voice.setVideoCaptureDevice(id, deviceId))
}
