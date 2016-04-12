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

import java.util

import android.os.Parcel
import com.waz.ZLog._
import com.waz.api
import com.waz.api.VoiceChannel.{JoinCallback, VideoCaptureDevice}
import com.waz.api.impl.VoiceChannel.{FakeParticipant, Participant}
import com.waz.api._
import com.waz.model.VoiceChannelData.{ChannelState, ConnectionState}
import com.waz.model._
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils._
import com.waz.utils.events.{ClockSignal, Signal}
import org.threeten.bp
import org.threeten.bp.Duration.{ZERO, between}
import org.threeten.bp.Instant.now

import scala.collection.JavaConverters._

class VoiceChannel(val id: ConvId, var data: VoiceChannelData)(implicit ui: UiModule) extends UiObservable with com.waz.api.VoiceChannel with SignalLoading {

  private implicit val tag: LogTag = logTagFor[VoiceChannel]

  private var participants = Array.empty[Participant]
  private var speaker: Boolean = false

  addLoader(_.voice.voiceChannelSignal(id))(set)
  addLoader(_.mediamanager.isSpeakerOn)(updateSpeaker)

  def updateSpeaker(speaker: Boolean): Unit = {
    debug(s"update speaker to $speaker")
    this.speaker = speaker
    notifyChanged()
  }

  def set(updated: VoiceChannelData): Unit = {
    debug(s"set($updated)")

    val activeParticipantsById = onlyRelevantParticipants(updated)

    val participantsAddedOrRemoved = {
      val newParticipants = activeParticipantsById.values filterNot { p => participants exists (_.userId == p.userId) } map participantFromData(updated.id)
      val removedUserIds = participants map (_.userId) filterNot activeParticipantsById.contains

      // update existing participants if required
      participants flatMap { p => activeParticipantsById get p.userId map (q => (p, q)) } foreach { case (p, q) =>
        if (p.getState != q.state || p.isMuted != q.muted || p.sendsVideo != q.sendsVideo) {
          p.state = q.state
          p.muted = q.muted
          p.sendsVideo = q.sendsVideo
          p.onUpdated()
        }
      }

      if (newParticipants.nonEmpty || removedUserIds.nonEmpty) {
        participants = ((participants filterNot (p => removedUserIds.contains(p.userId))) ++ newParticipants) sortBy (_.idx)
        true
      } else false
    }

    if (data.copy(participantsById = Map.empty) != updated.copy(participantsById = Map.empty) || participantsAddedOrRemoved) {
      data = data.copy(state = updated.state, deviceState = updated.deviceState, muted = updated.muted, silenced = updated.silenced,
        participantsById = activeParticipantsById, sessionId = updated.sessionId, caller = updated.caller,
        tracking = updated.tracking, video = updated.video, selfId = updated.selfId, revision = updated.revision)

      debug(s"state changed ($data), will notify listeners, participants: ${participants map (_.userId) mkString ", "}")
      notifyChanged()
    }
  }

  private def onlyRelevantParticipants(data: VoiceChannelData): Map[UserId, VoiceParticipantData] =
    data.participantsById.filter { case (k, v) => v.state == ConnectionState.Connecting || v.state == ConnectionState.Connected } - data.selfId

  private def participantFromData(convId: ConvId)(p: VoiceParticipantData): Participant = new Participant(p.userId, p.state, p.muted, p.sendsVideo, p.idx, ui.users.getUser(p.userId))(convId)

  override def getState: VoiceChannelState = if (data.state == ChannelState.Unknown) ChannelState.Idle else data.state // UI doesn't really need to know about our internal unknown state

  override def getKindOfCall: KindOfCall = data.tracking.kindOfCall

  override def isActiveDevice: Boolean = ChannelState.isActiveDevice(data.state)

  override def canTransferToThisDevice: Boolean = ChannelState.canTransfer(data.state)

  override def getParticipants: Array[api.VoiceChannel.Participant] = participants.asInstanceOf[Array[api.VoiceChannel.Participant]]

  override def getParticipant(user: api.User) = participants.find(_.userId.str == user.getId).getOrElse(new FakeParticipant(user))

  override def getCaller = data.caller.map(id => ui.users.getUser(id)).orNull

  override def getConversation: IConversation = ui.convs.convById(data.id)

  override def isSpeaker: Boolean = speaker

  override def isMuted: Boolean = data.muted

  override def isSilenced: Boolean = data.silenced

  @deprecated(message = "Please use getCauseForCallDrop() instead", since = "23")
  override def isDropped: Boolean = data.tracking.cause != CauseForCallStateEvent.REQUESTED

  override def getCauseForCallDrop: CauseForCallStateEvent = data.tracking.cause

  override def setSpeaker(speaker: Boolean): Unit = {
    if (this.speaker != speaker) {
      this.speaker = speaker
      ui.zms.flatMapFuture(_.mediamanager.setSpeaker(speaker))
      notifyChanged()
    }
  }

  override def leave(): Unit = ui.channels.leaveCall(data.id)

  override def join(callback: JoinCallback): Unit = ui.channels.joinCall(data.id, withVideo = false, Option(callback))
  override def joinWithVideo(callback: JoinCallback): Unit = ui.channels.joinCall(data.id, withVideo = true, Option(callback))

  override def unmute(): Unit = ui.channels.unmuteCall(data.id)

  override def mute(): Unit = ui.channels.muteCall(data.id)

  override def silence(): Unit = ui.channels.silenceCall(data.id)

  override def unsilence(): Unit = ui.channels.unsilenceCall(data.id)

  override def getCallStart: util.Date = data.tracking.established.map(_.javaDate).getOrElse(MessageData.UnknownInstant.javaDate)

  override def getMillisDurationOfMostRecentCall: Long = data.tracking.duration.toMillis

  override def getCallDuration(i: bp.Duration): api.UiSignal[bp.Duration] =
    UiSignal(_.voice.voiceChannelSignal(id).flatMap { data =>
      if (data.deviceState != ConnectionState.Connected) Signal.const(ZERO)
      else new ClockSignal(i.asScala).map(_ => data.tracking.established.fold2(ZERO, between(_, now)))
    })

  override def getCallSessionId: String = data.sessionId.fold("")(_.str)

  override def isVideoCall: Boolean = data.video.isVideoCall

  override def canSendVideo: Boolean = data.video.canSendVideo

  override def getVideoSendState: VideoSendState = data.video.videoSendState

  override def setVideoSendState(state: VideoSendState): Unit = ui.channels.setVideoSendState(data.id, state)

  override def getVideoCaptureDevices: util.List[VideoCaptureDevice] = (data.video.captureDevices : Vector[VideoCaptureDevice]).asJava

  override def getCurrentVideoCaptureDevice: VideoCaptureDevice = data.video.currentCaptureDevice.orNull

  override def setVideoCaptureDevice(deviceId: String): Unit = ui.channels.setVideoCaptureDevice(data.id, deviceId)

  override def equals(other: Any): Boolean = other match {
    case other: VoiceChannel => data.id == other.data.id
    case _ => false
  }

  override def hashCode: Int = data.id.hashCode

  override def writeToParcel(p: Parcel, flags: Int): Unit = p.writeString(JsonEncoder.encodeString(data))
  override def describeContents(): Int = 0
}

object VoiceChannel {
  class Participant(val userId: UserId, private[VoiceChannel] var state: ConnectionState, private[VoiceChannel] var muted: Boolean = false, var sendsVideo: Boolean = false, val idx: Int, user: User)(convId: ConvId)(implicit ui: UiModule) extends com.waz.api.VoiceChannel.Participant with UiObservable with SignalLoading {
    private implicit val logTag: LogTag = logTagFor[Participant]

    private var volume: Float = 0.0f

    override def getUser = user
    override def getState = state
    override def isMuted = muted
    override def isSendingVideo = sendsVideo
    override def getVolume = volume

    addLoader(_.voice.volumeChanged(convId, userId))(setVolume)

    def setVolume(updatedVolume: Float): Unit = {
      volume = updatedVolume // threshold is already in place in service, therefore not needed here
      verbose(s"updated volume of $userId to $updatedVolume")
      notifyChanged()
    }

    private[VoiceChannel] def onUpdated(): Unit = notifyChanged()
  }

  class FakeParticipant(user: api.User) extends com.waz.api.VoiceChannel.Participant with UiObservable {
    override def getState = ConnectionState.Idle
    override def getUser = user
    override def getVolume = 0.5f
    override def isMuted = false
    override def isSendingVideo = false
  }
}
