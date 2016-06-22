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
package com.waz.api

import java.util
import java.util.Date

import android.os.Parcelable.Creator
import android.os.{Parcel, Parcelable}
import com.waz.api.VoiceChannel.{JoinCallback, VideoCaptureDevice}
import com.waz.service.ZMessaging
import org.threeten.bp

object VoiceChannel {

  trait Participant extends UiObservable {
    def getState: ConnectionState
    def getVolume: Float
    def isMuted: Boolean
    def isSendingVideo: Boolean
    def getUser: User
  }

  trait VideoCaptureDevice {
    def getId: String
    def getName: String
  }

  trait JoinCallback {
    def onCallJoined(): Unit
    def onAlreadyJoined(): Unit
    def onCallJoinError(message: String): Unit
    def onConversationTooBig(memberCount: Int, maxMembers: Int): Unit
    def onVoiceChannelFull(maxJoined: Int): Unit
  }

  val CREATOR: Parcelable.Creator[VoiceChannel] = new Creator[VoiceChannel] {
    override def newArray(size: Int) = new Array[VoiceChannel](size)
    override def createFromParcel(source: Parcel): VoiceChannel = ZMessaging.currentUi.channels.getVoiceChannel(source)
  }
}

trait VoiceChannel extends UiObservable with Parcelable {
  def getState: VoiceChannelState

  def getParticipants: Array[VoiceChannel.Participant]

  def getParticipant(user: User): VoiceChannel.Participant

  /** Returns the user who initiated the call, if available and known, or <code>null</code> otherwise. */
  def getCaller: User

  def getConversation: IConversation

  def getKindOfCall: KindOfCall

  def join(callback: JoinCallback): Unit

  def joinWithVideo(callback: JoinCallback): Unit

  def leave(): Unit

  def mute(): Unit

  def unmute(): Unit

  def silence(): Unit

  def unsilence(): Unit

  def isMuted: Boolean

  def isSpeaker: Boolean

  def isSilenced: Boolean

  /** Returns true if this voice channel has been ended due to technical problem. */
  def isDropped: Boolean

  /** Returns the cause for the latest call state change (usually, this means dropped calls). */
  def getCauseForCallDrop: CauseForCallStateEvent

  def setSpeaker(speaker: Boolean): Unit

  def isActiveDevice: Boolean

  def canTransferToThisDevice: Boolean

  /** Returns local time of call start, will be reset when flows are established. */
  def getCallStart: Date

  /** Returns the duration of the most recent call. Only relevant if the call was ended, for ongoing call this will
    * be zero (most of the time). If ongoing call timer is needed then use getCallDuration(interval) or
    * getCallStart() and compute duration from it.
    */
  def getMillisDurationOfMostRecentCall: Long

  def getCallDuration(refreshInterval: bp.Duration): UiSignal[bp.Duration]

  def getCallSessionId: String

  /** Whether this call is a video call or not. */
  def isVideoCall: Boolean

  /** Whether video can be sent over this channel. */
  def canSendVideo: Boolean

  /** Whether video is currently being sent over this channel or not, or whether we just display the local preview. */
  def getVideoSendState: VideoSendState

  /** Set this to activate/deactive sending of video to the other participant(s) of the call, or set it to preview. */
  def setVideoSendState(state: VideoSendState): Unit

  /** Request available devices for capturing video from AVS. */
  def getVideoCaptureDevices: util.List[VideoCaptureDevice]

  /** Returns the currently used video capture device. Might return null if not currently set. */
  def getCurrentVideoCaptureDevice: VideoCaptureDevice

  /** Set which video capture device should be used for this video call.
    * @param deviceId The ID of the capture device to use. */
  def setVideoCaptureDevice(deviceId: String): Unit
}
