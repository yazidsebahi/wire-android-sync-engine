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
package com.waz.model

import java.util.concurrent.TimeUnit

import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import com.waz.api
import com.waz.api.VoiceChannel.VideoCaptureDevice
import com.waz.api._
import com.waz.db.Dao2
import com.waz.db.Col._
import com.waz.model.VoiceChannelData.{ChannelState, ConnectionState}
import com.waz.utils.{JsonDecoder, JsonEncoder}
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.duration.{Duration, FiniteDuration}

case class VoiceChannelData(id: ConvId,
                            state: VoiceChannelState,
                            deviceState: api.ConnectionState,
                            muted: Boolean = false,
                            silenced: Boolean = false,
                            participantsById: Map[UserId, VoiceParticipantData] = Map.empty,
                            establishedFlows: Set[UserId] = Set.empty,
                            sessionId: Option[CallSessionId] = None,
                            lastSequenceNumber: Option[CallSequenceNumber],
                            caller: Option[UserId],
                            tracking: CallTrackingData,
                            video: VideoCallData,
                            selfId: UserId,
                            revision: Revision) {

  import ChannelState._

  val unjoined = state != Unknown && state != Idle && state != DeviceConnected

  def active = ChannelState.isActive(state) && !silenced
  def ongoing = ChannelState.isOngoing(state) && !silenced
  def deviceActive = ConnectionState.isDeviceActive(deviceState)
  lazy val participants = participantsById.values.to[Vector].sortBy(_.idx)

  override def toString: String = {
    s"id: $id, state: $state, deviceState: $deviceState, muted: $muted, silenced: $silenced, participantsById: $participantsById, establishedFlows: $establishedFlows, sessionId: $sessionId, lastSequenceNumber: $lastSequenceNumber, caller: $caller, tracking: $tracking, video: $video, selfId: $selfId, revision: $revision"
  }
}

case class Revision(nr: Int) extends Ordered[Revision] {
  def compare(that: Revision): Int = this.nr - that.nr
  def incremented: Revision = Revision(nr + 1)
}

case class CaptureDeviceData(id: String, name: String) extends VideoCaptureDevice {
  override def getId = id
  override def getName = name
}

object CaptureDeviceData extends ((String, String) => CaptureDeviceData) {
  implicit lazy val Decoder: JsonDecoder[CaptureDeviceData] = new JsonDecoder[CaptureDeviceData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): CaptureDeviceData = CaptureDeviceData('id, 'name)
  }

  implicit lazy val Encoder: JsonEncoder[CaptureDeviceData] = new JsonEncoder[CaptureDeviceData] {
    override def apply(v: CaptureDeviceData): JSONObject = JsonEncoder { o =>
      o.put("id", v.id)
      o.put("name", v.name)
    }
  }
}

case class CallTrackingData(
  initiated:          Option[Instant],
  joined:             Option[Instant],
  established:        Option[Instant],
  duration:           FiniteDuration = Duration.Zero,
  maxNumParticipants: Int,
  kindOfCall:         KindOfCall,
  callDirection:      CallDirection,
  cause:              CauseForCallStateEvent = CauseForCallStateEvent.REQUESTED,
  requestedLocally:   Boolean = false) /* whether a state change was requested locally or received via some notification mechanism */

object CallTrackingData extends ((Option[Instant], Option[Instant], Option[Instant], FiniteDuration, Int, KindOfCall, CallDirection, CauseForCallStateEvent, Boolean) => CallTrackingData) {
  implicit lazy val Decoder: JsonDecoder[CallTrackingData] = new JsonDecoder[CallTrackingData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): CallTrackingData = CallTrackingData(
      decodeOptInstant('initiated), decodeOptInstant('joined), decodeOptInstant('established), FiniteDuration('duration, TimeUnit.MILLISECONDS),
    'maxNumParticipants, KindOfCall.valueOf('kindOfCall), CallDirection.valueOf('callDirection), CauseForCallStateEvent.fromJson('cause),
      'requestedLocally)
  }

  implicit lazy val Encoder: JsonEncoder[CallTrackingData] = new JsonEncoder[CallTrackingData] {
    override def apply(v: CallTrackingData): JSONObject = JsonEncoder { o =>
      v.initiated foreach (t => o.put("initiated", t.toEpochMilli))
      v.joined foreach (t => o.put("joined", t.toEpochMilli))
      v.established foreach (t => o.put("established", t.toEpochMilli))
      o.put("duration", v.duration.toMillis)
      o.put("maxNumParticipants", v.maxNumParticipants)
      o.put("kindOfCall", v.kindOfCall.name)
      o.put("callDirection", v.callDirection.name)
      o.put("cause", v.cause.asJson)
      o.put("requestedLocally", v.requestedLocally)
    }
  }
}

case class VideoCallData(isVideoCall: Boolean, wantsToSendVideo: Boolean, canSendVideo: Boolean, videoSendState: VideoSendState, captureDevices: Vector[CaptureDeviceData], currentCaptureDevice: Option[CaptureDeviceData])

object VideoCallData extends ((Boolean, Boolean, Boolean, VideoSendState, Vector[CaptureDeviceData], Option[CaptureDeviceData]) => VideoCallData) {
  val Empty = VideoCallData(isVideoCall = false, wantsToSendVideo = false, canSendVideo = false, videoSendState = VideoSendState.DONT_SEND, captureDevices = Vector.empty, currentCaptureDevice = None)

  implicit lazy val Decoder: JsonDecoder[VideoCallData] = new JsonDecoder[VideoCallData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): VideoCallData = VideoCallData('isVideoCall, 'wantsToSendVideo, 'canSendVideo, VideoSendState.valueOf('videoSendState), arrayColl[CaptureDeviceData, Vector](js.getJSONArray("captureDevices")), opt[CaptureDeviceData]('currentCaptureDevice))
  }

  implicit lazy val Encoder: JsonEncoder[VideoCallData] = new JsonEncoder[VideoCallData] {
    override def apply(v: VideoCallData): JSONObject = JsonEncoder { o =>
      o.put("isVideoCall", v.isVideoCall)
      o.put("wantsToSendVideo", v.wantsToSendVideo)
      o.put("canSendVideo", v.canSendVideo)
      o.put("videoSendState", v.videoSendState.name)
      o.put("captureDevices", JsonEncoder.arr(v.captureDevices))
      v.currentCaptureDevice foreach { dev => o.put("currentCaptureDevice", JsonEncoder.encode[CaptureDeviceData](dev)) }
    }
  }
}

object VoiceParticipantData extends ((ConvId, UserId, api.ConnectionState, Boolean, Boolean, Float, Int) => VoiceParticipantData) {
  implicit lazy val Decoder: JsonDecoder[VoiceParticipantData] = new JsonDecoder[VoiceParticipantData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): VoiceParticipantData =
      VoiceParticipantData(convId = decodeConvId('convId), userId = decodeUserId('userId), state = api.ConnectionState.valueOf('state), muted = 'muted, sendsVideo = 'sendsVideo, volume = 'volume, idx = 'idx)
  }

  implicit lazy val Encoder: JsonEncoder[VoiceParticipantData] = new JsonEncoder[VoiceParticipantData] {
    override def apply(v: VoiceParticipantData): JSONObject = JsonEncoder { o =>
      o.put("convId", v.convId.str)
      o.put("userId", v.userId.str)
      o.put("state", v.state.name)
      o.put("muted", v.muted)
      o.put("sendsVideo", v.sendsVideo)
      o.put("volume", v.volume)
      o.put("idx", v.idx)
    }
  }

  implicit object VoiceParticipantDataDao extends Dao2[VoiceParticipantData, ConvId, UserId] {
    val ConvId = id[ConvId]('conv_id).apply(_.convId)
    val UserId = id[UserId]('user_id).apply(_.userId)
    val State = text[ConnectionState]('dev_state, _.name, ConnectionState(_))(_.state)
    val Muted = bool('muted)(_.muted)
    val SendsVideo = bool('sends_video)(_.sendsVideo)
    override val idCol = (ConvId, UserId)

    override val table = Table("VoiceParticipants", ConvId, UserId, State, Muted, SendsVideo)

    override def apply(implicit cursor: Cursor): VoiceParticipantData = VoiceParticipantData(ConvId, UserId, State, Muted, SendsVideo)

    def deleteForChannel(id: ConvId)(implicit db: SQLiteDatabase) = delete(ConvId, id)

    def listForChannel(id: ConvId)(implicit db: SQLiteDatabase) = list(find(ConvId, id))

    def hasActiveParticipant(id: ConvId, user: UserId)(implicit db: SQLiteDatabase) =
      single(db.query(table.name, null, s"${ConvId.name} = ? and ${UserId.name} = ? and ${State.name} in (?, ?)", Array(id.str, user.str, State.col.sqlLiteral(ConnectionState.Connected), State.col.sqlLiteral(ConnectionState.Connecting)), null, null, null)).isDefined
  }
}

case class VoiceParticipantData(convId: ConvId, userId: UserId, state: api.ConnectionState, muted: Boolean = false, sendsVideo: Boolean = false, volume: Float = 0f, idx: Int = 0)

object VoiceChannelData extends ((ConvId, VoiceChannelState, api.ConnectionState, Boolean, Boolean, Map[UserId, VoiceParticipantData], Set[UserId], Option[CallSessionId], Option[CallSequenceNumber], Option[UserId], CallTrackingData, VideoCallData, UserId, Revision) => VoiceChannelData) {

  type ChannelState = VoiceChannelState
  object ChannelState {
    import com.waz.api.VoiceChannelState._

    val Unknown = UNKNOWN
    val Idle = NO_ACTIVE_USERS

    val DeviceCalling = SELF_CALLING
    val DeviceConnected = SELF_CONNECTED
    val DeviceJoining = SELF_JOINING
    val OtherCalling = OTHER_CALLING
    val UserCalling = TRANSFER_CALLING
    val UserConnected = TRANSFER_READY
    val OthersConnected = OTHERS_CONNECTED
    val valuesMap = Seq(UNKNOWN, NO_ACTIVE_USERS, SELF_CALLING, SELF_CONNECTED, SELF_JOINING, OTHER_CALLING, OTHERS_CONNECTED, TRANSFER_CALLING, TRANSFER_READY).map(s => s.name() -> s).toMap

    def apply(name: String): ChannelState = valuesMap(name)

    def isActive(state: VoiceChannelState): Boolean = state != Idle && state != Unknown
    def isOngoing(state: VoiceChannelState): Boolean = state == DeviceCalling || state == DeviceJoining || state == DeviceConnected || state == UserCalling || state == UserConnected

    def isConnecting(state: VoiceChannelState): Boolean = state match {
      case DeviceCalling | DeviceJoining | OtherCalling => true
      case _ => false
    }

    def isActiveDevice(state: VoiceChannelState): Boolean = state == DeviceCalling || state == DeviceConnected || state == DeviceJoining

    def canTransfer(state: VoiceChannelState): Boolean = state == UserCalling || state == UserConnected
  }

  type ConnectionState = api.ConnectionState
  object ConnectionState {
    import api.ConnectionState._

    val Unknown = UNKNOWN
    val Idle = NOT_CONNECTED
    val Connecting = CONNECTING
    val Connected = CONNECTED

    val valuesMap = Seq(UNKNOWN, NOT_CONNECTED, CONNECTING, CONNECTED).map(s => s.name -> s).toMap

    def apply(name: String): ConnectionState = valuesMap(name)

    def isDeviceActive(deviceState: ConnectionState) = deviceState == Connecting || deviceState == Connected
  }

  implicit lazy val Decoder: JsonDecoder[VoiceChannelData] = new JsonDecoder[VoiceChannelData] {
    import JsonDecoder._
    import UserId.UserIdDecoder
    import VoiceParticipantData.Decoder

    override def apply(implicit js: JSONObject): VoiceChannelData = VoiceChannelData(
      id = 'id, state = VoiceChannelState.valueOf('state), deviceState = api.ConnectionState.valueOf('deviceState),
      muted = 'muted, silenced = 'silenced,
      participantsById = decodeArray[VoiceParticipantData]('participants).map(p => p.userId -> p).toMap,
      establishedFlows = decodeArray[UserId]('establishedFlows).toSet,
      sessionId = decodeOptId[CallSessionId]('sessionId), lastSequenceNumber = decodeOptCallSequenceNumber('lastSequenceNumber),
      caller = decodeOptId[UserId]('caller),
      tracking = decode[CallTrackingData]('tracking),
      video = decode[VideoCallData]('video),
      selfId = 'self,
      revision = Revision('revision)
    )
  }

  implicit lazy val Encoder: JsonEncoder[VoiceChannelData] = new JsonEncoder[VoiceChannelData] {
    import UserId.UserIdEncoder
    import VoiceParticipantData.Encoder

    override def apply(v: VoiceChannelData): JSONObject = JsonEncoder { o =>
      o.put("id", v.id.str)
      o.put("state", v.state.name)
      o.put("deviceState", v.deviceState.name())
      o.put("muted", v.muted)
      o.put("silenced", v.silenced)
      o.put("participants", JsonEncoder.arr(v.participantsById.values.toSeq))
      o.put("establishedFlows", JsonEncoder.arr(v.establishedFlows.toSeq))
      v.sessionId foreach (id => o.put("sessionId", id.str))
      v.lastSequenceNumber foreach (n => o.put("lastSequenceNumber", n.value))
      v.caller foreach (id => o.put("caller", id.str))
      o.put("tracking", JsonEncoder.encode[CallTrackingData](v.tracking))
      o.put("video", JsonEncoder.encode[VideoCallData](v.video))
      o.put("self", v.selfId.str)
      o.put("revision", v.revision.nr)
    }
  }
}
