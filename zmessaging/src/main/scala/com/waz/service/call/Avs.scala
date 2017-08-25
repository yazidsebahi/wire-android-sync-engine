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

import com.sun.jna.Pointer
import com.waz.ZLog
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.{error, verbose}
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.call.Calling._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.jna.{Size_t, Uint32_t}
import com.waz.utils.returning
import org.threeten.bp.Instant

import scala.concurrent.{Future, Promise}

trait Avs {
  def available: Future[Unit] //Fails if not available
  def onNetworkChanged(): Future[Unit]
  def init(callingService: CallingService): Future[Unit]
  def close(): Unit
  def startCall(convId: RConvId, isVideoCall: Boolean, isGroup: Boolean): Future[Int]
  def answerCall(convId: RConvId, isGroup: Boolean): Unit
  def onHttpResponse(status: Int, reason: String, arg: Pointer): Future[Unit]
  def onReceiveMessage(msg: String, currTime: Instant, msgTime: Instant, convId: RConvId, userId: UserId, clientId: ClientId): Unit
  def endCall(convId: RConvId, isGroup: Boolean): Unit
  def rejectCall(convId: RConvId, isGroup: Boolean): Unit
  def setVideoStateHandler(handler: VideoReceiveStateHandler): Unit
  def setVideoSendActive(convId: RConvId, active: Boolean): Unit
  //cbr = constant bit rate
  def enableAudioCbr(enabled: Int): Unit
  def setAudioCbrEnabledHandler(handler: BitRateStateHandler): Unit
  def setGroupChangedHandler(handler: GroupChangedHandler): Unit
  def setCallStateHandler(handler: CallStateChangeHandler): Unit
}

/**
  * Facilitates synchronous communication with AVS and also provides a wrapper around the native code which can be easily
  * mocked for testing the CallingService
  */
class AvsImpl() extends Avs {

  private implicit val dispatcher = new SerialDispatchQueue(name = "AvsWrapper")

  import Avs._

  private var _init = Future.failed[Unit](new IllegalStateException("AVS is not yet initialised"))

  override lazy val available = Calling.v3Available

  override def init(cs: CallingService) = available.flatMap { _ =>
    verbose(s"Initialising calling for self: ${cs.selfUserId} and current client: ${cs.clientId}")
    val callingReady = Promise[Unit]()
    _init = returning(callingReady.future) {
      _.onFailure { case e =>
        error("Error initialising calling v3", e)
      }
    }

    Calling.wcall_init(
      cs.selfUserId.str,
      cs.clientId.str,
      new ReadyHandler {
        override def onReady(version: Int, arg: Pointer) = {
          cs.onReady(version)
          callingReady.success(())
        }
      },
      new SendHandler {
        override def onSend(ctx: Pointer, convId: String, userId: String, clientId: String, data: Pointer, len: Size_t, arg: Pointer) = {
          cs.onSend(ctx, RConvId(convId), UserId(userId), ClientId(clientId), data.getString(0, "UTF-8"))
          0
        }
      },
      new IncomingCallHandler {
        override def onIncomingCall(convId: String, msg_time: Uint32_t, userId: String, video_call: Boolean, should_ring: Boolean, arg: Pointer) =
          cs.onIncomingCall(RConvId(convId), UserId(userId), video_call, should_ring)
      },
      new MissedCallHandler {
        override def onMissedCall(convId: String, msg_time: Uint32_t, userId: String, video_call: Boolean, arg: Pointer): Unit =
          cs.onMissedCall(RConvId(convId), instant(msg_time), UserId(userId), video_call)
      },
      new AnsweredCallHandler {
        override def onAnsweredCall(convId: String, arg: Pointer) = cs.onOtherSideAnsweredCall(RConvId(convId))
      },
      new EstablishedCallHandler {
        override def onEstablishedCall(convId: String, userId: String, arg: Pointer) =
          cs.onEstablishedCall(RConvId(convId), UserId(userId))
      },
      new CloseCallHandler {
        override def onClosedCall(reasonCode: Int, convId: String, msg_time: Uint32_t, userId: String, arg: Pointer) =
          cs.onClosedCall(ClosedReason(reasonCode), RConvId(convId), instant(msg_time), UserId(userId))
      },
      new MetricsHandler {
        override def onMetricsReady(convId: String, metricsJson: String, arg: Pointer) =
          cs.onMetricsReady(RConvId(convId), metricsJson)
      },
      null
    )

    _init
  }.map( _ => {
    //TODO it would be nice to convince AVS to move these methods into the method wcall_init.
    Calling.wcall_set_video_state_handler(new VideoReceiveStateHandler {
      override def onVideoReceiveStateChanged(state: Int, arg: Pointer) = cs.onVideoReceiveStateChanged(VideoReceiveState(state))
    })

    Calling.wcall_set_audio_cbr_enabled_handler(new BitRateStateHandler {
      override def onBitRateStateChanged(arg: Pointer) = cs.onBitRateStateChanged()
    })

    Calling.wcall_set_group_changed_handler(new GroupChangedHandler {
      override def onGroupChanged(convId: String, arg: Pointer) = {
        //TODO change this set to an ordered set to for special audio effects?
        val mStruct = wcall_get_members(convId)
        val members = if (mStruct.membc.intValue() > 0) mStruct.toArray(mStruct.membc.intValue()).map(u => UserId(u.userid)).toSet else Set.empty[UserId]
        wcall_free_members(mStruct.getPointer)
        cs.onGroupChanged(RConvId(convId), members)
      }
    })
  })

  private def withAvsReturning[A](onSuccess: => A, onFailure: => A): Future[A] = _init.map(_ => onSuccess).recover {
    case err =>
      ZLog.error("Tried to perform action on Calling v3 after it failed to initialise", err)
      onFailure
  }

  private def withAvs(f: => Unit): Future[Unit] = withAvsReturning(f, {})

  override def onNetworkChanged() = withAvs(Calling.wcall_network_changed())

  override def close() = wcall_close()

  override def startCall(convId: RConvId, isVideoCall: Boolean, isGroup: Boolean) = withAvsReturning(wcall_start(convId.str, isVideoCall, isGroup), -1)

  override def answerCall(convId: RConvId, isGroup: Boolean) = withAvs(wcall_answer(convId.str, isGroup))

  override def onHttpResponse(status: Int, reason: String, arg: Pointer) = withAvs(wcall_resp(status, reason, arg))

  override def onReceiveMessage(msg: String, currTime: Instant, msgTime: Instant, convId: RConvId, from: UserId, sender: ClientId) = {
    val bytes = msg.getBytes("UTF-8")
    withAvs(wcall_recv_msg(bytes, bytes.length, uint32_tTime(currTime), uint32_tTime(msgTime), convId.str, from.str, sender.str))
  }

  override def endCall(convId: RConvId, isGroup: Boolean) = withAvs(wcall_end(convId.str, isGroup))

  override def rejectCall(convId: RConvId, isGroup: Boolean) = withAvs(wcall_reject(convId.str, isGroup))

  override def setVideoStateHandler(handler: VideoReceiveStateHandler) = withAvs(wcall_set_video_state_handler(handler))

  override def setVideoSendActive(convId: RConvId, active: Boolean) = withAvs(wcall_set_video_send_active(convId.str, active))

  override def enableAudioCbr(enabled: Int) = withAvs(wcall_enable_audio_cbr(enabled))

  override def setAudioCbrEnabledHandler(handler: BitRateStateHandler) = withAvs(wcall_set_audio_cbr_enabled_handler(handler))

  override def setGroupChangedHandler(handler: GroupChangedHandler) = withAvs(wcall_set_group_changed_handler(handler))

  override def setCallStateHandler(handler: CallStateChangeHandler) = withAvs(wcall_set_state_handler(handler))
}

object Avs {
  def instant(uint32_t: Uint32_t) = Instant.ofEpochMilli(uint32_t.value.toLong * 1000)

  def uint32_tTime(instant: Instant) =
    returning(Uint32_t((instant.toEpochMilli / 1000).toInt))(t => verbose(s"uint32_tTime for $instant = ${t.value}"))

  /**
    *   WCALL_REASON_NORMAL              0
    *   WCALL_REASON_ERROR               1
    *   WCALL_REASON_TIMEOUT             2
    *   WCALL_REASON_LOST_MEDIA          3
    *   WCALL_REASON_CANCELED            4
    *   WCALL_REASON_ANSWERED_ELSEWHERE  5
    *   WCALL_REASON_IO_ERROR            6
    *   WCALL_REASON_STILL_ONGOING       7
    *   WCALL_REASON_TIMEOUT_ECONN       8
    *   WCALL_REASON_DATACHANNEL         9
    *
    *   interrupted - SE only (when interrupted by GSM call)
    */
  type ClosedReason = ClosedReason.Value
  object ClosedReason extends Enumeration {
    val Normal, Error, Timeout, LostMedia, Canceled, AnsweredElsewhere, IOError, StillOngoing, TimeoutEconn, DataChannel, Interrupted = Value
  }

  /**
    *   WCALL_VIDEO_RECEIVE_STOPPED  0
    *   WCALL_VIDEO_RECEIVE_STARTED  1
    *   WCALL_VIDEO_RECEIVE_BAD_CONN 2
    */
  type VideoReceiveState = VideoReceiveState.Value
  object VideoReceiveState extends Enumeration {
    val Stopped, Started, BadConnection = Value
  }
}
