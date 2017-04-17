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
import com.waz.CLibrary.Members
import com.waz.ZLog
import com.waz.ZLog.{error, verbose}
import com.waz.ZLog.ImplicitTag._
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.call.CallInfo.{ClosedReason, VideoReceiveState}
import com.waz.service.call.Calling._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.jna.{Size_t, Uint32_t}
import com.waz.utils.returning
import org.threeten.bp.Instant

import scala.concurrent.{Future, Promise}

trait CallingService {
  def onReady(version: Int)
  def onIncomingCall(convId: RConvId, userId: UserId, videoCall: Boolean, shouldRing: Boolean)
  def onAnsweredCall(convId: RConvId)
  def onEstablishedCall(convId: RConvId, userId: UserId)
  def onClosedCall(reason: ClosedReason, convId: RConvId, userId: UserId, metricsJson: String)
  def onMissedCall(convId: RConvId, time: Instant, userId: UserId, videoCall: Boolean)
  def onVideoReceiveStateChanged(videoReceiveState: VideoReceiveState)
  def onSend(ctx: Pointer, convId: RConvId, userId: UserId, clientId: ClientId, msg: String)
  def onBitRateStateChanged()
  def onCallStateChanged(convId: RConvId, state: Int)
  def onGroupChanged(convId: RConvId)
}

trait AvsV3 {
  def onNetworkChanged(): Future[Unit]
  def init(callingService: CallingService): Future[Unit]
  def close(): Unit
  def startCall(convId: RConvId, isVideoCall: Boolean, isGroup: Boolean): Future[Int]
  def answerCall(convId: RConvId, isGroup: Boolean): Unit
  def onHttpResponse(status: Int, reason: String, arg: Pointer): Future[Unit]
  def onReceiveMessage(msg: String, currTime: Instant, msgTime: Instant, convId: RConvId, userId: UserId, clientId: ClientId): Unit
  def endCall(convId: RConvId, isGroup: Boolean): Unit
  def rejectCall(convId: RConvId, isGroup: Boolean): Unit
  def setVideoStateHandler(handler: VideoStateHandler): Unit
  def setVideoSendActive(convId: RConvId, active: Boolean): Unit
  //cbr = constant bit rate
  def enableAudioCbr(enabled: Int): Unit
  def setAudioCbrEnabledHandler(handler: BitRateStateHandler): Unit
  def setGroupChangedHandler(handler: GroupChangedHandler): Unit
  def getCallMembers(convId: RConvId): Future[Members]
  def freeCallMembers(arg: Pointer): Unit
  def setCallStateHandler(handler: StateChangeHandler): Unit
}

/**
  * Facilitates synchronous communication with AVS and also provides a wrapper around the native code which can be easily
  * mocked for testing the CallingService
  */
class DefaultAvsV3(selfUserId: UserId, clientId: ClientId) extends AvsV3 {

  private implicit val dispatcher = new SerialDispatchQueue(name = "AvsWrapper")

  import AvsV3._

  private val callingReady = Promise[Unit]()
  private val _init = callingReady.future

  override def init(callingService: CallingService) = Calling.v3Available.flatMap { _ =>
    verbose(s"Initialising calling for self: $selfUserId and current client: $clientId")
    Calling.wcall_init(
      selfUserId.str,
      clientId.str,
      new ReadyHandler {
        override def onReady(version: Int, arg: Pointer) = {
          callingService.onReady(version)
          callingReady.success(())
        }
      },
      new SendHandler {
        override def onSend(ctx: Pointer, convId: String, userId: String, clientId: String, data: Pointer, len: Size_t, arg: Pointer) = {
          callingService.onSend(ctx, RConvId(convId), UserId(userId), ClientId(clientId), data.getString(0, "UTF-8"))
          0
        }
      },
      new IncomingCallHandler {
        override def onIncomingCall(convId: String, userId: String, video_call: Boolean, should_ring: Boolean, arg: Pointer) =
          callingService.onIncomingCall(RConvId(convId), UserId(userId), video_call, should_ring)
      },
      new MissedCallHandler {
        override def onMissedCall(convId: String, msg_time: Uint32_t, userId: String, video_call: Boolean, arg: Pointer): Unit =
          callingService.onMissedCall(RConvId(convId), instant(msg_time), UserId(userId), video_call)

      },
      new AnsweredCallHandler {
        override def onAnsweredCall(convId: String, arg: Pointer) = callingService.onAnsweredCall(RConvId(convId))
      },
      new EstablishedCallHandler {
        override def onEstablishedCall(convId: String, userId: String, arg: Pointer) =
          callingService.onEstablishedCall(RConvId(convId), UserId(userId))
      },
      new CloseCallHandler {
        override def onClosedCall(reasonCode: Int, convId: String, userId: String, metrics_json: String, arg: Pointer) =
          callingService.onClosedCall(ClosedReason(reasonCode), RConvId(convId), UserId(userId), metrics_json)
      },
      null
    )
    _init
  }.map( _ => {
    //TODO it would be nice to convince AVS to move these methods into the method wcall_init.
    Calling.wcall_set_video_state_handler(new VideoStateHandler {
      override def onVideoStateChanged(state: Int, arg: Pointer) = callingService.onVideoReceiveStateChanged(VideoReceiveState(state))
    })

    Calling.wcall_set_audio_cbr_enabled_handler(new BitRateStateHandler {
      override def onBitRateStateChanged(arg: Pointer) = callingService.onBitRateStateChanged()
    })

    Calling.wcall_set_group_changed_handler(new GroupChangedHandler {
      override def onGroupChanged(convId: String, arg: Pointer) = callingService.onGroupChanged(RConvId(convId))
    })

    Calling.wcall_set_state_handler(new StateChangeHandler {
      override def onCallStateChanged(convId: String, state: Int, arg: Pointer) = callingService.onCallStateChanged(RConvId(convId), state)
    })
  })

  _init.onFailure { case e =>
    error("Error initialising calling v3", e)
  }

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

  override def setVideoStateHandler(handler: VideoStateHandler) = withAvs(wcall_set_video_state_handler(handler))

  override def setVideoSendActive(convId: RConvId, active: Boolean) = withAvs(wcall_set_video_send_active(convId.str, active))

  override def enableAudioCbr(enabled: Int) = withAvs(wcall_enable_audio_cbr(enabled))

  override def setAudioCbrEnabledHandler(handler: BitRateStateHandler) = withAvs(wcall_set_audio_cbr_enabled_handler(handler))

  override def setGroupChangedHandler(handler: GroupChangedHandler) = withAvs(wcall_set_group_changed_handler(handler))

  //TODO can we make Members a more friendly class?
  override def getCallMembers(convId: RConvId) = withAvsReturning(wcall_get_members(convId.str), null)

  override def freeCallMembers(arg: Pointer) = withAvs(wcall_free_members(arg))

  override def setCallStateHandler(handler: StateChangeHandler) = withAvs(wcall_set_state_handler(handler))
}

object AvsV3 {
  def instant(uint32_t: Uint32_t) = Instant.ofEpochMilli(uint32_t.value.toLong * 1000)

  def uint32_tTime(instant: Instant) =
    returning(Uint32_t((Instant.now.toEpochMilli / 1000).toInt))(t => verbose(s"uint32_tTime for $instant = ${t.value}"))
}
