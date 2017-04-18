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

import com.sun.jna.{Callback, Native, Pointer}
import com.waz.CLibrary.Members
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.utils.jna.{Size_t, Uint32_t}

import scala.concurrent.Promise

object Calling {

  private val available = Promise[Unit]()
  val v3Available = available.future

  try {
    verbose("Native.register: avs")
    Native.register(Calling.getClass, "avs")
    available.success({})
  }
  catch {
    case e: Throwable =>
      error("Unable to start avs", e)
      available.failure(e)
  }

  @native def wcall_network_changed(): Unit

  @native def wcall_init(userid: String, clientid: String, readyh: Callback, sendh: Callback, incomingh: Callback, missedh: Callback, answeredh: Callback, estabh: Callback, closeh: Callback, arg: Pointer): Int

  @native def wcall_close(): Unit

  @native def wcall_start(convid: String, is_video_call: Boolean, is_group: Boolean): Int

  @native def wcall_answer(convid: String, is_group: Boolean): Unit

  @native def wcall_resp(status: Int, reason: String, arg: Pointer): Int

  @native def wcall_recv_msg(msg: Array[Byte], len: Int, curr_time: Uint32_t, msg_time: Uint32_t, convId: String, userId: String, clientId: String): Int

  @native def wcall_end(convId: String, is_group: Boolean): Unit

  @native def wcall_reject(convId: String, is_group: Boolean): Unit

  @native def wcall_set_video_state_handler(wcall_video_state_change_h: VideoReceiveStateHandler): Unit

  @native def wcall_set_video_send_active(convid: String, active: Boolean): Unit

  @native def wcall_enable_audio_cbr(enabled: Int): Unit

  @native def wcall_set_audio_cbr_enabled_handler(wcall_audio_cbr_enabled_h: BitRateStateHandler): Unit

  @native def wcall_set_group_changed_handler(wcall_group_changed_h: GroupChangedHandler): Unit

  @native def wcall_get_members(convid: String): Members

  @native def wcall_free_members(pointer: Pointer): Unit

  @native def wcall_set_state_handler(wcall_state_change_h: CallStateChangeHandler): Unit

  /* This will be called when the calling system is ready for calling.
     * The version parameter specifies the config obtained version to use
     * for calling.
     */
  trait ReadyHandler extends Callback {
    def onReady(version: Int, arg: Pointer): Unit
  }

  /* Send calling message otr data */
  trait SendHandler extends Callback {
    def onSend(ctx: Pointer, convId: String, userId: String, clientId: String, data: Pointer, len: Size_t, arg: Pointer): Int
  }

  /* Incoming call */
  trait IncomingCallHandler extends Callback {
    def onIncomingCall(convid: String, userid: String, video_call: Boolean, should_ring: Boolean, arg: Pointer): Unit
  }

  /* Missed call */
  trait MissedCallHandler extends Callback {
    def onMissedCall(convId: String, msg_time: Uint32_t, userId: String, video_call: Boolean, arg: Pointer): Unit
  }

  trait AnsweredCallHandler extends Callback {
    def onAnsweredCall(convId: String, arg: Pointer): Unit
  }

  /* Call established (with media) */
  trait EstablishedCallHandler extends Callback {
    def onEstablishedCall(convId: String, userId: String, arg: Pointer): Unit
  }

  trait CloseCallHandler extends Callback {
    def onClosedCall(reason: Int, convid: String, userid: String, metrics_json: String, arg: Pointer): Unit
  }

  trait BitRateStateHandler extends Callback {
    def onBitRateStateChanged(arg: Pointer): Unit
  }

  trait CallStateChangeHandler extends Callback {
    def onCallStateChanged(convId: String, state: Int, arg: Pointer): Unit
  }

  trait VideoReceiveStateHandler extends Callback {
    def onVideoReceiveStateChanged(state: Int, arg: Pointer): Unit
  }

  trait GroupChangedHandler extends Callback {
    def onGroupChanged(convId: String, arg: Pointer): Unit
  }

}
