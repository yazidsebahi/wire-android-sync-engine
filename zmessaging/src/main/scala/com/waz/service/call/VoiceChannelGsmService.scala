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

import android.content.Context
import android.telephony.{PhoneStateListener, TelephonyManager}
import com.waz.ZLog._
import com.waz.api.VoiceChannelState
import com.waz.service.LifecycleState
import com.waz.threading.Threading
import com.waz.utils.events.EventContext

class VoiceChannelGsmService(voice: VoiceChannelService, callingService: DefaultCallingService) {
  import voice._

  private implicit val eventContext = EventContext.Global
  private implicit val dispatcher = Threading.Ui
  private implicit val logTag: LogTag = logTagFor[VoiceChannelGsmService]

  private lazy val telephonyManager = context.getSystemService(Context.TELEPHONY_SERVICE).asInstanceOf[TelephonyManager]

  private var listening = false

  lazy val listener = new PhoneStateListener {
    override def onCallStateChanged(state: Int, incomingNumber: String): Unit = {

      import TelephonyManager._
      val stateStr = state match {
        case CALL_STATE_IDLE    => "idle"
        case CALL_STATE_RINGING => "ringing"
        case CALL_STATE_OFFHOOK => "offhook"
      }

      info(s"GSM call state changed: $stateStr")
      if (state == CALL_STATE_OFFHOOK) dropWireCalls()
    }
  }

  (for {
    hasContent <- content.activeChannel.map(_.isDefined)
    v3call <- callingService.currentCall.map(_.state != VoiceChannelState.NO_ACTIVE_USERS)
  } yield hasContent || v3call).on(dispatcher) {
    case false => stopListening()
    case true =>
      if (hasGsmCall) {
        info(s"GSM call in progress, leaving voice channels or v3 call")
        dropWireCalls()
      }
      else startListening()
  }

  voice.content.activeChannels.map(_.incoming.headOption).on(dispatcher)(_ foreach { _ => if (hasGsmCall) dropWireCalls() })

  private def startListening() = if (!listening) {
    info("startListening")
    telephonyManager.listen(listener, PhoneStateListener.LISTEN_CALL_STATE)
    listening = true
  }

  private def stopListening() = if (listening) {
    info("stopListening")
    telephonyManager.listen(listener, PhoneStateListener.LISTEN_NONE)
    listening = false
  }

  def hasGsmCall = telephonyManager.getCallState == TelephonyManager.CALL_STATE_OFFHOOK

  def dropWireCalls() = {
    interruptActiveVoiceChannels()
    callingService.onInterrupted()
  }
}
