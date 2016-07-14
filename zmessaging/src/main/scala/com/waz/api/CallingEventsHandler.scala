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

import org.threeten.bp.Duration

trait CallingEventsHandler {
  def onCallingEvent(event: CallingEvent): Unit
}

sealed trait CallingEvent {
  def kind: KindOfCallingEvent
  def isRingingEvent: Boolean

  def kindOfCall: KindOfCall
  def direction: CallDirection
  def isVideoCall: Boolean
  def isUiActive: Boolean
  def networkMode: NetworkMode
  def isOtto: Boolean
}

sealed trait RingingEvent extends CallingEvent {
  override val isRingingEvent = true
}

trait RingingStarted extends RingingEvent {
  override val kind = KindOfCallingEvent.RINGING_STARTED
}

case class IncomingRingingStarted(kindOfCall: KindOfCall, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, inOngoingCall: Boolean, isConversationMuted: Boolean, isOtto: Boolean) extends RingingStarted {
  override val direction = CallDirection.INCOMING
}

case class OutgoingRingingStarted(kindOfCall: KindOfCall, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, isOtto: Boolean) extends RingingStarted {
  override val direction = CallDirection.OUTGOING
}

case class RingingEnded(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, isOtto: Boolean) extends RingingEvent {
  override val kind = KindOfCallingEvent.RINGING_STOPPED
}

sealed trait OngoingCallEvent extends CallingEvent {
  override val isRingingEvent = false

  def hasCallEnded: Boolean

  def numConvMembers: Int
}

sealed trait CallStarted extends OngoingCallEvent {
  override val hasCallEnded = false
}

case class CallJoined(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, numParticipants: Int, numConvMembers: Int, durationUntilJoin: Duration, isOtto: Boolean) extends CallStarted {
  override val kind = KindOfCallingEvent.CALL_JOINED
}

case class CallEstablished(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, numParticipants: Int, numConvMembers: Int, callSetupTime: Duration, isOtto: Boolean) extends CallStarted {
  override val kind = KindOfCallingEvent.CALL_ESTABLISHED
}

sealed trait CallEnded extends OngoingCallEvent {
  override val hasCallEnded = true

  def cause: CauseForCallEnd
  def maxNumParticipants: Int
  def duration: Duration
}

case class CallEndedNormally(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, numConvMembers: Int, cause: CauseForCallEnd, maxNumParticipants: Int, duration: Duration, isOtto: Boolean) extends CallEnded {
  override val kind = KindOfCallingEvent.CALL_ENDED
}

case class CallTransferred(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, numConvMembers: Int, cause: CauseForCallEnd, maxNumParticipants: Int, duration: Duration, isOtto: Boolean) extends CallEnded {
  override val kind = KindOfCallingEvent.CALL_TRANSFERRED
}

case class CallDropped(kindOfCall: KindOfCall, direction: CallDirection, isVideoCall: Boolean, isUiActive: Boolean, networkMode: NetworkMode, numConvMembers: Int, cause: CauseForCallEnd, maxNumParticipants: Int, duration: Duration, dropCause: CauseForCallStateEvent, isOtto: Boolean) extends CallEnded {
  override val kind = KindOfCallingEvent.CALL_DROPPED
}
