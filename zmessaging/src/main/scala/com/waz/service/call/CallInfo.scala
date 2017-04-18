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
import com.waz.api.VideoSendState._
import com.waz.api.VoiceChannelState.NO_ACTIVE_USERS
import com.waz.api.{VideoSendState, VoiceChannelState}
import com.waz.model.{ConvId, GenericMessage, UserId}
import com.waz.service.call.AvsV3.ClosedReason.Normal
import com.waz.service.call.AvsV3.VideoReceiveState.Stopped
import com.waz.service.call.AvsV3.{ClosedReason, VideoReceiveState}
import org.threeten.bp.Instant

/**
  * Note - We use the v2 `VoiceChannelState` here to simplify state handling in the UI, however we only use
  * a subset of them, namely: NO_ACTIVE_USERS, SELF_CALLING, OTHER_CALLING, SELF_JOINING and SELF_CONNECTED.
  */
case class CallInfo(convId:            Option[ConvId]                    = None,
                    caller:            UserId                            = UserId.Zero,
                    others:            Set[UserId]                       = Set.empty,
                    state:             VoiceChannelState                 = NO_ACTIVE_USERS,
                    shouldRing:        Boolean                           = false,
                    muted:             Boolean                           = false,
                    isGroupConv:       Boolean                           = false,
                    isVideoCall:       Boolean                           = false,
                    videoSendState:    VideoSendState                    = DONT_SEND,
                    videoReceiveState: VideoReceiveState                 = Stopped,
                    estabTime:         Option[Instant]                   = None,
                    hangupRequested:   Boolean                           = false, //whether selfUser called end call, or some other reason
                    closedReason:      ClosedReason                      = Normal,
                    outstandingMsg:    Option[(GenericMessage, Pointer)] = None) { //Any messages we were unable to send due to conv degradation
  override def toString: String =
    s"""
       |CallInfo:
       | convId:            $convId
       | caller:            $caller
       | others:            $others
       | state:             $state
       | shouldRing:        $shouldRing
       | muted:             $muted
       | isGroupConv:       $isGroupConv
       | isVideoCall:       $isVideoCall
       | videoSendState:    $videoSendState
       | videoReceiveState: $videoReceiveState
       | estabTime:         $estabTime
       | hangupRequested:   $hangupRequested
       | closedReason       $closedReason
       | hasOutstandingMsg: ${outstandingMsg.isDefined}
    """.stripMargin
}

object CallInfo {

  val IdleCall = CallInfo()

  object IsIdle {
    def unapply(arg: CallInfo): Boolean = arg.state == NO_ACTIVE_USERS
  }

  object IsActive {
    def unapply(arg: CallInfo): Boolean = !IsIdle.unapply(arg)
  }
}
