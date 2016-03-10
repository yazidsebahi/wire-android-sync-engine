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
package com.waz.testutils

import com.waz.api.VoiceChannel.JoinCallback
import com.waz.api.impl.ErrorResponse
import com.waz.service.call.VoiceChannelService._

class CallJoinSpy {
  @volatile var callJoinResult = Option.empty[CallJoinResult]

  def reset(): Unit = callJoinResult = None

  val joinCallback: JoinCallback = new JoinCallback {
    override def onCallJoined(): Unit = callJoinResult = Some(CallJoined)
    override def onVoiceChannelFull(maxJoined: Int): Unit = callJoinResult = Some(VoiceChannelFull(maxJoined))
    override def onCallJoinError(message: String): Unit = callJoinResult = Some(CallJoinError(ErrorResponse(600, message, "label")))
    override def onConversationTooBig(memberCount: Int, maxMembers: Int): Unit = callJoinResult = Some(ConversationTooBig(memberCount, maxMembers))
    override def onAlreadyJoined(): Unit = callJoinResult = Some(Unchanged)
  }
}
