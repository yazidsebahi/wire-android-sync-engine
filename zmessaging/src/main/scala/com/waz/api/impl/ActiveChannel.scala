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

import android.app.PendingIntent
import android.content.{Intent, Context}
import android.graphics.Bitmap
import com.waz.api.{KindOfCall, VoiceChannelState, NotificationsHandler}
import com.waz.bitmap
import com.waz.model.ConvId
import com.waz.model.VoiceChannelData.ChannelState
import com.waz.zms.CallService

class ActiveChannel(context: Context, state: VoiceChannelState, callerName: String, kindOfCall: KindOfCall, override val isVideoCall: Boolean, convId: ConvId, convName: String, picture: Bitmap) extends NotificationsHandler.ActiveChannel {

  import ActiveChannel._

  override def getConversationName: String = convName

  override def getPicture: Bitmap = if (picture == bitmap.EmptyBitmap) null else picture

  override def getState: VoiceChannelState = state

  override def getConversationId: String = convId.str

  override def getCallerName: String = callerName

  override def getKindOfCall: KindOfCall = kindOfCall

  override def getJoinActionIntent: PendingIntent = joinIntent(CallService.joinIntent(context, convId))

  override def getJoinWithVideoActionIntent: PendingIntent = joinIntent(CallService.joinWithVideoIntent(context, convId))

  private def joinIntent(intent: Intent): PendingIntent = state match {
    case ChannelState.OtherCalling | ChannelState.UserCalling | ChannelState.OthersConnected =>
      PendingIntent.getService(context, JoinRequestCode, intent, PendingIntent.FLAG_UPDATE_CURRENT)
    case _ => null // can't join channel in other state
  }

  override def getLeaveActionIntent: PendingIntent = state match {
    case ChannelState.UserCalling | ChannelState.UserConnected => null
    case _ => PendingIntent.getService(context, LeaveRequestCode, CallService.leaveIntent(context, convId), PendingIntent.FLAG_UPDATE_CURRENT)
  }

  override def getSilenceActionIntent: PendingIntent = state match {
    case ChannelState.OtherCalling | ChannelState.OthersConnected | ChannelState.UserCalling | ChannelState.UserConnected =>
      PendingIntent.getService(context, SilenceRequestCode, CallService.silenceIntent(context, convId), PendingIntent.FLAG_UPDATE_CURRENT)
    case _ => null
  }

  override def toString: String = s"ActiveChannel(state = $state, convId = $convId, name = $convName, caller = $callerName, kindOfCall = $kindOfCall, isVideoCall = $isVideoCall, picture = $picture)"

  override def equals(other: Any): Boolean = other match { // required for signals to determine whether something about the channel has changed
    case o: ActiveChannel => state == o.getState && convId.str == o.getConversationId && kindOfCall == o.getKindOfCall && isVideoCall == o.isVideoCall && callerName == o.getCallerName && convName == o.getConversationName && picture == o.getPicture
    case _ => false
  }

  override def hashCode: Int = convId.hashCode
}

object ActiveChannel {
  val JoinRequestCode = 8912
  val LeaveRequestCode = 8913
  val SilenceRequestCode = 8914
}
