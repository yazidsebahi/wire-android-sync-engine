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
import com.waz.ZLog._
import com.waz.api._
import com.waz.content.{ConversationStorage, UsersStorage, VoiceChannelStorage}
import com.waz.model.VoiceChannelData.ChannelState._
import com.waz.model.{ConvId, VoiceChannelData}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events._
import org.threeten.bp.{Instant, Duration => JsrDuration}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

case class ActiveChannels(ongoing: Option[VoiceChannelData], incoming: Vector[VoiceChannelData])
case class ChannelUpdate(before: VoiceChannelData, updated: VoiceChannelData, timestamp: Option[Instant])

class VoiceChannelContent(context: Context, val storage: VoiceChannelStorage, convs: ConversationStorage, users: UsersStorage, callLog: CallLogService) { self =>

  import VoiceChannelContent._

  private implicit val tag = logTagFor[VoiceChannelContent]

  var stopIncoming = CancellableFuture.successful(())

  val channels = new mutable.HashMap[ConvId, VoiceChannelHandle]

  val updatedActiveChannels = new Publisher[ChannelUpdate]

  val activeChannels = new AggregatingSignal[ChannelUpdate, ActiveChannels](updatedActiveChannels, Future.successful(ActiveChannels(None, Vector.empty)), { case (current, ChannelUpdate(before, updated, localTime)) =>
    val remainingTimeout = localTime .map { t => IncomingGroupCallTimeout - JsrDuration.between(t, Instant.now()).toMillis.millis }

    val ongoing = updateOngoingChannel(current.ongoing, updated)
    if (current.ongoing != ongoing) logEstablished(current.ongoing, updated)

    val incoming = updateIncomingChannels(current.incoming, before, updated, remainingTimeout)
    current.copy(ongoing = ongoing, incoming = incoming)
  })

  val activeChannel = activeChannels map (_.ongoing)
  val ongoingAndTopIncomingChannel = activeChannels map { ch => (ch.ongoing, ch.incoming.headOption) }

  private def updateOngoingChannel(current: Option[VoiceChannelData], updated: VoiceChannelData): Option[VoiceChannelData] = {
    if (updated.ongoing) Some(updated) // switch to new ongoing call
    else if (current.exists(_.id == updated.id)) None // was ongoing, but not anymore
    else current // ongoing channel remains unchanged
  }

  private def updateIncomingChannels(channels: Vector[VoiceChannelData], before: VoiceChannelData, updated: VoiceChannelData, remainingTimeout: Option[FiniteDuration]): Vector[VoiceChannelData] = {
    val idx = channels.indexWhere(_.id == updated.id)
    if (idx == -1) { // updated channel was not incoming before
      if ((before.state == Idle || before.state == Unknown) && updated.state == OtherCalling && !updated.silenced) {
        stopIncoming.cancel()

        if (updated.tracking.kindOfCall != KindOfCall.GROUP) channels :+ updated else {
          remainingTimeout filter (_ > 0.seconds) map { t =>
            debug(s"Timeout incoming group call: ${updated.id}")
            // for incoming group calls, overlay and ringtone time out after a timeout
            stopIncoming = CancellableFuture.delayed(t) { updatedActiveChannels ! ChannelUpdate(updated, updated.copy(silenced = true), None) } (Threading.Background) // silenced for inbox purposes only...
            channels :+ updated

          } getOrElse channels
        }
      }
      else channels

    } else { // updated channel already is in incoming list, check if it has to be removed or just modified
      if (Set(OtherCalling, OthersConnected).contains(updated.state) && !updated.silenced) channels.updated(idx, updated)
      else {
        if (idx == 0) stopIncoming.cancel() // here, we have to check whether this is the first incoming channel - if so, we have to also cancel the incoming timeout...
        channels.filterNot(_.id == updated.id) // remove channel, not incoming anymore
      }
    }
  }

  private def logEstablished(channel: Option[VoiceChannelData], updated: VoiceChannelData): Unit = {
    val (previous, current) = (channel map (_.state) getOrElse Idle, updated.state)
    if (previous != current) {
      (previous, current) match {
        case (_, DeviceConnected) => callLog.addEstablishedCall(updated.sessionId, updated.id, updated.video.isVideoCall)
        case _ =>
      }
    }
  }
}

object VoiceChannelContent {
  private val IncomingGroupCallTimeout = 30.seconds
}
