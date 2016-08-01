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
import com.waz.api.NotificationsHandler.NotificationsHandlerFactory
import com.waz.api._
import com.waz.content.{ConversationStorage, UsersStorage, VoiceChannelStorage}
import com.waz.model.VoiceChannelData.ChannelState._
import com.waz.model.VoiceChannelData.ConnectionState
import com.waz.model.{ConvId, VoiceChannelData}
import com.waz.service.tracking.TrackingEventsService
import com.waz.service.{NetworkModeService, ZmsLifecycle}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.{Instant, Duration => JsrDuration}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

case class ActiveChannels(ongoing: Option[VoiceChannelData], incoming: Vector[VoiceChannelData])
case class ChannelUpdate(before: VoiceChannelData, updated: VoiceChannelData, timestamp: Option[Instant], uiActive: Option[Boolean] = None, networkMode: Option[NetworkMode] = None)

class VoiceChannelContent(context: Context, val storage: VoiceChannelStorage, handlerFactory: NotificationsHandlerFactory,
    convs: ConversationStorage, users: UsersStorage, lifecycle: ZmsLifecycle, network: NetworkModeService, callLog: CallLogService) { self =>

  import VoiceChannelContent._

  private implicit val tag = logTagFor[VoiceChannelContent]

  private lazy val eventsHandler = handlerFactory.getCallingEventsHandler

  var stopOutgoingRinging = CancellableFuture.successful(())
  var stopIncoming = CancellableFuture.successful(())

  val channels = new mutable.HashMap[ConvId, VoiceChannelHandle]

  val updatedActiveChannels = new Publisher[ChannelUpdate]

  val updatedActiveChannelsWithUiActiveAndNetworkMode: EventStream[ChannelUpdate] =
    new EventStreamWithAuxSignal(new EventStreamWithAuxSignal(updatedActiveChannels, lifecycle.uiActive), network.networkMode) map { case ((c: ChannelUpdate, uiActive), mode) => c.copy(uiActive = uiActive, networkMode = mode) }

  val activeChannels = new AggregatingSignal[ChannelUpdate, ActiveChannels](updatedActiveChannelsWithUiActiveAndNetworkMode, Future.successful(ActiveChannels(None, Vector.empty)), { case (current, ChannelUpdate(before, updated, localTime, uiActive, networkMode)) =>
    val remainingTimeout = localTime .map { t => IncomingGroupCallTimeout - JsrDuration.between(t, Instant.now()).toMillis.millis }

    val convIsOtto = convs.get(updated.id).flatMap(c => c.fold2(Future.successful(false), conv => TrackingEventsService.isOtto(conv, users)))(Threading.Background)
    val ongoing = updateOngoingChannel(current.ongoing, updated)
    if (current.ongoing != ongoing) convIsOtto.foreach(isOtto => updateEvents(updateEventsOnOngoingChannelChange(current.ongoing, updated, uiActive getOrElse false, networkMode getOrElse NetworkMode.OFFLINE, isOtto)))(Threading.Background)

    val incoming = updateIncomingChannels(current.incoming, before, updated, remainingTimeout)
    if (current.incoming.headOption != incoming.headOption) convIsOtto.foreach(isOtto => updateEvents(updateEventsOnIncomingChannelChange(current.incoming.headOption, incoming.headOption, ongoing.isDefined, uiActive getOrElse false, networkMode getOrElse NetworkMode.OFFLINE, isOtto)))(Threading.Background)

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

  private def updateEventsOnOngoingChannelChange(channel: Option[VoiceChannelData], updated: VoiceChannelData, uiActive: Boolean, mode: NetworkMode, isOtto: Boolean): CallingEventsHandler => Unit = { handler =>
    debug(s"updateEventsOnOngoingChannelChange($channel, $updated, uiActive = $uiActive)")

    val (previous, current) = (channel map (_.state) getOrElse Idle, updated.state)
    if (previous != current) {
      if (previous == DeviceCalling) channel foreach { ch => handler.onCallingEvent(RingingEnded(ch.tracking.kindOfCall, ch.tracking.callDirection, ch.video.isVideoCall, uiActive, mode, isOtto)) }

      stopOutgoingRinging.cancel()
      if (!updated.silenced) {
        (previous, current) match {
          case (_, DeviceCalling) =>
            logEvent(KindOfCallingEvent.RINGING_STARTED, updated)
            handler.onCallingEvent(OutgoingRingingStarted(updated.tracking.kindOfCall, updated.video.isVideoCall, uiActive, mode, isOtto))
            stopOutgoingRinging =
              if (updated.tracking.kindOfCall == KindOfCall.GROUP) CancellableFuture.delayed(OutgoingGroupCallTimeout) {
                logEvent(KindOfCallingEvent.RINGING_STOPPED, updated)
                handler.onCallingEvent(RingingEnded(updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode, isOtto))
              } (Threading.Background)
              else CancellableFuture.successful(())

          case (DeviceJoining, UserConnected) | (DeviceConnected, UserConnected) =>
            logEvent(KindOfCallingEvent.CALL_TRANSFERRED, updated)
            handler.onCallingEvent(CallTransferred(updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode, updated.participantsById.size, CauseForCallEnd.TRANSFERRED, updated.tracking.maxNumParticipants, updated.tracking.duration, isOtto))

          case (_, DeviceJoining) =>
            logEvent(KindOfCallingEvent.CALL_JOINED, updated)
            handler.onCallingEvent(CallJoined(
              updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode,
              updated.participantsById.values count (p => p.state == ConnectionState.Connected || p.state == ConnectionState.Connecting),
              updated.participantsById.size, computeDurationBetween(updated.tracking.initiated, updated.tracking.joined), isOtto))

          case (_, DeviceConnected) =>
            logEvent(KindOfCallingEvent.CALL_ESTABLISHED, updated)
            handler.onCallingEvent(CallEstablished(
              updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode,
              updated.participantsById.values count (p => p.state == ConnectionState.Connected || p.state == ConnectionState.Connecting),
              updated.participantsById.size, computeDurationBetween(updated.tracking.joined, updated.tracking.established), isOtto))

          case (_, Idle) | (_, OthersConnected) if (previous == DeviceConnected || previous == DeviceJoining) && updated.tracking.cause != CauseForCallStateEvent.REQUESTED =>
            logEvent(KindOfCallingEvent.CALL_DROPPED, updated)

            handler.onCallingEvent(CallDropped(
              updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode, updated.participantsById.size,
              if (updated.tracking.requestedLocally) CauseForCallEnd.SELF else CauseForCallEnd.OTHER,
              updated.tracking.maxNumParticipants, updated.tracking.duration, updated.tracking.cause, isOtto))


          case (_, Idle) | (_, OthersConnected) if previous == DeviceConnected || previous == DeviceJoining =>
            logEvent(KindOfCallingEvent.CALL_ENDED, updated)
            handler.onCallingEvent(CallEndedNormally(
              updated.tracking.kindOfCall, updated.tracking.callDirection, updated.video.isVideoCall, uiActive, mode, updated.participantsById.size,
              if (updated.tracking.requestedLocally) CauseForCallEnd.SELF else CauseForCallEnd.OTHER,
              updated.tracking.maxNumParticipants, updated.tracking.duration, isOtto))

          case _ =>
        }
      }
    }
  }

  private def computeDurationBetween(start: Option[Instant], end: Option[Instant]): JsrDuration = (for {
    start <- start
    end <- end
  } yield JsrDuration.between(start, end)) getOrElse JsrDuration.ZERO

  private def logEvent(e: KindOfCallingEvent, d: VoiceChannelData): Unit = callLog.add(e, d.sessionId, d.id, d.video.isVideoCall)

  private def updateEventsOnIncomingChannelChange(before: Option[VoiceChannelData], after: Option[VoiceChannelData], ongoing: Boolean, uiActive: Boolean, mode: NetworkMode, isOtto: Boolean): CallingEventsHandler => Unit = { handler =>
    debug(s"updateEventsOnIncomingChannelChange($before, $after, ongoing = $ongoing, uiActive = $uiActive)")

    (before, after) match {
      case (None, Some(incoming)) =>
        convs.get(incoming.id) .foreach { conv =>
          handler.onCallingEvent(IncomingRingingStarted(incoming.tracking.kindOfCall, incoming.video.isVideoCall, uiActive, mode, ongoing, conv exists (_.muted), isOtto))
        } (Threading.Ui)

      case (Some(incoming), None) =>
        handler.onCallingEvent(RingingEnded(incoming.tracking.kindOfCall, incoming.tracking.callDirection, incoming.video.isVideoCall, uiActive, mode, isOtto))

      case _ => // nothing to do
    }
  }

  private def updateEvents(f: CallingEventsHandler => Unit) = Future(f(eventsHandler))(Threading.Ui)
}

object VoiceChannelContent {
  private val IncomingGroupCallTimeout = 30.seconds
  private val OutgoingGroupCallTimeout = 30.seconds
}
