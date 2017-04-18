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
import com.waz.api.CauseForCallStateEvent
import com.waz.api.impl.ErrorResponse
import com.waz.model.Event.CallProperties
import com.waz.model.VoiceChannelData.ConnectionState
import com.waz.model._
import com.waz.service._
import com.waz.service.call.FlowManagerService.EstablishedFlows
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.push.PushService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.VoiceChannelClient
import com.waz.sync.client.VoiceChannelClient.JoinCallFailed
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, SourceSignal}
import com.waz.utils.returning
import org.threeten.bp.{Instant, Duration => Duration310}

import scala.collection.breakOut
import scala.concurrent.Future

class VoiceChannelService(val context: Context, val content: VoiceChannelContent, push: PushService,
                          val lifecycle: ZmsLifecycle, val sync: SyncServiceHandle,
                          val convs: ConversationsContentUpdater, users: UserService,
                          private[call] val flows: DefaultFlowManagerService, val network: DefaultNetworkModeService,
                          errors: ErrorsService, client: VoiceChannelClient)

  extends VoiceChannelUiService { self =>

  import VoiceChannelService._
  import content._

  private implicit val ev = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "VoiceChannelService")

  @volatile private var voiceChannelSignals = Map[ConvId, SourceSignal[VoiceChannelData]]()
  @volatile private var volumeChangedSignals = Map[(ConvId, UserId), SourceSignal[Float]]()

  def voiceChannelSignal(id: ConvId): SourceSignal[VoiceChannelData] =
    voiceChannelSignals.getOrElse(id, returning(new SourceSignal[VoiceChannelData]) { voiceChannelSignals += id -> _ })

  def volumeChanged(convId: ConvId, userId: UserId): SourceSignal[Float] =
    volumeChangedSignals.getOrElse((convId, userId), returning(new SourceSignal[Float]) { volumeChangedSignals += (convId, userId) -> _ })

  @volatile private[call] var metrics = CallMetrics()

  flows.onFlowEvent.on(dispatcher) { event => metrics = metrics.flowEvent(event.kind) }
  flows.onFlowRequest.on(dispatcher) { req => metrics = metrics.flowRequest(req) }
  flows.onFlowResponse.on(dispatcher) { req => metrics = metrics.flowResponse(req) }

  val onAvsMetrics = flows.onAvsMetricsReceived.map { avsMetrics =>
    withConversation(avsMetrics.rConvId, "onAvsMetricsReceived") { (conv, selfUser) =>
      val channel = voiceChannel(conv.id, selfUser)
      channel.data.tracking.joined.fold (Future.successful(())) { instant =>
        Future.successful {
          avsMetrics.isVideoCall = channel.data.video.isVideoCall
          avsMetrics.kindOfCall = channel.data.tracking.kindOfCall
          avsMetrics
        }
      }
    }
  }

  val callStateEventsStage = EventScheduler.Stage[CallStateEvent] {
    case (_, Seq(event)) => handleCallStateEvent(event)
    case (_, convEvents) =>
      verbose(s"process batch: $convEvents, times: ${convEvents.map(_.localTime)}")
      def merge(prev: CallStateEvent, ev: CallStateEvent): CallStateEvent =
        returning(ev.copy(participants = ev.participants.orElse(prev.participants), device = ev.device.orElse(prev.device))) { copied =>
          copied.localTime = ev.localTime
        }

      // select last event with latest version of participants and device info
      val (merged, resetSequence) = convEvents.tail .foldLeft((convEvents.head, convEvents.head.sequenceNumber.isEmpty)) { case ((prev, reset), current) =>
        val updatedReset = reset || current.sequenceNumber.isEmpty
        (if (areOutOfOrder(prev.sequenceNumber, current.sequenceNumber)) merge(current, prev) else merge(prev, current), updatedReset)
      }
      handleCallStateEvent(merged, resetSequence = resetSequence)
  }

  val memberLeaveEventsStage = EventScheduler.Stage[MemberLeaveEvent] { (convId, events) =>
    users.withSelfUserFuture { selfUserId =>
      val selfLeaveEvent = (for {
        event <- events
        userId <- event.userIds if userId == selfUserId
      } yield event).lastOption

      selfLeaveEvent.fold(Future.successful(Option.empty[VoiceChannelHandle])) { event =>
        convs.convByRemoteId(event.convId) flatMap {
          case Some(conv) => getVoiceChannel(conv.id, selfUserId).future map (Some(_))
          case None => Future.successful(None)
        }
      }
    } map (_ foreach { _.leave(CauseForCallStateEvent.REQUESTED, forceIdleState = true) })
  }

  lifecycle.lifecycleState.on(dispatcher) {
    case LifecycleState.Idle =>
      // release flow for inactive call, flows are automatically acquired on incoming calls by avs, we don't want them to continue when app is paused
      activeChannel.currentValue.flatten foreach { channel =>
        if (channel.deviceState == ConnectionState.Idle) releaseFlows(channel.id)
      }
    case _ => // ignore
  }

  private[call] var prevActiveChannel = Option.empty[ConvId]

  activeChannel.on(dispatcher) { channel: Option[VoiceChannelData] =>
    if (channel.isDefined && channel.map(_.id) != prevActiveChannel) {
      verbose(s"update active channel to: $channel")
      prevActiveChannel flatMap channels.get foreach (_.leave(CauseForCallStateEvent.ONGOING_CALL))
    }
    prevActiveChannel = channel map (_.id)
  }

  flows.onMediaEstablished.on(dispatcher) { convId =>
    withConversation(convId, "onMediaEstablished") { (conv, selfUser) =>
      metrics = metrics.mediaEstablished(conv.id)
      voiceChannel(conv.id, selfUser).onMediaEstablished()
    }
  }

  flows.onFlowManagerError.on(dispatcher) { case (convId: RConvId, err: Int) =>
    withConversation(convId, "onFlowManagerError") { (conv, selfUser) =>
      debug(s"onFlowManagerError($convId, $err")
      voiceChannel(conv.id, selfUser).leave(CauseForCallStateEvent.FLOW_ERROR)
    }
  }

  flows.onVolumeChanged.on(dispatcher) { case (convId: RConvId, participantId: UserId, volume: Float) =>
    withConversation(convId, "onVolumeChanged") { (conv, selfUser) =>
      val realUserId = participantId match {
        case UserId("self") => selfUser
        case UserId("other") => UserId(convId.str)
        case userId => userId
      }
      voiceChannel(conv.id, selfUser).onVolumeChanged(realUserId, volume)
    }
  }

  flows.onFlowsEstablished.on(dispatcher) { case EstablishedFlows(convId, userIds) =>
      withConversation(convId, "onFlowsEstablished") { (conv, selfUser) =>
        metrics = metrics.flowsEstablished(conv.id, userIds)
        voiceChannel(conv.id, selfUser).onFlowsEstablished(userIds)
      }
  }

  def getVoiceChannel(id: ConvId, selfUser: UserId) = dispatcher { voiceChannel(id, selfUser) }

  def getActiveChannel: CancellableFuture[Option[VoiceChannelHandle]] = dispatcher { activeChannel.currentValue.flatten flatMap (ch => channels.get(ch.id)) }

  def getIncomingChannels: CancellableFuture[Vector[VoiceChannelHandle]] = dispatcher {
    activeChannels.currentValue.to[Vector].flatMap(_.incoming flatMap { ch => channels.get(ch.id) })
  }

  def releaseFlows(id: ConvId): Future[Unit] = convs.convById(id).map(_.foreach { conv =>
    metrics = metrics.releasedFlows(conv.id)
    flows.releaseFlows(conv.remoteId)
  })

  def acquireFlows(id: ConvId, selfId: UserId, participantIds: Set[UserId], sessionId: Option[CallSessionId]): Future[Unit] = convs.convById(id) flatMap (_.fold {
    Future.successful(error(s"Unable to retrieve conversation $id, thus unable to acquire flows for it..."))
  } { conv =>
    metrics = metrics.acquiredFlows(conv.id)
    flows.acquireFlows(conv.remoteId, selfId, participantIds, sessionId)
  })

  private[call] def voiceChannel(id: ConvId): Future[VoiceChannelHandle] =
    channels.get(id).fold {
      users.withSelfUserFuture { selfUser => dispatcher { voiceChannel(id, selfUser) } .future }
    } { ch =>
      Future.successful(ch)
    }

  private def voiceChannel(id: ConvId, selfUser: UserId): VoiceChannelHandle = channels.getOrElseUpdate(id, new VoiceChannelHandle(id, selfUser, content.storage, push, errors, lifecycle, this))

  private[call] def setMuted(muted: Boolean) = flows.setFlowManagerMuted(muted)

  private[call] def getSortedParticipantIds(participants: Array[UserId]): Array[UserId] = flows.getSortedGroupCallParticipantIds(participants.map(_.str)(breakOut)) map UserId

  def handleCallStateEvent(event: CallStateEvent, retryCount: Int = 0, resetSequence: Boolean = false) = users.withSelfUserFuture { selfUser =>
    debug(s"handleCallStateEvent: $event, time: ${event.localTime}")

    convs.processConvWithRemoteId(event.convId, retryAsync = true) { conv =>
      metrics = metrics.stateEvent(conv.id, event)
      voiceChannel(conv.id, selfUser).handleCallStateEvent(event, resetSequence)
    } (logTag, dispatcher)
  }

  def withConversation(id: RConvId, nameForErrorMessage: String)(op: (ConversationData, UserId) => Future[_]): Future[_] =
    users.withSelfUserFuture { selfUser =>
      convs.convByRemoteId(id) .flatMap {
        case Some(conv) => op(conv, selfUser)
        case None =>
          error(s"No conversation data found in $nameForErrorMessage for remoteId: $id")
          Future.successful(())
      } (dispatcher)
    }

  def postCallState(id: ConvId, deviceState: ConnectionState, props: CallProperties, cause: CauseForCallStateEvent = CauseForCallStateEvent.REQUESTED): Future[CallJoinResult] =
    (if (deviceState == ConnectionState.Idle) Future.successful(()) else push.connectedPushPromise.future) flatMap { _ =>
      debug(s"postCallState($deviceState), websocket connected")
      convs.convById(id) flatMap {
        case Some(conv) =>
          metrics.postCallStateRequest(id)
          client.updateSelfCallState(conv.remoteId, CallDeviceState(ConnectionState.isDeviceActive(deviceState), props), cause).future flatMap {
            case Right(Right(event)) => handleCallStateEvent(event) map (_ => CallJoined)
            case Right(Left(fail @ JoinCallFailed("conv-too-big", _, Some(memberCount), Some(maxMembers)))) =>
              info(s"Conversation too big ($memberCount members, $maxMembers allowed), unable to join.")
              Future.successful(ConversationTooBig(memberCount, maxMembers))
            case Right(Left(fail @ JoinCallFailed("voice-channel-full", Some(maxJoined), _, _))) =>
              info(s"Too many members already in the call (max possible: $maxJoined, unable to join.")
              Future.successful(VoiceChannelFull(maxJoined))
            case Left(e) =>
              error(s"updateSelfCallState($deviceState) failed for conv: $conv")
              Future.successful(CallJoinError(e))
            case Right(Left(fail)) =>
              error(s"updateSelfCallState($deviceState) failed for conv: $conv")
              Future.failed(new IllegalStateException(s"illegal call join failure response: $fail"))
          }
        case None =>
          error(s"updateSelfCallState($deviceState) failed, no conversation found ($id)")
          Future.failed(new IllegalStateException(s"conversation not found: $id"))
      }
    }

  def interruptActiveVoiceChannels(): CancellableFuture[Unit] = for {
    active <- getActiveChannel
    incoming <- getIncomingChannels
    _ = active foreach (_.leave(CauseForCallStateEvent.INTERRUPTED))
    _ = incoming foreach (_.setSilenced(true))
  } yield ()
}

object VoiceChannelService {
  private implicit val logTag: LogTag = logTagFor[VoiceChannelService]

  def areOutOfOrder(first: Option[CallSequenceNumber], second: Option[CallSequenceNumber]): Boolean =
    first.zip(second).headOption.exists { case (previous, current) => previous.value > current.value }

  sealed trait CallJoinResult
  case object Unchanged extends CallJoinResult
  case object CallJoined extends CallJoinResult
  case class CallJoinError(error: ErrorResponse) extends CallJoinResult
  case class ConversationTooBig(memberCount: Int, maxMembers: Int) extends CallJoinResult
  case class VoiceChannelFull(maxJoined: Int) extends CallJoinResult

  case class CallMetrics(lastEvent: Option[Instant] = None, lastJoin: Option[Instant] = None) {
    def joined(id: ConvId): CallMetrics = log(s"join($id)", isJoin = true)
    def left(id: ConvId): CallMetrics = log(s"leave($id)")
    def acquiredFlows(id: ConvId): CallMetrics = log(s"acquireFlows($id)")
    def releasedFlows(id: ConvId): CallMetrics = log(s"releaseFlows($id)")
    def mediaEstablished(id: ConvId): CallMetrics = log(s"mediaEstablished($id)")

    def stateEvent(id: ConvId, event: CallStateEvent): CallMetrics = {
      val self = event.device.fold("-") { d => s"self=${if (d.joined) "joined" else "idle"}" }
      val parts = event.participants.fold("-") { p => s"joined=[${shortenedUserIds(p filter (_.joined) map (_.user))}]" }
      val cause = event.cause.asJson
      log(s"stateEvent($id, $self, $parts, $cause)")
    }

    def postCallStateRequest(id: ConvId): CallMetrics = log(s"postCallState($id)")
    def flowEvent(kind: String): CallMetrics = log(s"flowEvent($kind)")
    def flowRequest(req: String): CallMetrics = log(s"flowRequest($req)")
    def flowResponse(req: String): CallMetrics = log(s"flowResponse($req)")
    def flowsEstablished(id: ConvId, ids: Set[UserId]): CallMetrics = log(s"flowEstablished($id, ${shortenedUserIds(ids)})")

    private def log(msg: String, isJoin: Boolean = false): CallMetrics = {
      val now = Instant.now
      val Seq(lastDelta, joinDelta) = Seq(lastEvent, lastJoin) map (_.fold("???") { Duration310.between(_, now).toMillis.toString })
      verbose(f"Call Metrics: $joinDelta%7s ms since join, $lastDelta%7s ms since previous - $msg")
      copy(lastEvent = Some(now), lastJoin = if (isJoin) Some(now) else lastJoin)
    }

    private def shortenedUserIds(ids: Set[UserId]): String = ids map (_.str take 4) mkString ","
  }
}
