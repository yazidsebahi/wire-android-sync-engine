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

import com.waz.ZLog._
import com.waz.api._
import com.waz.content.VoiceChannelStorage
import com.waz.model.ConversationData.ConversationType
import com.waz.model.VoiceChannelData.ConnectionState
import com.waz.model.VoiceChannelData._
import com.waz.model._
import com.waz.model.sync.SyncJob.Priority
import com.waz.service.{ErrorsService, ZmsLifecycle}
import com.waz.service.call.VoiceChannelService.{CallJoinResult, CallJoined, Unchanged}
import com.waz.service.push.PushServiceImpl
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.RichTraversableOnce
import com.waz.utils.events.EventContext
import com.waz.utils._
import com.waz.zms.CallService
import org.threeten.bp.Duration.{ZERO, between}
import org.threeten.bp.Instant
import org.threeten.bp.Instant.now

import scala.concurrent.Future
import scala.concurrent.duration._

class VoiceChannelHandle(val id: ConvId, selfUserId: UserId, storage: VoiceChannelStorage, push: PushServiceImpl, errors: ErrorsService, lifecycle: ZmsLifecycle, service: VoiceChannelService)(implicit val dispatcher: SerialDispatchQueue) {
  import com.waz.model.VoiceChannelData.ChannelState._
  import VoiceChannelHandle._

  private implicit val ev = EventContext.Global

  @volatile var data = VoiceChannelData(
    id, ChannelState.Unknown, ConnectionState.Idle, lastSequenceNumber = None, caller = None,
    tracking = CallTrackingData(initiated = None, joined = None, established = None, duration = Duration.Zero, maxNumParticipants = 0, kindOfCall = KindOfCall.UNKNOWN, callDirection = CallDirection.INCOMING),
    video = VideoCallData.Empty, selfId = selfUserId, revision = Revision(0))

  @volatile private var previousVideoSendState = Option.empty[VideoSendState] // XXX this should really be handled by AVS

  private val initFuture: Future[Unit] = for {
    participants <- storage.listParticipants(id).future
    convData <- service.convs.convById(id)
    sorted = assignOrderingIndexToParticipants(service.getSortedParticipantIds)(participants.by(_.userId), selfUserId)
    maxNumParticipants = math.max(data.tracking.maxNumParticipants, numActiveParticipants(participants))
  } yield update(data => data.copy(
    participantsById = sorted,
    video = data.video.copy(isVideoCall = sorted.values.exists(_.sendsVideo)), // re-initialize call according to previous send state of participants
    tracking = data.tracking.copy(maxNumParticipants = maxNumParticipants, kindOfCall = convData.fold(KindOfCall.UNKNOWN) { c =>
      if (c.convType == ConversationType.Group) KindOfCall.GROUP
      else if (c.convType == ConversationType.Unknown) KindOfCall.UNKNOWN
      else KindOfCall.ONE_TO_ONE
    })), notify = false)

  initFuture foreach { _ => // compute initial state based on participants and idle device state
    push.connectedPushPromise.future map { _ => setState(returning(computeCurrentState(data))(restoreCallState), None, requestedLocally = false) }

    lifecycle.uiActive.on(dispatcher) { uiActive => // XXX don't send video when in background - ideally, this should be handled by avs completely...
      if (data.video.isVideoCall) {
        if (! uiActive) {
          debug(s"ui inactive, remembering state: ${data.video.videoSendState}, setting to DONT_SEND")
          previousVideoSendState = Some(data.video.videoSendState)
          setVideoSendState(VideoSendState.DONT_SEND)
          service.flows.setBackground(true)
        } else {
          service.flows.setBackground(false)
          debug(s"ui active again, restoring state: $previousVideoSendState")
          previousVideoSendState foreach setVideoSendState
          previousVideoSendState = None
        }
      }
    }
  }

  private def restoreCallState(localState: ChannelState): Unit = if (localState == ChannelState.UserConnected) {
    warn(s"in state $localState, syncing call state in order to maybe recover an ongoing call")
    service.sync.syncCallState(id, fromFreshNotification = true, Priority.High)
  }

  def getData: Future[VoiceChannelData] = initFuture map (_ => data)

  def getVolume(id: UserId) = getData map (_.participantsById.get(id).fold(0f)(_.volume))

  private[call] def active = data.active
  private[call] def ongoing = data.ongoing
  private[call] def deviceActive = data.deviceActive

  def handleCallStateEvent(event: CallStateEvent, resetSequence: Boolean = false) = initFuture map { _ =>
    debug(s"handleCallStateEvent: $event")

    if (isValidCallStateTransition(event, resetSequence)) {
      val before = data
      event.participants foreach { parts =>
        val joined = parts.filter(_.joined)
        if (data.participantsById.values.forall(_.state == ConnectionState.Idle)) update(data => joined.toSeq match { // new call has started, reset some data
          case Seq(caller) =>
            data.copy(
              caller = Some(caller.user),
              video = data.video.copy(isVideoCall = caller.props.contains(CallProperty.SendsVideo), wantsToSendVideo = false, canSendVideo = false), // never update the video call flag after initially setting it
              tracking = data.tracking.copy(callDirection = if (caller.user == selfUserId) CallDirection.OUTGOING else CallDirection.INCOMING))
          case _ =>
            data.copy(
              caller = joined.headOption map (_.user), // we don't know the actual caller in this case, just use any joined user
              video = data.video.copy(isVideoCall = joined flatMap (_.props) contains CallProperty.SendsVideo, wantsToSendVideo = false, canSendVideo = false),
              tracking = data.tracking.copy(callDirection = CallDirection.INCOMING)) // best guess: it's an incoming group call, we shouldn't ever get these batched if we initiated the call ourselves
        }, notify = false)

        setParticipants(parts.map { p =>
          VoiceParticipantData(id, p.user,
            state = if (!p.joined) ConnectionState.Idle else if (data.establishedFlows.contains(p.user) || p.user == selfUserId ) ConnectionState.Connected else ConnectionState.Connecting,
            sendsVideo = if (p.user == selfUserId) data.video.videoSendState == VideoSendState.SEND else p.props.contains(CallProperty.SendsVideo))
        })
      }

      update(data => data.copy(lastSequenceNumber = event.sequenceNumber, sessionId = event.sessionId, tracking = data.tracking.copy(cause = event.cause)), notify = false)
      if (before != data) service.voiceChannelSignal(id) ! data

      val device = event.device .orElse {
        if (otherChannelIsActive) None
        else if (event.participants exists { _ forall(!_.joined) }) Some(CallDeviceState(joined = false, Set.empty)) else None // set device to unconnected (force-idle) if all participants are idle
      }

      device foreach { dev =>
        val newState =
          if (data.deviceState == ConnectionState.Connected && dev.joined) ConnectionState.Connected
          else if (dev.joined) ConnectionState.Connecting
          else ConnectionState.Idle

        setDeviceState(newState)
      }

      setState(computeCurrentState(data), event.maybeLocalTime, requestedLocally = event.localTime == Event.UnknownDateTime)

    } else warn(s"ignoring invalid call state transition or duplicate/out-of-order call state events in state $data: $event")
  }

  private def isValidCallStateTransition(event: CallStateEvent, resetSequence: Boolean): Boolean = {
    (resetSequence || !VoiceChannelService.areOutOfOrder(data.lastSequenceNumber, event.sequenceNumber)) && (data.state match {
      case DeviceJoining | DeviceConnected | UserConnected =>
        event.participants forall { p =>
          p.toSeq.partition(_.user == selfUserId) match {
            case (Seq(self), Seq(other)) =>
              // This handles the special case of two users calling each other simultaneously, so that the push events go through faster than the PUT request response.
              // Can be removed once we either don't evaluate response content anymore, or don't get push messages back anymore, or the backend introduces proper sequencing...
              !self.joined || other.joined
            case _ => true
          }
        }
      case _ => true
    })
  }

  private def setState(state: ChannelState, localTime: Option[Instant], requestedLocally: Boolean): Unit = if (data.state != state) {
    val before = data
    debug(s"setState(${before.state}, $state, $requestedLocally)")
    val time = localTime orElse Some(now)

    update(data => data.copy(state = state, tracking = data.tracking.copy(requestedLocally = requestedLocally)), notify = false)

    update(data => (before.state, state) match {
      case (Idle | Unknown, OtherCalling | OthersConnected) | (Idle | Unknown, DeviceCalling) => data.copy(tracking = data.tracking.copy(initiated = time, joined = None, established = None))
      case (_, DeviceJoining)                                                                 => data.copy(tracking = data.tracking.copy(joined = time))
      case (DeviceJoining, DeviceConnected)                                                   => data.copy(tracking = data.tracking.copy(established = time, duration = Duration.Zero))
      case (_, Idle)                                                                          => data.copy(silenced = false, caller = None, video = data.video.copy(wantsToSendVideo = false, canSendVideo = false, videoSendState = VideoSendState.DONT_SEND))
      case (_, _)                                                                             => data
    }, notify = true)

    service.content.updatedActiveChannels ! ChannelUpdate(before, data, localTime)

    if (!requestedLocally && before.state == UserConnected && state == DeviceJoining) { // ugly special case: recover an ongoing call after app restart
      update(_.copy(deviceState = ConnectionState.Idle), notify = false)
      join(withVideo = data.video.isVideoCall)
    }
  }

  private def setParticipants(ps: Set[VoiceParticipantData]): Unit = {
    verbose(s"setting call participants: $ps")
    val sorted = assignOrderingIndexToParticipants(service.getSortedParticipantIds)(ps.by(_.userId), selfUserId)
    update(data => data.copy(participantsById = sorted, tracking = data.tracking.copy(maxNumParticipants = math.max(data.tracking.maxNumParticipants, numActiveParticipants(ps.toSeq)))), notify = false)
    saveParticipants(data)
    notifyChanged()
  }

  private def activeParticipantIds(ps: Map[UserId, VoiceParticipantData]): Set[UserId] = ps .filter { case (_, v) => ConnectionState.isDeviceActive(v.state) } .keySet
  private def numActiveParticipants(ps: Seq[VoiceParticipantData]): Int = ps.count { p => ConnectionState.isDeviceActive(p.state) }

  private def setDeviceState(deviceState: ConnectionState) = {
    verbose(s"setting device state: $deviceState")
    if (deviceState != data.deviceState) {
      update(_.copy(deviceState = deviceState), notify = false)
      if (deviceState == ConnectionState.Idle) {
        service.releaseFlows(id)
        update(data => data.copy(muted = false, tracking = data.tracking.copy(duration = data.tracking.established.fold(ZERO)(s => between(s, now)).asScala)), notify = false)

      } else CallService(service.context, id) // start tracking
    }
  }

  def join(withVideo: Boolean): Future[CallJoinResult] = initFuture flatMap { _ =>
    service.metrics = service.metrics.joined(id)
    debug(s"join $id, state: ${data.state}, dev: ${data.deviceState}, withVideo = $withVideo")
    if (data.deviceState == ConnectionState.Idle) {
      val caller = data.caller orElse Some(selfUserId)
      val outgoing = caller contains selfUserId

      update(data => data.copy(muted = false, silenced = false,
        caller = caller,
        tracking = data.tracking.copy(cause = CauseForCallStateEvent.REQUESTED, callDirection = if (outgoing) CallDirection.OUTGOING else CallDirection.INCOMING),
        video = data.video.copy(isVideoCall = if (outgoing) withVideo else data.video.isVideoCall, wantsToSendVideo = withVideo),
        participantsById = data.participantsById + (selfUserId -> VoiceParticipantData(id, selfUserId, ConnectionState.Connecting, muted = data.muted, sendsVideo = withVideo))), notify = false)
      saveParticipants(data)
      setDeviceState(ConnectionState.Connecting)
      setState(computeCurrentState(data), Some(Instant.now), requestedLocally = true)

      prepareCaptureDevices() flatMap { _ =>
        service.postCallState(id, data.deviceState, if (withVideo) Set(CallProperty.SendsVideo) else Set.empty).map {
          case CallJoined =>
            service.acquireFlows(id, selfUserId, activeParticipantIds(data.participantsById) - selfUserId, data.sessionId)
            CallJoined
          case other => leave(CauseForCallStateEvent.DISCONNECTED); other
        }
      }

    } else Future.successful(Unchanged)
  }

  private def prepareCaptureDevices(): Future[Unit] = {
    if (data.video.isVideoCall) service.flows.getVideoCaptureDevices map { captureDevices =>
      update(data => data.copy(video = data.video.copy(captureDevices = captureDevices, currentCaptureDevice = captureDevices.headOption)), notify = false)
      service.voiceChannelSignal(id) ! data
    } else Future.successful(())
  }

  // forcing idle state is required when we leave or get thrown out of group conversations during calls because we won't receive any further notifications about the call
  def leave(cause: CauseForCallStateEvent, forceIdleState: Boolean = false): Future[VoiceChannelState] = initFuture map { _ => leaveNow(cause, forceIdleState) }

  private def leaveNow(cause: CauseForCallStateEvent, forceIdleState: Boolean): VoiceChannelState = {
    debug(s"leave $id, state: ${data.state}, dev: ${data.deviceState}, cause: $cause, forceIdleState: $forceIdleState")
    service.metrics = service.metrics.left(id)
    if (data.state != Idle) {
      val prevState = data.deviceState

      update(data => data.copy(participantsById = updatedParticipantsOnLeaving(), video = data.video.copy(videoSendState = VideoSendState.DONT_SEND, currentCaptureDevice = data.video.captureDevices.headOption), tracking = data.tracking.copy(cause = cause)), notify = false)
      saveParticipants(data)

      setDeviceState(ConnectionState.Idle)
      val newState = computeCurrentState(data)
      setState(newState, Some(Instant.now), requestedLocally = true)

      if (prevState != ConnectionState.Idle && cause != CauseForCallStateEvent.DISCONNECTED) { // only post if device state was not already idle before (eg happens for second incoming calls)
        service.postCallState(id, ConnectionState.Idle, Set.empty, cause) onComplete { _ =>
          if (prevState != ConnectionState.Connected) {
            // FIXME: we need to generate missed call message, but it's not easy without VoiceChannelDeactivate event, maybe backend will send this event to originating device also
            service.sync.syncConversations(Set(id))
          }
          if (forceIdleState) setState(Idle, Some(Instant.now), requestedLocally = true)
        }
      }
    }
    data.state
  }

  private def updatedParticipantsOnLeaving(): Map[UserId, VoiceParticipantData] = {
    val activeParticipants = data.participantsById.filterNot { case (pId, p) => p.state == ConnectionState.Idle }
    val callShouldEnd = activeParticipants.size <= 2

    data.participantsById.map { case (pId, p) =>
      pId -> (if (callShouldEnd || p.userId == selfUserId) p.copy(state = ConnectionState.Idle) else p)
    }
  }

  def setMuted(muted: Boolean) = initFuture map { _ =>
    debug(s"setMuted($muted), previously: ${data.muted}")
    if (data.muted != muted) {
      val before = data
      update(_.copy(muted = muted), notify = true)
      if (data.deviceState != ConnectionState.Idle) service.setMuted(muted)
      service.content.updatedActiveChannels ! ChannelUpdate(before, data, None)
    }
    this
  }

  def setSilenced(silenced: Boolean): Future[VoiceChannelHandle] = initFuture map { _ =>
    debug(s"setSilenced($silenced)")
    val state = computeCurrentState(data)
    if (data.silenced != silenced && (state == OtherCalling || state == OthersConnected)) {
      val before = data
      update(_.copy(silenced = silenced), notify = true)
      if (data.deviceState != ConnectionState.Idle) {
        if (silenced) service.releaseFlows(id)
        else service.acquireFlows(id, selfUserId, activeParticipantIds(data.participantsById) - selfUserId, data.sessionId)
      }
      service.content.updatedActiveChannels ! ChannelUpdate(before, data, None)
    }
    this
  }

  def onMediaEstablished() = initFuture map { _ =>
    debug(s"onMediaEstablished($id, $selfUserId)")
    data.deviceState match {
      case ConnectionState.Connecting =>
        val before = data
        update(_.copy(deviceState = ConnectionState.Connected), notify = false)
        if (data.state == ChannelState.DeviceJoining) {
          setState(ChannelState.DeviceConnected, Some(Instant.now), requestedLocally = true)
        }
        if (data.video.isVideoCall && data.video.wantsToSendVideo) canSendVideo foreach {
          if (_) {
            data.video.currentCaptureDevice map { dev => service.setVideoCaptureDevice(id, dev.id) } getOrElse Future.successful(()) map { _ => setVideoSendState(VideoSendState.SEND) }
          } else {
            updateParticipant(selfUserId, _.copy(sendsVideo = false))
            errors.addErrorWhenActive(ErrorData(Uid(), ErrorType.CANNOT_SEND_VIDEO, convId = Some(id), responseMessage = "Unable to send video, recipient does not have the required capabilities."))
          }
        }
        service.setMuted(data.muted)
        notifyChanged()
        service.content.updatedActiveChannels ! ChannelUpdate(before, data, None)
      case ConnectionState.Connected =>
        info(s"got media established callback, but device is already connected")
      case _ =>
        warn(s"got media established, but device was not connecting, ignoring, channel: $this")
    }
  }

  def onVolumeChanged(userId: UserId, volume: Float) = initFuture map { _ =>
    data.participantsById.get(userId) foreach { p =>
      update(data => data.copy(participantsById = data.participantsById + (userId -> p.copy(volume = volume))), notify = false)
      service.volumeChanged(id, userId) ! volume
    }
  }

  def onFlowsEstablished(userFlows: Set[UserId]) = withRemoteId("onFlowsEstablished()") (remoteId => Future {
    val updatedParticipants = for {
      userId <- userFlows
      participant <- data.participantsById.get(userId)
      updated = participant.copy(state = if (participant.state == ConnectionState.Connecting) ConnectionState.Connected else participant.state)
    } yield userId -> updated

    update(data => data.copy(establishedFlows = userFlows, participantsById = data.participantsById ++ updatedParticipants), notify = false)
    saveParticipants(data)
    notifyChanged()
  })

  private def canSendVideo: Future[Boolean] = withRemoteId(s"canSendVideo()") (remoteId => Future {
    debug(s"canSendVideo($remoteId)")
    returning(service.flows.canSendVideo(remoteId)) { canSend =>
      debug(s"canSendVideo returned $canSend")
      update(data => data.copy(video = data.video.copy(canSendVideo = canSend)), notify = false)
      service.voiceChannelSignal(id) ! data
    }
  })

  def setVideoSendState(state: VideoSendState): Future[Unit] = if (state != data.video.videoSendState) {
    debug(s"setting video send state to $state")
    val before = data.video.videoSendState
    updateParticipant(selfUserId, _.copy(sendsVideo = state == VideoSendState.SEND))
    update(data => data.copy(video = data.video.copy(videoSendState = state)), notify = true)
    service.voiceChannelSignal(id) ! data
    broadcastVideoSendState(before, state)
  } else Future.successful(())

  private def broadcastVideoSendState(before: VideoSendState, state: VideoSendState): Future[Unit] = withRemoteId(s"setSendingVideo($state)") { remoteId =>
    service.flows.setVideoSendState(remoteId, state) map { _ =>
      if (before != state && data.deviceState == ConnectionState.Connected)
        verbose(s"broadcasting video send state $state")
        service.postCallState(id, data.deviceState, if (state == VideoSendState.SEND) Set(CallProperty.SendsVideo) else Set.empty, CauseForCallStateEvent.REQUESTED)  // no need to wait for this
    }
  }

  private def updateParticipant(id: UserId, updater: VoiceParticipantData => VoiceParticipantData): Unit =
    data.participantsById.get(id) map updater foreach { updated => update(data => data.copy(participantsById = data.participantsById + (id -> updated)), notify = false) }

  def setVideoCaptureDevice(deviceId: String): Future[Unit] = withRemoteId(s"setVideoCaptureDevice($deviceId)") {
    update(data => data.copy(video = data.video.copy(currentCaptureDevice = data.video.captureDevices.find(_.id == deviceId))), notify = false)
    service.flows.setVideoCaptureDevice(_, deviceId) map { _ =>
      service.voiceChannelSignal(id) ! data
    }
  }

  private def withRemoteId[T](name: String)(op: RConvId => Future[T]) = initFuture flatMap { _ => service.convs.convById(id) } flatMap {
    case Some(conv) => op(conv.remoteId)
    case None => Future.failed(new IllegalStateException(s"$name: conversation $id not found"))
  }

  private def saveParticipants(data: VoiceChannelData) = storage.setParticipants(id, data.participantsById.values.toSeq)

  private def update(updater: VoiceChannelData => VoiceChannelData, notify: Boolean): Unit = {
    val before = data
    data = updater(data).copy(revision = data.revision.incremented)
    if (data != before) notifyChanged()
  }

  private def notifyChanged() = {
    debug(s"channelChanged($id, state: ${data.state}, deviceState: ${data.deviceState}), silenced: ${data.silenced}, unjoined: ${data.unjoined}, participants: ${data.participantsById}, video: ${data.video}")
    service.convs.storage.update(id, _.copy(hasVoice = data.ongoing, voiceMuted = data.muted, unjoinedCall =  data.unjoined))

    service.voiceChannelSignal(id) ! data
  }

  private def computeCurrentState(data: VoiceChannelData): VoiceChannelState = {
    val joined = data.participantsById.values.filter(p => Set(ConnectionState.Connected, ConnectionState.Connecting) contains p.state)
    val empty = joined.isEmpty
    val calling = joined.size == 1
    val callActive = joined.size > 1
    val selfJoined = joined.exists(_.userId == selfUserId)
    val connecting = data.deviceState == ConnectionState.Connecting
    val connected = data.deviceState == ConnectionState.Connected

    if (empty && !connecting) Idle
    else if ((empty || calling && selfJoined) && (connecting || connected)) DeviceCalling
    else if (calling && selfJoined) UserCalling
    else if (calling && connecting) DeviceJoining
    else if (calling) OtherCalling
    else if (callActive && selfJoined && connected) DeviceConnected
    else if (callActive && connecting) DeviceJoining
    else if (callActive && selfJoined) UserConnected
    else if (callActive) OthersConnected
    else {
      error(s"Unhandled case for participants: ${data.participantsById.values}")
      Idle
    }
  }

  private def otherChannelIsActive: Boolean = service.content.activeChannel.currentValue.exists(_.exists(_.id != id))
}

object VoiceChannelHandle {
  private implicit val logTag: LogTag = logTagFor[VoiceChannelHandle]

  def assignOrderingIndexToParticipants(sort: Array[UserId] => Array[UserId])(ps: Map[UserId, VoiceParticipantData], selfUserId: UserId): Map[UserId, VoiceParticipantData] = {
    val activeOtherParticipantIds = ps .filterKeys (selfUserId != _) .filter { case (k, v) => ConnectionState.isDeviceActive(v.state) } .keys .to[Array]
    val sorted = sort(activeOtherParticipantIds)
    val (leftOfSelf, rightOfSelf) = sorted.splitAt(sorted.length / 2)
    val indexes = ((leftOfSelf :+ selfUserId) ++ rightOfSelf).zipWithIndex .toMap
    ps map { case (k, v) => k -> v.copy(idx = indexes.getOrElse(k, -1)) }
  }
}
