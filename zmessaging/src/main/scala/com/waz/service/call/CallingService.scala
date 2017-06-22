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
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.VideoSendState._
import com.waz.api.impl.ErrorResponse
import com.waz.content.MembersStorage
import com.waz.model.ConversationData.ConversationType
import com.waz.model.otr.ClientId
import com.waz.model.{ConvId, RConvId, UserId, _}
import com.waz.service._
import com.waz.service.call.AvsV3.ClosedReason.{AnsweredElsewhere, Interrupted, StillOngoing}
import com.waz.service.call.AvsV3.{ClosedReason, VideoReceiveState}
import com.waz.service.call.CallInfo.CallState._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.push.PushServiceImpl
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Context
import com.waz.utils.{RichDate, RichInstant}
import com.waz.zms.CallWakeService
import org.threeten.bp.{Duration, Instant}

import scala.concurrent.Future

class CallingService(context:             Context,
                     account:             AccountId,
                     selfUserId:          UserId,
                     avs:                 AvsV3,
                     convs:               ConversationsContentUpdater,
                     members:             MembersStorage,
                     otrSyncHandler:      OtrSyncHandler,
                     flowManagerService:  FlowManagerService,
                     messagesService:     MessagesService,
                     mediaManagerService: MediaManagerService,
                     pushService:         PushServiceImpl,
                     callLogService:      CallLogService,
                     network:             NetworkModeService,
                     errors:              ErrorsService) {

  private implicit val eventContext = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "CallingService")

  //need to ensure that flow manager and media manager are initialised for v3 (they are lazy values)
  private val fm = flowManagerService.flowManager
  private val mm = mediaManagerService.mediaManager

  val v3Available = Signal.future(avs.available.map(_ => true).recover { case _ => false })
  val availableCalls = Signal(Map.empty[ConvId, CallInfo]) //any call a user can potentially join
  val currentCall = Signal(Option.empty[CallInfo])
  val previousCall = Signal(Option.empty[CallInfo]) //Snapshot of last active call after hangup for tracking
  //state about any call for which we should show the CallingActivity
  val otherSideCBR = Signal(false) // by default we assume the call is VBR

  val requestedCallVersion = Signal(-1)

  avs.init(this)

  availableCalls.onChanged { cs =>
    val ids = cs.map{case (cId, _) => cId}
    verbose(s"Active calls: $ids")
  }

  currentCall.onChanged { info =>
    verbose(s"Calling information changed: $info")
    info.foreach(i => CallWakeService(context, account, i.convId)) // start tracking
  }

  def onReady(version: Int) = {
    verbose(s"Calling ready: avs version: $version")
    requestedCallVersion ! version
  }

  def onSend(ctx: Pointer, convId: RConvId, userId: UserId, clientId: ClientId, msg: String) = {
    otherSideCBR.mutate(_ => false)
    withConv(convId) { conv =>
      sendCallMessage(conv.id, GenericMessage(Uid(), GenericContent.Calling(msg)), ctx)
    }
  }

  /**
    * @param shouldRing "Also we give you a bool to indicate whether you should ring in incoming. its always true in 1:1,
    *                   true if someone called recently for group but false if the call was started more than 30 seconds ago"
    *                                                                                                               - Chris the All-Knowing.
    */
  def onIncomingCall(convId: RConvId, userId: UserId, videoCall: Boolean, shouldRing: Boolean) = withConv(convId) { conv =>
    isGroup(conv).map { isGroup =>
      verbose(s"Incoming call from $userId in conv: $convId (should ring: $shouldRing)")
      otherSideCBR.mutate(_ => false)
      val callInfo = CallInfo(
        conv.id,
        userId,
        OtherCalling,
        isGroup,
        Set(userId),
        isVideoCall = videoCall,
        //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
        videoSendState = if(videoCall) PREVIEW else DONT_SEND)

      availableCalls.mutate(calls => calls + (conv.id -> callInfo))
      currentCall.mutate {
        case None if shouldRing =>
          Some(callInfo)
        case cur => verbose(s"Incoming call from $userId while in a call or call shouldn't ring - ignoring"); cur
      }
    }
  }

  def onOtherSideAnsweredCall(convId: RConvId) = withConv(convId) { conv =>
    verbose(s"outgoing call answered for conv: ${conv.id}")
    currentCall.mutate {
      case Some(call) if call.convId == conv.id => Some(call.copy(state = SelfJoining))
      case Some(call) => warn("Other side answered non-active call, ignoring"); Some(call)
      case None => warn("Other side answered call without a current active call"); None
    }
  }

  def onMissedCall(convId: RConvId, time: Instant, userId: UserId, videoCall: Boolean) = {
    verbose(s"Missed call for conversation: $convId at $time from user $userId. Video: $videoCall")
    messagesService.addMissedCallMessage(convId, userId, time)
  }

  def onEstablishedCall(convId: RConvId, userId: UserId) = withConv(convId) { conv =>
    verbose(s"call established for conv: ${conv.id}, userId: $userId")
    currentCall.mutate {
      case Some(c) =>
        setVideoSendActive(conv.id, if (Seq(PREVIEW, SEND).contains(c.videoSendState)) true else false) //will upgrade call videoSendState
        setCallMuted(c.muted) //Need to set muted only after call is established
        //on est. group call, switch from self avatar to other user now in case `onGroupChange` is delayed
        val others = c.others + userId - selfUserId
        Some(c.copy(state = SelfConnected, estabTime = Some(Instant.now), others = others, maxParticipants = 2))
      case None => warn("Received onEstablishedCall callback without a current active call"); None
    }
  }

  def onClosedCall(reason: ClosedReason, convId: RConvId, time: Instant, userId: UserId) = withConv(convId) { conv =>
    verbose(s"call closed for conv: ${conv.id} at $time, userId: $userId")
    if (reason != StillOngoing) availableCalls.mutate(calls => calls - conv.id)
    currentCall.mutate(onCallClosed(_, reason, conv, userId))
  }

  //TODO pass call metrics to tracking when AVS are ready for it.
  def onMetricsReady(convId: RConvId, metricsJson: String) = {
    verbose(s"Call metrics for $convId, metrics: $metricsJson")
  }

  def onVideoReceiveStateChanged(videoReceiveState: VideoReceiveState) = dispatcher { //ensure call state change is posted to dispatch queue
    verbose(s"video state changed: $videoReceiveState")
    currentCall.mutate {
      case Some(c) => Some(c.copy(videoReceiveState = videoReceiveState))
      case None => warn("onVideoReceiveStateChange called without current active call"); None
    }
  }

  //TODO should this be synchronised too?
  def onBitRateStateChanged() = otherSideCBR.mutate(_ => true)

  def onGroupChanged(convId: RConvId, members: Set[UserId]) = withConv(convId) { conv =>
    verbose(s"group members changed, convId: $convId, other members: $members")
    currentCall.mutate {
      case Some(call) if call.convId == conv.id =>
        call.estabTime.foreach { est =>
          messagesService.addSuccessfulCallMessage(conv.id, call.caller, est, est.until(Instant.now))
          callLogService.addEstablishedCall(None, conv.id, call.isVideoCall)
        }
        //members doesn't include self - so add 1
        Some(call.copy(others = members, maxParticipants = math.max(call.maxParticipants, members.size + 1)))
      case call =>
        //TODO should we keep track of this info for background calls?
        info("Group members changed for non-active call, ignoring"); call
    }
  }

  network.networkMode.onChanged { _ =>
    currentCall.head.flatMap {
      case Some(call) =>
        verbose("network mode changed during call - informing AVS")
        avs.onNetworkChanged()
      case _ =>
        Future.successful[Unit](())
    }
  }

  /**
    * Either start a new call or join an active call (incoming/ongoing)
    *
    * Note: This method is called from background service, so it has to return Future which spans whole execution,
    * this ensures that CallWakeService will keep a wake lock while this function runs
    *
    * @param isVideo will be discarded if call is already active
    */
  def startCall(convId: ConvId, isVideo: Boolean = false): Future[Any] = withConvAsync(convId) { conv =>
    verbose(s"startCall $convId, $isVideo")

    (for {
      current <- currentCall.head
      active  <- availableCalls.head
      isGroup <- isGroup(conv)
      other   <-
        if (isGroup) Future.successful(Set(selfUserId))
        else if (conv.team.isEmpty) Future.successful(Set(UserId(conv.id.str)))
        else members.getByConvs(Set(conv.id)).map(_.map(_.userId).filter(_ != selfUserId).toSet)
    } yield (current, active, isGroup, other)).map {
      case (Some(cur), _, isGroup, _) if cur.convId == convId =>
        cur.state match {
          case OtherCalling =>
            verbose(s"Answering call")
            avs.answerCall(conv.remoteId, isGroup)
            currentCall.mutate(c => c.map(_.copy(state = SelfJoining)))
          case _ =>
            warn("Tried to join an already joined/connecting call - ignoring")
        }
      case (Some(cur), _, _, _) =>
        warn("Tried to start a new call while already in a call - ignoring")
      case (_, active, isGroup, _) if active.contains(convId) =>
        verbose("Joining an ongoing background call")
        avs.answerCall(conv.remoteId, isGroup)
        currentCall ! Some(active(convId).copy(state = SelfJoining))
      case (_, _, isGroup, other) =>
        verbose("No active call, starting new call")
        avs.startCall(conv.remoteId, isVideo, isGroup).map {
          case 0 =>
            //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
            val info = CallInfo(
              conv.id,
              selfUserId,
              SelfCalling,
              isGroup,
              other,
              isVideoCall = isVideo,
              videoSendState = if (isVideo) PREVIEW else DONT_SEND)
            currentCall ! Some(info)
            availableCalls.mutate(calls => calls + (conv.id -> info))
          case err => warn(s"Unable to start call, reason: errno: $err")
        }
    }
  }

  /**
   * @returns Future as this function is called from background service
   */
  def endCall(convId: ConvId): Future[Unit] = withConv(convId) { conv =>
    verbose(s"endCall: $convId")
    currentCall.mutate {
      case Some(call) =>
        verbose(s"Call ended in state: ${call.state}")
        //avs reject and end call will always trigger the onClosedCall callback - there we handle the end of the call
        if (call.state == OtherCalling) avs.rejectCall(conv.remoteId, call.isGroup) else avs.endCall(conv.remoteId, call.isGroup)
        Some(call.copy(hangupRequested = true))
      case None => warn("Tried to endCall without a current active call"); None
    }
  }

  def continueDegradedCall(): Unit = currentCall.head.map {
    case Some(info) =>
      (info.outstandingMsg, info.state) match {
        case (Some((msg, ctx)), _) => convs.storage.setUnknownVerification(info.convId).map(_ => sendCallMessage(info.convId, msg, ctx))
        case (None, OtherCalling) => convs.storage.setUnknownVerification(info.convId).map(_ => startCall(info.convId))
        case _ => error(s"Tried resending message on invalid info: ${info.convId} in state ${info.state} with msg: ${info.outstandingMsg}")
      }
    case None => warn("Tried to continue degraded call without a current active call")
  }

  private def sendCallMessage(convId: ConvId, msg: GenericMessage, ctx: Pointer): Unit = withConv(convId) { conv =>
    verbose(s"Sending msg on behalf of avs: convId: $convId, msg: $msg")
    otrSyncHandler.postOtrMessage(conv, msg).map {
      case Right(_) =>
        currentCall.mutate(_.map(_.copy(outstandingMsg = None)))
        avs.onHttpResponse(200, "", ctx)
      case Left(ErrorResponse.Unverified) =>
        warn(s"Conversation degraded, delay sending message on behalf of AVS")
        //TODO need to handle degrading of conversation during a call
        //Currently, the call will just time out...
        currentCall.mutate(_.map(_.copy(outstandingMsg = Some(msg, ctx))))
      case Left(ErrorResponse(code, errorMsg, label)) =>
        avs.onHttpResponse(code, errorMsg, ctx)
    }
  }

  //Drop the current call in case of incoming GSM interruption
  def onInterrupted(): Unit = {
    verbose("onInterrupted - gsm call received")
    currentCall.collect { case Some(info) => info.convId }.currentValue.foreach { convId =>
      //Ensure that conversation state is only performed INSIDE withConv
      withConv(convId) { conv =>
        currentCall.mutate {
          case Some(c) =>
            avs.endCall(conv.remoteId, c.isGroup)
            Some(c.copy(closedReason = Interrupted))
          case c => c //no call, nothing to do
        }
      }
    }
  }

  def setCallMuted(muted: Boolean): Unit = fm.foreach { f =>
    verbose(s"setCallMuted: $muted")
    currentCall.mutate {
      case Some(c) =>
        f.setMute(muted)
        Some(c.copy(muted = muted))
      case c =>
        warn("No active call, ignoring mute operation")
        c
    }
  }

  def setVideoSendActive(convId: ConvId, send: Boolean): Unit = {
    verbose(s"setVideoSendActive: $convId, $send")
    withConv(convId) { conv =>
      avs.setVideoSendActive(conv.remoteId, send)
      currentCall.mutate(_.map(_.copy(videoSendState = if (send) SEND else DONT_SEND)))
    }
  }

  def setAudioConstantBitRateEnabled(enabled: Int): Unit = {
    verbose(s"setting the audio cbr to $enabled")
    avs.enableAudioCbr(enabled)
  }

  val callMessagesStage = EventScheduler.Stage[CallMessageEvent] {
    case (_, events) => Future.successful(events.sortBy(_.time).foreach { e =>
      receiveCallEvent(e.content, e.time.instant, e.convId, e.from, e.sender)
    })
  }

  private def onCallClosed(currentCall: Option[CallInfo], reason: ClosedReason, conv: ConversationData, userId: UserId): Option[CallInfo] = {
    verbose(s"call closed: reason: $reason, convId: ${conv.id}, userId: $userId")
    currentCall match {
      case Some(call) if call.convId == conv.id =>
        if (reason != AnsweredElsewhere) call.state match {
          //TODO do we want a small timeout before placing a "You called" message, in case of accidental calls? maybe 5 secs
          case SelfCalling =>
            verbose("Call timed out out the other didn't answer - add a \"you called\" message")
            messagesService.addMissedCallMessage(conv.id, selfUserId, Instant.now)
          case OtherCalling | SelfJoining =>
            verbose("Call timed out out and we didn't answer - mark as missed call")
            messagesService.addMissedCallMessage(conv.id, call.caller, Instant.now)
          case SelfConnected =>
            verbose("Had a successful call, save duration as a message")
            call.estabTime.foreach { est =>
              messagesService.addSuccessfulCallMessage(conv.id, call.caller, est, est.until(Instant.now))
              //TODO can this information be gathered some other way - we really only care about successful calls.
              callLogService.addEstablishedCall(None, conv.id, call.isVideoCall)
            }
          case _ =>
            warn(s"Call closed from unexpected state: ${call.state}")
        }
        previousCall ! Some(call)
        None
      case Some(call) =>
        verbose("A call other than the current one was closed - likely missed another incoming call.")
        messagesService.addMissedCallMessage(conv.id, userId, Instant.now)
        //don't change the current call state, since the close callback was for a different conv/call
        Some(call)
      case None =>
        warn("Tried to close call on without an active call")
        None
    }
  }

  private def receiveCallEvent(msg: String, msgTime: Instant, convId: RConvId, from: UserId, sender: ClientId): Unit = {
    val drift = pushService.beDrift.currentValue.getOrElse(Duration.ZERO)
    val curTime = Instant.now + drift
    verbose(s"Received msg for avs: localTime: ${Instant.now} curTime: $curTime, drift: $drift, msgTime: $msgTime, msg: $msg")
    avs.onReceiveMessage(msg, curTime, msgTime, convId, from, sender)
  }

  private def withConv(convId: RConvId)(f: ConversationData => Unit) = convs.convByRemoteId(convId).map {
    case Some(conv) => f(conv)
    case _ => error(s"Unknown conv: $convId")
  }

  private def withConv(convId: ConvId)(f: ConversationData => Unit): Future[Unit] = convs.convById(convId) map {
    case Some(conv) => f(conv)
    case _ => error(s"Could not find conversation: $convId")
  }

  private def withConvAsync(convId: ConvId)(f: ConversationData => Future[_]): Future[Any] = convs.convById(convId) flatMap {
    case Some(conv) => f(conv)
    case _ => Future successful error(s"Could not find conversation: $convId")
  }

  /**
    * Team conversations with only 1 other user should be considered 1:1 conversations for the sake of calling.
    */
  private def isGroup(conv: ConversationData) =
    if (conv.team.isDefined) members.getByConvs(Set(conv.id)).map(_.map(_.userId)).map(_.size > 2)
    else Future.successful(conv.convType == ConversationType.Group)
}


