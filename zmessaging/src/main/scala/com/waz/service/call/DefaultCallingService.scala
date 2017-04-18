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
import com.sun.jna.Pointer
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.VideoSendState._
import com.waz.api.VoiceChannelState._
import com.waz.api.impl.ErrorResponse
import com.waz.api.{IConversation, VoiceChannelState}
import com.waz.content.MembersStorage
import com.waz.model.otr.ClientId
import com.waz.model.{ConvId, RConvId, UserId, _}
import com.waz.service.call.CallInfo.ClosedReason.{AnsweredElsewhere, Interrupted}
import com.waz.service.call.CallInfo._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.push.PushService
import com.waz.service._
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.{RichDate, RichInstant}
import com.waz.zms.CallService
import org.threeten.bp.{Duration, Instant}

import scala.concurrent.Future

/**
  * Implementation notes:
  *  - The void *arg, here represented in the native methods as a Pointer, is a reference to the object that handles the callbacks and native
  *    method calls. Since we know which class is doing the calling and handling, we don't really need to use it and so pass and receive null
  *
  *  - All modifications to the current call state should be done INSIDE a future to the serial dispatch queue - best is to perform any
  *  mutations inside `withConvWhenReady` or `withConvFromPointer`. This should ensure that all state changes are performed serially to
  *  the currentCall state in the order the user/AVS triggers the callbacks.
  */
class DefaultCallingService(context:             Context,
                            selfUserId:          UserId,
                            avs:                 AvsV3,
                            convs:               ConversationsContentUpdater,
                            membersStorage:      MembersStorage,
                            otrSyncHandler:      OtrSyncHandler,
                            flowManagerService:  FlowManagerService,
                            messagesService:     MessagesService,
                            mediaManagerService: MediaManagerService,
                            pushService:         PushService,
                            callLogService:      CallLogService,
                            network:             NetworkModeService,
                            errors:              ErrorsService) extends CallingService {

  private implicit val eventContext = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "CallingService")

  //need to ensure that flow manager and media manager are initialised for v3 (they are lazy values)
  private val fm = flowManagerService.flowManager
  private val mm = mediaManagerService.mediaManager

  val v3Available = Signal.future(avs.available.map(_ => true).recover { case _ => false })
  val currentCall = Signal(IdleCall)
  val otherSideCBR = Signal(false) // by default we assume the call is VBR

  val requestedCallVersion = Signal(-1)

  avs.init(this)

  currentCall.onChanged { i =>
    verbose(s"Calling information changed: $i")
    i.convId.foreach(CallService(context, _)) // start tracking
  }

  override def onReady(version: Int) = {
    verbose(s"Calling ready: avs version: $version")
    requestedCallVersion ! version
  }

  override def onSend(ctx: Pointer, convId: RConvId, userId: UserId, clientId: ClientId, msg: String) = {
    otherSideCBR.mutate(_ => false)
    withConv(convId) { conv =>
      sendCallMessage(conv.id, GenericMessage(Uid(), GenericContent.Calling(msg)), ctx)
    }
  }

  override def onIncomingCall(convId: RConvId, userId: UserId, videoCall: Boolean, shouldRing: Boolean) = withConv(convId) { conv =>
    verbose(s"Incoming call from $userId in conv: $convId (should ring: $shouldRing)")
    otherSideCBR.mutate(_ => false)
    currentCall.mutate {
      //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
      case IsIdle() =>
        CallInfo(
          Some(conv.id),
          userId,
          Set(userId),
          OTHER_CALLING,
          shouldRing = shouldRing,
          isGroupConv = conv.convType == IConversation.Type.GROUP,
          isVideoCall = videoCall,
          videoSendState = if(videoCall) PREVIEW else DONT_SEND)
      case cur =>
        verbose(s"Incoming call from $userId while in a call - ignoring")
        cur
    }
  }

  override def onAnsweredCall(convId: RConvId) = withConv(convId) { conv =>
    verbose(s"outgoing call answered for conv: ${conv.id}")
    currentCall.mutate {
      case call if call.convId.contains(conv.id) => call.copy(state = SELF_JOINING)
    }
  }

  override def onMissedCall(convId: RConvId, time: Instant, userId: UserId, videoCall: Boolean) = {
    verbose(s"Missed call for conversation: $convId at $time from user $userId. Video: $videoCall")
    messagesService.addMissedCallMessage(convId, userId, time)
  }

  override def onEstablishedCall(convId: RConvId, userId: UserId) = withConv(convId) { conv =>
    verbose(s"call established for conv: ${conv.id}, userId: $userId")
    currentCall.mutate{ c =>
      setVideoSendActive(conv.id, if (Seq(PREVIEW, SEND).contains(c.videoSendState)) true else false) //will upgrade call videoSendState
      setCallMuted(c.muted) //Need to set muted only after call is established
      c.copy(state = SELF_CONNECTED, estabTime = Some(Instant.now))
    }
  }

  override def onClosedCall(reason: ClosedReason, convId: RConvId, userId: UserId, metricsJson: String) = withConv(convId) { conv =>
    verbose(s"call closed for conv: ${conv.id}, userId: $userId")
    currentCall.mutate { call =>
      onCallClosed(call, reason, conv, userId)
    }
  }

  override def onVideoReceiveStateChanged(videoReceiveState: VideoReceiveState) = dispatcher { //ensure call state change is posted to dispatch queue
    verbose(s"video state changed: $videoReceiveState")
    currentCall.mutate(_.copy(videoReceiveState = videoReceiveState))
  }

  //TODO should this be synchronised too?
  override def onBitRateStateChanged() = otherSideCBR.mutate(_ => true)

  override def onCallStateChanged(convId: RConvId, state: Int) = dispatcher {
    verbose(s"call state changed, convId: $convId, state: $state")
    if (state == 2)
      currentCall.head.map {
        _.state match {
          case VoiceChannelState.SELF_CONNECTED =>
            currentCall.mutate(_.copy(shouldRing = false, state = VoiceChannelState.OTHER_CALLING))
          case _ => // Ignore
        }
      }
  }

  override def onGroupChanged(convId: RConvId) = withConv(convId) { conv =>
    verbose(s"group members changed, convId: $convId")
    currentCall.head.map {
      case call if call.convId.contains(conv.id) =>
        avs.getCallMembers(convId).map { members =>
          if (members.membc.intValue() > 0) {
            val memberArray = members.toArray(members.membc.intValue())
            currentCall.mutate(_.copy(others = memberArray.map(u => UserId(u.userid)).toSet))
          } else
            currentCall.mutate(_.copy(others = Set.empty))

          avs.freeCallMembers(members.getPointer)
          call.estabTime.foreach { est =>
            messagesService.addSuccessfulCallMessage(conv.id, call.caller, est, est.until(Instant.now))
            callLogService.addEstablishedCall(None, conv.id, call.isVideoCall)
          }
        }
      case _ => //ignore
    }
  }

  network.networkMode.onChanged { _ =>
    currentCall.head.flatMap{
      _.state match {
        case s if s != NO_ACTIVE_USERS =>
          verbose("network mode changed during call - informing AVS")
          avs.onNetworkChanged()
        case _ =>
          Future.successful[Unit](())
      }
    }
  }

  def startCall(convId: ConvId, isVideo: Boolean = false, isGroup: Boolean): Unit = withConv(convId) { conv =>
    currentCall.head.map { callInfo =>
      verbose(s"startCall currentState: ${callInfo.state}, currentConvId: ${callInfo.convId}")
      callInfo.state match {
        case OTHER_CALLING if callInfo.convId.contains(convId) =>
          acceptCall(convId, isGroup)
        case _ =>
          verbose(s"startCall convId: $convId, isVideo: $isVideo, isGroup: $isGroup")
          membersStorage.getByConv(conv.id).map { members =>
            avs.startCall(conv.remoteId, isVideo, isGroup).map {
              case 0 =>
                currentCall.mutate {
                  case IsIdle() =>
                    val others = members.map(_.userId).find(_ != selfUserId).toSet
                    //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
                    CallInfo(Some(conv.id), selfUserId, others, SELF_CALLING, isGroupConv = isGroup, isVideoCall = isVideo, videoSendState = if (isVideo) PREVIEW else DONT_SEND)
                  case cur if cur.state == OTHER_CALLING && cur.convId.contains(convId) =>
                    cur
                  case cur =>
                    error("Call already in progress, not updating")
                    cur
                }
              case err => warn(s"Unable to start call, reason: errno: $err")
            }
          }
      }
    }
  }

  def endCall(convId: ConvId): Unit = withConv(convId) { conv =>
    verbose(s"endCall: $convId")
    //wcall_end will always(???) lead to the CloseCall handler - we handle state there
    currentCall.mutate { call =>
      if (call.state == VoiceChannelState.OTHER_CALLING) {
        verbose("Incoming call ended - performing reject instead")
        avs.rejectCall(conv.remoteId, call.isGroupConv)
        onCallClosed(call.copy(hangupRequested = true), ClosedReason.Normal, conv, call.caller)
      } else {
        verbose(s"Call ended in other state: ${call.state}, ending normally")
        avs.endCall(conv.remoteId, call.isGroupConv)
        call.copy(hangupRequested = true)
      }
    }
  }

  def continueDegradedCall(): Unit = currentCall.head.map(info => (info.convId, info.outstandingMsg, info.state) match {
    case (Some(cId), Some((msg, ctx)), _) => convs.storage.setUnknownVerification(cId).map(_ => sendCallMessage(cId, msg, ctx))
    case (Some(cId), None, OTHER_CALLING) => convs.storage.setUnknownVerification(cId).map(_ => acceptCall(cId, info.isGroupConv))
    case _ => error(s"Tried resending message on invalid info: ${info.convId} in state ${info.state} with msg: ${info.outstandingMsg}")
  })

  private def sendCallMessage(convId: ConvId, msg: GenericMessage, ctx: Pointer): Unit = withConv(convId) { conv =>
    verbose(s"Sending msg on behalf of avs: convId: $convId, msg: $msg")
    otrSyncHandler.postOtrMessage(conv, msg).map {
      case Right(_) =>
        currentCall.mutate(_.copy(outstandingMsg = None))
        avs.onHttpResponse(200, "", ctx)
      case Left(ErrorResponse.Unverified) =>
        warn(s"Conversation degraded, delay sending message on behalf of AVS")
        //TODO need to handle degrading of conversation during a call
        //Currently, the call will just time out...
        currentCall.mutate(_.copy(outstandingMsg = Some(msg, ctx)))
      case Left(ErrorResponse(code, errorMsg, label)) =>
        avs.onHttpResponse(code, errorMsg, ctx)
    }
  }

  //Drop the current call in case of incoming GSM interruption
  def onInterrupted(): Unit = {
    verbose("onInterrupted - gsm call received")
    currentCall.map(_.convId).currentValue.flatten.foreach { convId =>
      //Ensure that conversation state is only performed INSIDE withConvWhenReady
      withConv(convId) { conv =>
        avs.endCall(conv.remoteId, conv.convType == IConversation.Type.GROUP)
        currentCall.mutate(_.copy(closedReason = Interrupted))
      }
    }
  }

  def acceptCall(convId: ConvId, isGroup: Boolean): Unit = withConv(convId) { conv =>
    verbose(s"answerCall: $convId")
    avs.answerCall(conv.remoteId, isGroup)
    currentCall.mutate {
      case call if call.convId.contains(conv.id) => call.copy(state = SELF_JOINING)
    }
  }

  def setCallMuted(muted: Boolean): Unit = fm.foreach { f =>
    verbose(s"setCallMuted: $muted")
    currentCall.mutate {
      case c@IsActive() =>
        f.setMute(muted)
        c.copy(muted = muted)
      case c =>
        error("No active call, ignoring mute operation")
        c
    }
  }

  def setVideoSendActive(convId: ConvId, send: Boolean): Unit = {
    verbose(s"setVideoSendActive: $convId, $send")
    withConv(convId) { conv =>
      avs.setVideoSendActive(conv.remoteId, send)
      currentCall.mutate(_.copy(videoSendState = if (send) SEND else DONT_SEND))
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

  private def onCallClosed(currentCall: CallInfo, reason: ClosedReason, conv: ConversationData, userId: UserId): CallInfo = {
    verbose(s"call closed: reason: $reason, convId: ${conv.id}, userId: $userId")
    currentCall match {
      case call if call.convId.contains(conv.id) =>
        if (reason != AnsweredElsewhere) call.state match {
          //TODO do we want a small timeout before placing a "You called" message, in case of accidental calls? maybe 5 secs
          case SELF_CALLING =>
            verbose("Call timed out out the other didn't answer - add a \"you called\" message")
            messagesService.addMissedCallMessage(conv.id, selfUserId, Instant.now)
          case OTHER_CALLING | SELF_JOINING if currentCall.shouldRing =>
            verbose("Call timed out out and we didn't answer - mark as missed call")
            messagesService.addMissedCallMessage(conv.id, userId, Instant.now)
          case SELF_CONNECTED =>
            verbose("Had a successful call, save duration as a message")
            call.estabTime.foreach { est =>
              messagesService.addSuccessfulCallMessage(conv.id, call.caller, est, est.until(Instant.now))
              //TODO can this information be gathered some other way - we really only care about successful calls.
              callLogService.addEstablishedCall(None, conv.id, call.isVideoCall)
            }
          case _ =>
            warn(s"Call closed from unexpected state: ${call.state}")
        }

        //Leave the current call with any information that makes sense to keep after a call was finished for tracking - will be overwritten on a new call.
        IdleCall.copy(
          convId          = call.convId,
          estabTime       = call.estabTime,
          caller          = call.caller,
          hangupRequested = call.hangupRequested,
          closedReason    = if (call.closedReason == Interrupted) Interrupted else reason
        )
      case call if call.convId.isDefined =>
        verbose("A call other than the current one was closed - likely missed another incoming call.")
        messagesService.addMissedCallMessage(conv.id, userId, Instant.now)
        //don't change the current call state, since the close callback was for a different conv/call
        call
      case _ =>
        warn("There was no current call defined - setting call information to idle")
        IdleCall
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

  private def withConv(convId: ConvId)(f: ConversationData => Unit): Unit = convs.convById(convId).map {
    case Some(conv) => f(conv)
    case _ => error(s"Could not find conversation: $convId")
  }
}


