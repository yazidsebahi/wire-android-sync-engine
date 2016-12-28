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


import java.util.Date

import android.content.Context
import com.sun.jna.Pointer
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.VideoSendState._
import com.waz.api.VoiceChannelState._
import com.waz.api.impl.ErrorResponse
import com.waz.content.MembersStorage
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.call.CallInfo._
import com.waz.service.call.Calling._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.{EventScheduler, MediaManagerService}
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.utils.jna.{Size_t, Uint32_t}
import com.waz.utils.{RichDate, returning}
import com.waz.zms.CallService
import org.threeten.bp.Instant

import scala.concurrent.{Future, Promise}

/**
  * Implementation notes:
  *  - The void *arg, here represented in the native methods as a Pointer, is a reference to the object that handles the callbacks and native
  *    method calls. Since we know which class is doing the calling and handling, we don't really need to use it and so pass and receive null
  */
class CallingService(context: Context, selfUserId: UserId, clientId: ClientId, convs: ConversationsContentUpdater, membersStorage: MembersStorage,
                     otrSyncHandler: OtrSyncHandler, flowManagerService: FlowManagerService, messagesService: MessagesService,
                     mediaManagerService: MediaManagerService) {

  private implicit val eventContext = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "CallingService")

  private val fm = flowManagerService.flowManager

  val v3Available = Signal.future(Calling.v3Available.map(_ => true).recover { case _ => false })
  val currentCall = Signal(IdleCall)
  val missedCall  = EventStream[(ConvId, UserId, Instant)]()

  private val response = EventStream[(Either[ErrorResponse, Date], Pointer)]()

  currentCall.onChanged { i =>
    verbose(s"Calling information changed: $i")
    i.convId.foreach(CallService(context, _)) // start tracking
  }

  private lazy val init = Calling.v3Available.map { _ =>

    def withConversation(convId: String)(f: ConversationData => Unit) = convs.convByRemoteId(RConvId(convId)).map {
      case Some(conv) => f(conv)
      case _ => error(s"Unknown conv: $convId")
    }

    val callingReady = Promise[Unit]()

    verbose(s"Initialising calling for self: $selfUserId and current client: $clientId")
    Calling.wcall_init(selfUserId.str, clientId.str,
      new ReadyHandler {
        override def invoke(version: Int, arg: Pointer): Unit = {
          verbose(s"Calling ready: avs version: $version")
          callingReady.success(())
        }
      },
      new SendHandler {
        override def invoke(ctx: Pointer, convId: String, userId: String, clientId: String, data: Pointer, len: Size_t, arg: Pointer): Int = {
          val msg = data.getString(0, "UTF-8")
          verbose(s"Sending msg on behalf of avs: convId: $convId, userId: $userId, clientId: $clientId, msg: $msg")
          withConversation(convId) {
            otrSyncHandler.postOtrMessage(_, GenericMessage(Uid(), GenericContent.Calling(msg))).map { case (resp) => response ! (resp, ctx) }
          }
          0
        }
      },
      new IncomingCallHandler {
        override def invoke(convId: String, userId: String, video_call: Boolean, arg: Pointer): Unit = {
          verbose(s"Incoming call from $userId in conv: $convId")
          withConversation(convId) { conv =>
            currentCall.mutate {
              //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
              case IsIdle() => CallInfo(Some(conv.id), Set(UserId(userId)), OTHER_CALLING, isVideoCall = video_call, videoSendState = if(video_call) PREVIEW else DONT_SEND)
              case cur =>
                warn(s"Call already in progress, received incoming call from $userId")
                cur
            }
          }
        }
      },
      new MissedCallHandler {
        override def invoke(convId: String, msg_time: Uint32_t, userId: String, video_call: Boolean, arg: Pointer): Unit = {
          val t = instant(msg_time)
          missedCall ! (ConvId(convId), UserId(userId), t)
          verbose(s"Missed call for conversation: $convId at $t from user $userId. Video: $video_call")
        }
      },
      new EstablishedCallHandler {
        override def invoke(convId: String, userId: String, arg: Pointer): Unit = {
          verbose(s"call established for conv: $convId, userId: $userId")
          withConversation(convId) { conv =>
            currentCall.mutate{ c =>
              setVideoSendActive(conv.id, if (Seq(PREVIEW, SEND).contains(c.videoSendState)) true else false) //will upgrade call videoSendState
              c.copy(state = SELF_CONNECTED, estabTime = Some(Instant.now))
            }
          }
        }
      },
      new CloseCallHandler {
        override def invoke(reason: Int, convId: String, userId: String, metrics_json: String, arg: Pointer): Unit = {
          verbose(s"call closed: reason: ${ClosedReason(reason)}, convId: $convId, userId: $userId, metrics: $metrics_json")
          currentCall.mutate {
            case c if c.state == OTHER_CALLING && ClosedReason(reason) == ClosedReason.Normal =>
              verbose("Call timeout out and we didn't answer - mark as missed call")
              messagesService.addMissedCallMessage(ConvId(convId), UserId(userId), Instant.now)
              IdleCall
            case _ => IdleCall.copy(closedReason = ClosedReason(reason))
          }
        }
      },
      null
    )
    callingReady.future
  }.map { _ =>
    //Handles the state of received video, not sent video
    Calling.wcall_set_video_state_handler(new VideoStateHandler {
      override def invoke(state: Int, arg: Pointer): Unit = {
        verbose(s"video state changed: ${VideoReceiveState(state)}")
        currentCall.mutate(_.copy(videoReceiveState = VideoReceiveState(state)))
      }
    })
  }

  init.onFailure { case e =>
    error("Error initialising calling v3", e)
  }

  //TODO Dean - check error codes properly
  response {
    case (resp, ctx) => init.foreach { _ => Calling.wcall_resp(resp.fold(_.code, _ => 200), resp.fold(_.message, _ => ""), ctx) }
  }

  missedCall {
    case (convId, userId, time) => messagesService.addMissedCallMessage(convId, userId, time)
  }

  def startCall(convId: ConvId, isVideo: Boolean = false): Unit = withConvWhenReady(convId) { conv =>
    membersStorage.getByConv(conv.id).map { members =>
      verbose(s"startCall convId: $convId, isVideo: $isVideo")
      if (members.size == 2) {
        members.map(_.userId).find(_ != selfUserId).foreach { other =>
          Calling.wcall_start(conv.remoteId.str, isVideo)
          currentCall.mutate {
            case IsIdle() =>
              //Assume that when a video call starts, sendingVideo will be true. From here on, we can then listen to state handler
              CallInfo(Some(conv.id), Set(other), SELF_CALLING, isVideoCall = isVideo, videoSendState = if(isVideo) PREVIEW else DONT_SEND)
            case cur =>
              error("Call already in progress, not updating")
              cur
          }
        }
      } else {
        //TODO handle group conversations
        verbose("Group calls not yet supported in calling v3")
      }
    }
  }

  def endCall(convId: ConvId): Unit = withConvWhenReady(convId) { conv =>
    verbose(s"endCall: $convId")
    Calling.wcall_end(conv.remoteId.str)
    currentCall ! IdleCall
  }

  def acceptCall(convId: ConvId): Unit = withConvWhenReady(convId) { conv =>
    verbose(s"answerCall: $convId")
    Calling.wcall_answer(conv.remoteId.str)
    currentCall.mutate {
      case c if c.state == OTHER_CALLING => c.copy(state = SELF_JOINING)
      case c =>
        error(s"accepted call from unexpected state: ${c.state}")
        c
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
    withConvWhenReady(convId) { conv =>
      Calling.wcall_set_video_send_active(conv.remoteId.str, send)
      currentCall.mutate(_.copy(videoSendState = if (send) SEND else DONT_SEND))
    }
  }

  val callMessagesStage = EventScheduler.Stage[CallMessageEvent] {
    case (_, events) => Future.successful(events.sortBy(_.time).foreach { e =>
      receiveCallEvent(e.content, e.time.instant, e.convId, e.from, e.sender)
    })
  }

  private def receiveCallEvent(content: String, msgTime: Instant, convId: RConvId, from: UserId, sender: ClientId): Unit = {
    val msg = content.getBytes("UTF-8")
    val curTime = Instant.now
    verbose(s"Received msg for avs: curTime: $curTime, msgTime: $msgTime, msg: $content")
    Calling.wcall_recv_msg(msg, msg.length, uint32_tTime(curTime), uint32_tTime(msgTime), convId.str, from.str, sender.str)
  }

  protected def uint32_tTime(instant: Instant) =
    returning(Uint32_t((Instant.now.toEpochMilli / 1000).toInt))(t => verbose(s"uint32_tTime for $instant = ${t.value}"))

  protected def instant(uint32_t: Uint32_t) = Instant.ofEpochMilli(uint32_t.value.toLong * 1000)

  //Ensures that wcall is initialised and then loads the conversation to perform any call actions on.
  private def withConvWhenReady(convId: ConvId)(f: ConversationData => Unit): Unit = init.flatMap(_ => convs.convById(convId).map {
    case Some(conv) => f(conv)
    case _ => error(s"Could not find conversation: $convId")
  }).recover {
    case e: Throwable => error("Attempted to perform avs v3 action after failed init", e)
  }
}


