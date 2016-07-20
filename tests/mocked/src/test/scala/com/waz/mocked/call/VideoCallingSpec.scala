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
package com.waz.mocked.call

import com.waz.api._
import com.waz.mocked.{MockBackend, MockedFlows}
import com.waz.model.VoiceChannelData.ChannelState._
import com.waz.model._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils.TestApplication._
import com.waz.testutils.{CallJoinSpy, TestApplication}
import org.robolectric.annotation.Config
import org.scalatest.{FeatureSpec, OptionValues}

import scala.collection.breakOut
import scala.concurrent.duration._

@Config(application = classOf[TestApplication])
class VideoCallingSpec extends FeatureSpec with OptionValues with MockBackend with MockedClientApiSpec with MockedFlows {
  import DefaultPushBehaviour.Implicit

  lazy val selfId = UserId(api.getSelf.getUser.getId)
  lazy val Seq(other, yetAnother) = Seq("playa2", "insert-coin") map UserId

  lazy val convs = api.getConversations
  lazy val errors = api.getErrors

  lazy val Seq(conv, conv2) = Seq(other, yetAnother) map (id => convs.find(_.getId == id.str).value)
  lazy val groupConv = convs.find(_.getType == IConversation.Type.GROUP).value

  lazy val Seq(voice, voice2, group) = Seq(conv, conv2, groupConv) map (_.getVoiceChannel)
  lazy val Seq(remoteId, remoteId2, groupRemoteId) = Seq(conv, conv2, groupConv) map (_.data.remoteId)

  lazy val avs = api.getAvs

  val spy = new CallJoinSpy

  override protected def beforeAll(): Unit = {
    addConnection(other)
    addConnection(yetAnother)

    super.beforeAll()

    awaitUi(api.getSelf.isLoggedIn)

    addGroupConversation(Seq(selfId, other, yetAnother))

    awaitUi(convs.size == 3)(10.seconds)
    awaitUi(3.seconds)
  }

  feature("Simple video call flow") {
    scenario("Create outgoing call") {
      spy.reset()
      callingEventsSpy.latestEvent shouldEqual None

      callParticipants += remoteId -> participants(selfId -> (true, true), other -> (false, false))
      callSessionId += remoteId -> CallSessionId(s"session-id-1")
      voice.joinWithVideo(spy.joinCallback)
      voice.setVideoSendState(VideoSendState.PREVIEW)

      withDelay {
        voice.getState shouldEqual DeviceCalling
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.PREVIEW
        voice.isMuted shouldEqual false
        voice.getVideoCaptureDevices should have size 2
        hasConvVideoSendState(remoteId, VideoSendState.PREVIEW) shouldEqual true
        callingEventsSpy.latestEvent.value shouldEqual OutgoingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = true, isUiActive = true, networkMode = NetworkMode.WIFI, false)
      }
    }

    scenario("Background the app while ringing, don't show preview anymore") {
      api.onPause()

      withDelay {
        voice.isVideoCall shouldEqual true
        hasConvVideoSendState(remoteId, VideoSendState.DONT_SEND) shouldEqual true
      }
    }

    scenario("Foreground the app again while ringing, show preview again") {
      api.onResume()

      withDelay {
        voice.isVideoCall shouldEqual true
        hasConvVideoSendState(remoteId, VideoSendState.PREVIEW) shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.PREVIEW
      }
    }

    scenario("Mute the outgoing call while it's ringing") {
      voice.mute()
      withDelay {
        voice.getState shouldEqual DeviceCalling
        voice.isVideoCall shouldEqual true
        voice.isMuted shouldEqual true
      }
    }

    scenario("Other joins (with video)") {
      addNotification(CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (true, true), other -> (true, true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId))))
      withDelay {
        voice.getState shouldEqual DeviceJoining
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.PREVIEW
        voice.isMuted shouldEqual true
        otherParticipant.value.isSendingVideo shouldEqual true
        callingEventsSpy.events.take(2) should beMatching { case List(
          CallJoined(KindOfCall.ONE_TO_ONE, CallDirection.OUTGOING, true, true, NetworkMode.WIFI, 2, 2, d, false),
          RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.OUTGOING, true, true, NetworkMode.WIFI, false)
        ) if !d.isZero => () }
      }
    }

    scenario("Establish media") {
      setCanSendVideo(remoteId, canSend = true)
      establishMedia(remoteId)
      withDelay {
        voice.getState shouldEqual DeviceConnected
        hasConvVideoSendState(remoteId, VideoSendState.SEND) shouldEqual true
        voice.isVideoCall shouldEqual true
        voice.canSendVideo shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.SEND
        voice.isMuted shouldEqual true
        voice.getCurrentVideoCaptureDevice shouldEqual voice.getVideoCaptureDevices.get(0)
        otherParticipant.value.isSendingVideo shouldEqual true
        currentVideoCaptureDeviceId(remoteId) shouldEqual Some("front")
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.OUTGOING, true, true, NetworkMode.WIFI, 2, 2, d, false) if !d.isZero => () }
      }
    }

    scenario("Un-mute") {
      voice.unmute()
      withDelay {
        voice.isVideoCall shouldEqual true
        voice.isMuted shouldEqual false
        voice.isSilenced shouldEqual false
      }
    }

    scenario("Disable video") {
      voice.setVideoSendState(VideoSendState.DONT_SEND)
      withDelay {
        hasConvVideoSendState(remoteId, VideoSendState.DONT_SEND) shouldEqual true
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Re-enable video") {
      voice.setVideoSendState(VideoSendState.SEND)
      withDelay {
        hasConvVideoSendState(remoteId, VideoSendState.SEND) shouldEqual true
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.SEND
      }
    }

    scenario("Mute the call") {
      voice.mute()
      withDelay {
        voice.isVideoCall shouldEqual true
        voice.isMuted shouldEqual true
        voice.isSilenced shouldEqual false
      }
    }

    scenario("Un-mute the call again") {
      voice.unmute()
      withDelay {
        voice.isVideoCall shouldEqual true
        voice.isMuted shouldEqual false
        voice.isSilenced shouldEqual false
      }
    }

    scenario("Switch to back cam") {
      voice.setVideoCaptureDevice("back")
      withDelay {
        voice.getCurrentVideoCaptureDevice shouldEqual voice.getVideoCaptureDevices.get(1)
        currentVideoCaptureDeviceId(remoteId) shouldEqual Some("back")
      }
    }

    scenario("Background the app, don't send video anymore") {
      api.onPause()

      withDelay {
        voice.isVideoCall shouldEqual true
        hasConvVideoSendState(remoteId, VideoSendState.DONT_SEND) shouldEqual true
      }
    }

    scenario("Foreground the app, video is sent again") {
      api.onResume()

      withDelay {
        voice.isVideoCall shouldEqual true
        hasConvVideoSendState(remoteId, VideoSendState.SEND) shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.SEND
      }
    }

    scenario("Incoming 2nd video call") {
      callSessionId += remoteId2 -> CallSessionId("session-id-inc-1")
      addNotification(CallStateEvent(Uid(), remoteId2, Some(participants(selfId -> (false, false), yetAnother -> (true, true))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId2))))
      withDelay {
        voice.isVideoCall shouldEqual true
        voice.getState shouldEqual DeviceConnected
        voice.getVideoSendState shouldEqual VideoSendState.SEND
        voice2.isVideoCall shouldEqual true
        voice2.getState shouldEqual OtherCalling
        voice2.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Reject incoming 2nd call") {
      voice2.silence()
      withDelay {
        voice.isVideoCall shouldEqual true
        voice.getState shouldEqual DeviceConnected
        voice.getVideoSendState shouldEqual VideoSendState.SEND
        voice2.isVideoCall shouldEqual true
        voice2.getState shouldEqual OtherCalling
        voice2.getVideoSendState shouldEqual VideoSendState.DONT_SEND
        voice2.isSilenced shouldEqual true
      }
    }

    scenario("Hang up") {
      callParticipants.clear()
      voice.leave()
      withDelay {
        voice.getState shouldEqual Idle
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Start second video call") {
      awaitUi(1.second)
      callParticipants += remoteId -> participants(selfId -> (true, true), other -> (false, false))
      callSessionId += remoteId -> CallSessionId(s"session-id-1a")
      voice.joinWithVideo(spy.joinCallback)
      voice.setVideoSendState(VideoSendState.PREVIEW)

      withDelay {
        voice.getState shouldEqual DeviceCalling
        voice.getVideoSendState shouldEqual VideoSendState.PREVIEW
        voice.isVideoCall shouldEqual true
      }
    }

    scenario("Other hangs up (again)") {
      callParticipants.clear()

      addNotification(
        CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (false, false), other -> (false, false))), deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId)), sequenceNumber = Some(CallSequenceNumber(300))),
        CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (false, false), other -> (false, false))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId)), sequenceNumber = Some(CallSequenceNumber(300))))

      withDelay {
        voice.getState shouldEqual Idle
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Start audio call after the video calls") {
      awaitUi(1.second)
      callParticipants += remoteId -> participants(selfId -> (true, false), other -> (false, false))
      callSessionId += remoteId -> CallSessionId(s"session-id-1b")
      voice.join(spy.joinCallback)

      withDelay {
        voice.getState shouldEqual DeviceCalling
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
        voice.isVideoCall shouldEqual false
      }
    }

    scenario("Hang up audio call") {
      callParticipants.clear()
      voice.leave()
      withDelay {
        voice.getState shouldEqual Idle
        voice.isVideoCall shouldEqual false
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }
  }

  feature("Incoming video call") {
    scenario("Other creates video call") {
      awaitUi(1.second)
      spy.reset()
      resetVideoCalls()
      callSessionId += remoteId -> CallSessionId(s"session-id-2")
      addNotification(CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (false, false), other -> (true, true))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId))))
      withDelay {
        voice.getState shouldEqual OtherCalling
        voice.isVideoCall shouldEqual true
        otherParticipant.value.isSendingVideo shouldEqual true
      }
    }

    scenario("Accept call (without video)") {
      callParticipants += remoteId -> participants(selfId -> (true, false), other -> (true, true))
      voice.join(spy.joinCallback)
      withDelay {
        voice.getState shouldEqual DeviceJoining
        voice.isVideoCall shouldEqual true
        otherParticipant.value.isSendingVideo shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Establish media (but don't send video)") {
      setCanSendVideo(remoteId, canSend = true)
      establishMedia(remoteId)
      withDelay {
        voice.getState shouldEqual DeviceConnected
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
        hasConvVideoSendState(remoteId, VideoSendState.SEND) shouldEqual false
        voice.isVideoCall shouldEqual true
        voice.canSendVideo shouldEqual false
        voice.isMuted shouldEqual false
        voice.getCurrentVideoCaptureDevice shouldEqual voice.getVideoCaptureDevices.get(0)
        otherParticipant.value.isSendingVideo shouldEqual true
      }
    }

    scenario("Other turns off video, call should still remain a video call as it originally started as one") {
      addNotification(CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (true, false), other -> (true, false))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId))))
      withDelay {
        voice.getState shouldEqual DeviceConnected
        voice.isVideoCall shouldEqual true
        otherParticipant.value.isSendingVideo shouldEqual false
      }
    }

    scenario("Other turns on video again") {
      addNotification(CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (true, false), other -> (true, true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId))))
      withDelay {
        voice.getState shouldEqual DeviceConnected
        voice.isVideoCall shouldEqual true
        otherParticipant.value.isSendingVideo shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
      }
    }

    scenario("Other hangs up") {

      addNotification(
        CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (false, false), other -> (false, false))), deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId)), sequenceNumber = Some(CallSequenceNumber(200))),
        CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (false, false), other -> (false, false))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId)), sequenceNumber = Some(CallSequenceNumber(200))))

      withDelay {
        voice.getState shouldEqual Idle
        voice.isVideoCall shouldEqual true
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
        callingEventsSpy.latestEvent.value should beMatching { case CallEndedNormally(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, true, true, NetworkMode.WIFI, 2, CauseForCallEnd.OTHER, 2, _, false) => () }
      }
    }
  }

  feature("Other unable to receive video") {
    scenario("Create outgoing call") {
      spy.reset()
      resetVideoCalls()

      callParticipants += remoteId -> participants(selfId -> (true, true), other -> (false, false))
      callSessionId += remoteId -> CallSessionId(s"session-id-3")
      voice.joinWithVideo(spy.joinCallback)

      withDelay { voice.getState shouldEqual DeviceCalling }
    }

    scenario("Other joins") {
      addNotification(CallStateEvent(Uid(), remoteId, Some(participants(selfId -> (true, true), other -> (true, true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(remoteId))))
      withDelay { voice.getState shouldEqual DeviceJoining }
    }

    scenario("Establish media") {
      setCanSendVideo(remoteId, canSend = false)
      establishMedia(remoteId)

      withDelay {
        voice.getState shouldEqual DeviceConnected
        voice.isVideoCall shouldEqual true
        voice.canSendVideo shouldEqual false
        voice.getVideoSendState shouldEqual VideoSendState.DONT_SEND
        errors should have size 1
        errors.get(0).getType shouldEqual ErrorType.CANNOT_SEND_VIDEO
      }
    }

    scenario("Hang up") {
      callParticipants.clear()
      voice.leave()
      withDelay { voice.getState shouldEqual Idle }
    }
  }

  feature("Incoming 2nd group audio call after video call") {
    scenario("Initiate video call") {
      spy.reset()
      resetVideoCalls()

      callParticipants += remoteId -> participants(selfId -> (true, true), other -> (false, false), yetAnother -> (false, false))
      callSessionId += remoteId -> CallSessionId(s"group-id-1")
      group.joinWithVideo(spy.joinCallback)
      group.setVideoSendState(VideoSendState.PREVIEW)

      withDelay {
        group.getState shouldEqual DeviceCalling
        group.isVideoCall shouldEqual true
      }
    }

    scenario("Hang up again") {
      group.leave()

      withDelay {
        group.getState shouldEqual Idle
        group.isVideoCall shouldEqual true
      }
    }

    scenario("Incoming group call (merged call states)") {
      awaitUi(1.second)

      callSessionId += remoteId -> CallSessionId(s"group-id-2")
      addNotification(
        CallStateEvent(Uid(), groupRemoteId, Some(participants(selfId -> (false, false), other -> (true, false), yetAnother -> (false, false))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupRemoteId))),
        CallStateEvent(Uid(), groupRemoteId, Some(participants(selfId -> (false, false), other -> (true, false), yetAnother -> (true,  false))), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupRemoteId))))

      withDelay {
        group.getState shouldEqual OthersConnected
        group.isVideoCall shouldEqual false
        group.getCaller should not be null
      }
    }
  }

  def otherParticipant: Option[VoiceChannel.Participant] = voice.getParticipants.find(_.getUser.getId == other.str)

  def participants(ps: (UserId, (Boolean, Boolean))*): Set[CallParticipant] = ps.map { case (id, (joined, video)) => CallParticipant(id, joined = joined, props = if (video) Set(CallProperty.SendsVideo) else Set.empty) }(breakOut)
  def deviceState(joined: Boolean) = Some(CallDeviceState(joined = joined, props = Set.empty))
}
