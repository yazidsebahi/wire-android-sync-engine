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

import java.util.Date

import com.waz.api._
import com.waz.api.impl.ErrorResponse
import com.waz.mocked.{MockedMedia, MockedFlows, MockBackend}
import com.waz.model.VoiceChannelData.ChannelState._
import com.waz.model._
import com.waz.service.PlaybackRoute
import com.waz.service.call.VoiceChannelService.{CallJoined => VCSCallJoined, _}
import com.waz.sync.client.VoiceChannelClient.JoinCallFailed
import com.waz.testutils.HasId.idsOfAll
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils._
import com.waz.testutils.TestApplication._
import com.waz.threading.CancellableFuture
import com.waz.znet.ZNetClient._
import org.robolectric.annotation.Config
import org.scalatest.{OptionValues, BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.collection.{mutable, breakOut}
import scala.concurrent.duration._

@Config(application = classOf[TestApplication])
class VoiceChannelSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with OptionValues with MockBackend with MockedClientApiSpec with MockedFlows with MockedMedia {
  import DefaultPushBehaviour.Implicit

  lazy val selfId = UserId(api.getSelf.getUser.getId)

  lazy val convs = api.getConversations
  lazy val activeChannels = api.getActiveVoiceChannels
  lazy val meep = Set(UserId("meep"))
  lazy val foo = Set(UserId("foo"))
  lazy val meepAndFoo = Set(UserId("meep"), UserId("foo"))
  lazy val groupId = RConvId("group")
  lazy val onlyMeep = Set(UserId("meep"))
  lazy val onlyFoo = Set(UserId("foo"))

  case class ConvData(id: String, event: UserConnectionEvent, conv: IConversation, voice: VoiceChannel)

  val conv: mutable.Map[String, ConvData] = mutable.Map.empty

  var callStateError = Option.empty[ErrorResponse]
  var callJoinFailed = Option.empty[JoinCallFailed]

  val spy = new CallJoinSpy

  override def updateSelfCallState(id: RConvId, deviceState: CallDeviceState, cause: CauseForCallStateEvent): ErrorOrResponse[Either[JoinCallFailed, CallStateEvent]] =
    callStateError.fold(callJoinFailed.fold(super.updateSelfCallState(id, deviceState, cause))(f => CancellableFuture.successful(Right(Left(f)))))(e => CancellableFuture.successful(Left(e)))

  override protected def beforeAll(): Unit = {
    val meep = addConnection(UserId("meep"))
    val foo = addConnection(UserId("foo"))

    addGroupConversation(Seq(UserId("meep"), UserId("foo")), id = groupId, name = Some("group convo"))

    super.beforeAll()

    awaitUi(api.getSelf.isLoggedIn)
    awaitUi(convs.size == 3)(10.seconds)
    awaitUi(5.seconds)

    conv ++= Seq(("meep", meep), ("foo", foo)) map {
      case (name, event) =>
        val c = convs.getConversation(name)
        name -> ConvData(name, event, c, c.getVoiceChannel)
    }

    spy.reset()
  }

  feature("Receiving calls") {
    scenario("Receiving a call should update the respective conversation.") {
      val c = conv("meep")

      withDelay {
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.getState shouldEqual Idle
        c.voice.getCaller shouldBe null
        c.voice.getKindOfCall shouldEqual KindOfCall.UNKNOWN
        c.voice.getParticipants shouldBe empty
        c.voice.getCallSessionId shouldBe empty
        callingEventsSpy.latestEvent shouldEqual None
      }

      c.voice.addUpdateListener(new UpdateListener {
        def updated(): Unit = info(s"updated (meep): ${c.voice.getState}, devActive = ${c.voice.isActiveDevice}, silenced = ${c.voice.isSilenced}, sessionId = ${c.voice.getCallSessionId}")
      })

      incomingCall(c, "meep-1")

      withDelay {
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.getState shouldEqual OtherCalling
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        c.voice.getCallSessionId shouldEqual "meep-1-session-id"
        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual c.voice
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Receiving a call should not update any other conversations.") {
      val c = conv("foo")

      withDelay {
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.getState shouldEqual Idle
        c.voice.getCaller shouldBe null
        c.voice.getKindOfCall shouldEqual KindOfCall.UNKNOWN
        c.voice.getParticipants shouldBe empty
        c.voice.getCallSessionId shouldBe empty
      }
    }

    scenario("Silencing an incoming call should update the conversation correctly.") {
      val c = conv("meep")
      c.voice.silence()

      withDelay {
        c.voice.isSilenced shouldEqual true
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.conv.hasVoiceChannel shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        c.voice.getCallSessionId shouldEqual "meep-1-session-id"
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Second incoming call after silenced one should replace the silenced call.") {
      val meep = conv("meep")
      val foo = conv("foo")

      incomingCall(foo, "foo-1")

      withDelay {
        meep.conv.hasVoiceChannel shouldEqual false
        meep.conv.hasUnjoinedCall shouldEqual true
        idsOfAll(meep.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        meep.voice.getCallSessionId shouldEqual "meep-1-session-id"
        meep.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE

        foo.conv.hasVoiceChannel shouldEqual false
        foo.conv.hasUnjoinedCall shouldEqual true
        foo.voice.isSilenced shouldEqual false
        idsOfAll(foo.voice.getParticipants: _*) should contain theSameElementsAs onlyFoo
        foo.voice.getCallSessionId shouldEqual "foo-1-session-id"
        foo.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual foo.voice
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Cancelling an incoming, silenced call should update the conversation correctly.") {
      val foo = conv("foo")
      val meep = conv("meep")

      cancelCall(foo)
      cancelCall(meep)

      withDelay {
        idsOfAll(meep.voice.getParticipants: _*) shouldBe empty
        meep.voice.getCallSessionId shouldEqual "meep-1-session-id"
        meep.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(meep.voice.getCaller) shouldEqual None

        foo.conv.hasVoiceChannel shouldEqual false
        foo.conv.hasUnjoinedCall shouldEqual false
        foo.voice.getState shouldEqual Idle
        Option(foo.voice.getCaller) shouldEqual None
        foo.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        foo.voice.isSilenced shouldEqual false
        idsOfAll(foo.voice.getParticipants: _*) shouldBe empty
        foo.voice.getCallSessionId shouldEqual "foo-1-session-id"

        activeChannels.hasIncomingCall shouldEqual false
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Receiving another call will reset the silenced flag.") {
      val c = conv("meep")

      incomingCall(c, "meep-2")

      withDelay {
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.getState shouldEqual OtherCalling
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.voice.isSilenced shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        c.voice.getCallSessionId shouldEqual "meep-2-session-id"

        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual c.voice
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Receiving yet another call will queue it up (ui will not know about that).") {
      val meep = conv("meep")
      val foo = conv("foo")

      incomingCall(foo, "foo-2")

      withDelay {
        meep.conv.hasVoiceChannel shouldEqual false
        meep.conv.hasUnjoinedCall shouldEqual true
        meep.voice.getState shouldEqual OtherCalling
        meep.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        idsOfAll(meep.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        meep.voice.getCallSessionId shouldEqual "meep-2-session-id"
        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual meep.voice

        foo.conv.hasVoiceChannel shouldEqual false
        foo.conv.hasUnjoinedCall shouldEqual true
        foo.voice.getState shouldEqual OtherCalling
        foo.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        idsOfAll(foo.voice.getParticipants: _*) should contain theSameElementsAs onlyFoo
        foo.voice.getCallSessionId shouldEqual "foo-2-session-id"
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Cancelling the first call will move the second one to the front of the queue.") {
      val meep = conv("meep")
      val foo = conv("foo")

      cancelCall(meep)

      withDelay {
        meep.conv.hasVoiceChannel shouldEqual false
        meep.conv.hasUnjoinedCall shouldEqual false
        meep.voice.getState shouldEqual Idle
        meep.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        meep.voice.isSilenced shouldEqual false
        idsOfAll(meep.voice.getParticipants: _*) shouldBe empty
        meep.voice.getCallSessionId shouldEqual "meep-2-session-id"
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual foo.voice
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Cancelling the second call will reset everything to idle.") {
      val foo = conv("foo")

      cancelCall(foo)
      withDelay {
        foo.conv.hasVoiceChannel shouldEqual false
        foo.conv.hasUnjoinedCall shouldEqual false
        foo.voice.isSilenced shouldEqual false
        foo.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        foo.voice.getCallSessionId shouldEqual "foo-2-session-id"
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }
  }

  feature("Call notifications") {
    lazy val meep = conv("meep")

    scenario("Receive a 1-to-1 call while app is backgrounded") {
      api.onPause()
      withDelay {
        zmessaging.websocket.connected.currentValue shouldEqual Some(false)
      }

      callParticipants += meep.event.convId -> participants(selfId -> false, UserId(meep.id) -> true)
      incomingCall(meep, "meep-42")

      withDelay {
        notificationsSpy.incomingCall.value.getConversationId shouldEqual "meep"
        notificationsSpy.ongoingCall shouldEqual None
        notificationsSpy.uiActive shouldEqual false
      }
    }

    scenario("Join the incoming 1-to-1 call and put app to foreground") {
      spy.reset()
      callParticipants += meep.event.convId -> participants(selfId -> true, UserId(meep.id) -> true)
      meep.voice.join(spy.joinCallback)

      withDelay {
        notificationsSpy.incomingCall shouldBe None
        notificationsSpy.ongoingCall.value.getConversationId shouldEqual "meep"
        notificationsSpy.uiActive shouldEqual false
        spy.callJoinResult shouldEqual None
      }

      api.onResume()
      awaitUi(3.seconds)
      withDelay {
        spy.callJoinResult.value shouldEqual VCSCallJoined
      }
    }

    scenario("End the 1-to-1 call") {
      callParticipants.clear()
      cancelCall(meep)

      withDelay {
        notificationsSpy.incomingCall shouldBe None
        notificationsSpy.ongoingCall shouldBe None
        notificationsSpy.uiActive shouldEqual true
      }
    }
  }

  feature("Taking calls") {
    var callEnd = 0L
    var callStart = 0L
    lazy val c = conv("meep")

    scenario("Join an incoming call") {
      callParticipants.clear()
      spy.reset()

      incomingCall(c, "meep-3")
      withDelay {
        c.voice.getState shouldEqual OtherCalling
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs onlyMeep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        tracking(c.voice).initiated should be ('defined)
        tracking(c.voice).joined shouldEqual None
        tracking(c.voice).established shouldEqual None
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }

      callParticipants += c.event.convId -> participants(selfId -> true, UserId(c.id) -> true)
      c.voice.join(spy.joinCallback)

      withDelay {
        c.voice.getState shouldEqual DeviceJoining
        c.conv.hasUnjoinedCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        tracking(c.voice).initiated should be ('defined)
        tracking(c.voice).joined should be ('defined)
        tracking(c.voice).established shouldEqual None
        spy.callJoinResult.value shouldEqual VCSCallJoined
      }

      awaitUi(100.millis)
      establishMedia(c.event.convId)
      callStart = System.currentTimeMillis

      withDelay {
        c.voice.isActiveDevice shouldEqual true
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.getState shouldEqual DeviceConnected
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.voice.isSilenced shouldEqual false
        c.voice.isMuted shouldEqual false
        c.voice.isDropped shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.getOngoingCall shouldEqual c.voice
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs meep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        sessionIdUsedToAcquireFlows shouldEqual Some(CallSessionId("meep-3-session-id"))
        c.voice.getMillisDurationOfMostRecentCall shouldEqual Duration.Zero.toMillis
        c.voice.getCallStart.getTime shouldEqual (callStart +- 25L)
        tracking(c.voice).initiated should be ('defined)
        tracking(c.voice).joined should be ('defined)
        tracking(c.voice).established should be ('defined)
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, 2, d) if !d.isZero => () }
      }
    }

    scenario("Mute it") {
      c.voice.mute()

      withDelay {
        c.voice.isMuted shouldEqual true
        c.voice.isSilenced shouldEqual false
        c.voice.getState shouldEqual DeviceConnected
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.isDropped shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs meep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, 2, d) if !d.isZero => () }
      }
    }

    scenario("Unmute it") {
      c.voice.unmute()

      withDelay {
        c.voice.isMuted shouldEqual false
        c.voice.isSilenced shouldEqual false
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs meep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, 2, d) if !d.isZero => () }
      }
    }

    scenario("Switch away from speakers") {
      c.voice.setSpeaker(false)
      withDelay { c.voice.isSpeaker shouldEqual false }
    }

    scenario("Speaker gets reactivated by AVS") {
      changePlaybackRoute(PlaybackRoute.Speaker)
      withDelay { c.voice.isSpeaker shouldEqual true }
    }

    scenario("Speaker gets deactivated again by AVS") {
      changePlaybackRoute(PlaybackRoute.Earpiece)
      withDelay { c.voice.isSpeaker shouldEqual false }
    }

    scenario("Switch back to speakers manually") {
      c.voice.setSpeaker(true)
      withDelay { c.voice.isSpeaker shouldEqual true }
    }

    scenario("Transfer it to another device") {
      awaitUi(1.second)
      transferAway(c)
      callEnd = System.currentTimeMillis

      withDelay {
        c.voice.canTransferToThisDevice shouldEqual true
        c.voice.getState shouldEqual UserConnected
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.voice.isActiveDevice shouldEqual false
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.isDropped shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        c.voice.getCallStart.getTime shouldEqual (callStart +- 25L)
        c.voice.getMillisDurationOfMostRecentCall shouldEqual (callEnd - callStart +- 25L)
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs meep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallTransferred(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, CauseForCallEnd.TRANSFERRED, 2, d) if !d.isZero => () }
      }
    }

    scenario("Transfer it back to this device") {
      transferHere(c)
      withDelay {
        c.voice.getState shouldEqual DeviceJoining
      }

      establishMedia(c.event.convId)
      callStart = System.currentTimeMillis

      withDelay {
        c.voice.canTransferToThisDevice shouldEqual false
        c.voice.getState shouldEqual DeviceConnected
        Option(c.voice.getCaller).value.getId shouldEqual "meep"
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.voice.isActiveDevice shouldEqual true
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.isDropped shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        c.voice.getMillisDurationOfMostRecentCall shouldEqual Duration.Zero.toMillis
        c.voice.getCallStart.getTime shouldEqual (callStart +- 25L)
        idsOfAll(c.voice.getParticipants: _*) should contain theSameElementsAs meep
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, 2, d) if !d.isZero => () }
      }
    }

    scenario("Other hangs up") {
      cancelCall(c)
      callEnd = System.currentTimeMillis

      withDelay {
        c.voice.canTransferToThisDevice shouldEqual false
        c.voice.getState shouldEqual Idle
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.voice.isActiveDevice shouldEqual false
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.isDropped shouldEqual false
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        c.voice.getMillisDurationOfMostRecentCall shouldEqual (callEnd - callStart +- 25L)
        c.voice.getCallStart.getTime shouldEqual (callStart +- 25L)
        idsOfAll(c.voice.getParticipants: _*) shouldBe empty
        c.voice.getCallSessionId shouldEqual "meep-3-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallEndedNormally(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, CauseForCallEnd.OTHER, 2, _) => () }
      }
    }
  }

  feature("Initiating calls") {
    scenario("Call other") {
      val c = conv("meep")
      spy.reset()

      callParticipants += c.event.convId -> participants(selfId -> true, UserId(c.id) -> false)
      callSessionId += c.event.convId -> CallSessionId(s"meep-4-session-id")
      c.voice.join(spy.joinCallback)

      withDelay {
        c.voice.getState shouldEqual DeviceCalling
        Option(c.voice.getCaller).value.getId shouldEqual selfId.str
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.isActiveDevice shouldEqual true
        c.voice.isSilenced shouldEqual false
        c.voice.canTransferToThisDevice shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.getOngoingCall shouldEqual c.voice
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should be(empty) // doesn't include self
        c.voice.getCallSessionId shouldEqual "meep-4-session-id"
        tracking(c.voice).initiated should be ('defined)
        tracking(c.voice).joined shouldEqual None
        tracking(c.voice).established shouldEqual None
        callingEventsSpy.latestEvent.value shouldEqual OutgoingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
        spy.callJoinResult.value shouldEqual VCSCallJoined
      }
    }

    scenario("Transfer to other device") {
      val c = conv("meep")

      transferAway(c, otherJoined = false)

      withDelay {
        c.voice.getState shouldEqual UserCalling
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(c.voice.getCaller).value.getId shouldEqual selfId.str
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.isActiveDevice shouldEqual false
        c.voice.isSilenced shouldEqual false
        c.voice.canTransferToThisDevice shouldEqual true
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should be(empty)
        c.voice.getCallSessionId shouldEqual "meep-4-session-id"
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.OUTGOING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Transfer back") {
      val c = conv("meep")

      transferHere(c, otherJoined = false)

      withDelay {
        c.voice.getState shouldEqual DeviceCalling
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(c.voice.getCaller).value.getId shouldEqual selfId.str
        c.conv.hasVoiceChannel shouldEqual true
        c.conv.hasUnjoinedCall shouldEqual true
        c.voice.isActiveDevice shouldEqual true
        c.voice.isSilenced shouldEqual false
        c.voice.canTransferToThisDevice shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) should be(empty)
        c.voice.getCallSessionId shouldEqual "meep-4-session-id"
        callingEventsSpy.latestEvent.value shouldEqual OutgoingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Hang up") {
      val c = conv("meep")

      callParticipants += c.event.convId -> participants(selfId -> false, UserId(c.id) -> false)
      c.voice.leave()

      withDelay {
        c.voice.getState shouldEqual Idle
        c.voice.getKindOfCall shouldEqual KindOfCall.ONE_TO_ONE
        Option(c.voice.getCaller) shouldEqual None
        c.conv.hasVoiceChannel shouldEqual false
        c.conv.hasUnjoinedCall shouldEqual false
        c.voice.isActiveDevice shouldEqual false
        c.voice.isSilenced shouldEqual false
        c.voice.canTransferToThisDevice shouldEqual false
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        idsOfAll(c.voice.getParticipants: _*) shouldBe empty
        c.voice.getCallSessionId shouldEqual "meep-4-session-id"
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.OUTGOING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }
  }

  feature("Group calls") {
    lazy val group = convs.asScala.find(_.getType == IConversation.Type.GROUP).value
    lazy val groupVoice = group.getVoiceChannel

    scenario("Before call") {
      group.getName shouldEqual "group convo"
      group.hasVoiceChannel shouldEqual false
      group.hasUnjoinedCall shouldEqual false
      groupVoice.getState shouldEqual Idle
      Option(groupVoice.getCaller) shouldEqual None
      activeChannels.hasOngoingCall shouldEqual false
      activeChannels.hasIncomingCall shouldEqual false
    }

    scenario("Initiate group call") {
      spy.reset()
      callParticipants += groupId -> groupParticipants(self = true, meep = false, foo = false)
      callSessionId += groupId -> CallSessionId("group-1-session-id")
      groupVoice.join(spy.joinCallback)

      withDelay {
        group.hasVoiceChannel shouldEqual true
        group.hasUnjoinedCall shouldEqual true
        groupVoice.getState shouldEqual DeviceCalling
        Option(groupVoice.getCaller).value.getId shouldEqual selfId.str
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        idsOfAll(groupVoice.getParticipants: _*) should be(empty)
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        callingEventsSpy.latestEvent.value shouldEqual OutgoingRingingStarted(KindOfCall.GROUP, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined shouldEqual None
        tracking(groupVoice).established shouldEqual None
        spy.callJoinResult.value shouldEqual VCSCallJoined
      }
    }

    scenario("First participant joins") {
      zmessaging.network.networkMode ! NetworkMode._4G
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = true, meep = true, foo = false)), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        group.hasVoiceChannel shouldEqual true
        group.hasUnjoinedCall shouldEqual true
        groupVoice.getState shouldEqual DeviceJoining
        Option(groupVoice.getCaller).value.getId shouldEqual selfId.str
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meep
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined should be ('defined)
        tracking(groupVoice).established shouldEqual None
        callingEventsSpy.events.tail.head shouldEqual RingingEnded(KindOfCall.GROUP, CallDirection.OUTGOING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode._4G)
        callingEventsSpy.latestEvent.value should beMatching { case CallJoined(KindOfCall.GROUP, CallDirection.OUTGOING, false, true, NetworkMode._4G, 2, 3, d) if !d.isZero => () }
      }

      zmessaging.network.networkMode ! NetworkMode.WIFI
      establishMedia(groupId)

      withDelay {
        groupVoice.getState shouldEqual DeviceConnected
        group.hasUnjoinedCall shouldEqual false
        Option(groupVoice.getCaller).value.getId shouldEqual selfId.str
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meep
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined should be ('defined)
        tracking(groupVoice).established should be ('defined)
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.GROUP, CallDirection.OUTGOING, false, true, NetworkMode.WIFI, 2, 3, d) if !d.isZero => () }
      }
    }

    scenario("Second participant joins") {
      // "foo" joins
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = true, meep = true, foo = true)), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual DeviceConnected
        group.hasUnjoinedCall shouldEqual false
        Option(groupVoice.getCaller).value.getId shouldEqual selfId.str
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined should be ('defined)
        tracking(groupVoice).established should be ('defined)
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.GROUP, CallDirection.OUTGOING, false, true, NetworkMode.WIFI, 2, 3, d) if !d.isZero => () }
      }
    }

    scenario("Leave the call") {
      // leave
      callParticipants += groupId -> groupParticipants(self = false, meep = true, foo = true)
      groupVoice.leave()

      withDelay {
        groupVoice.getState shouldEqual OthersConnected
        group.hasUnjoinedCall shouldEqual true
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller).value.getId shouldEqual selfId.str
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        groupVoice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value should beMatching { case CallEndedNormally(KindOfCall.GROUP, CallDirection.OUTGOING, false, true, NetworkMode.WIFI, 3, CauseForCallEnd.SELF, 3, _) => () }
      }
    }

    scenario("Another participant leaves, call ends") {
      // "meep" leaves (and "foo" leaves by force)
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = false, meep = false, foo = false)), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual Idle
        group.hasUnjoinedCall shouldEqual false
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller) shouldEqual None
        idsOfAll(groupVoice.getParticipants: _*) shouldBe empty
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-1-session-id"
        groupVoice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value should beMatching { case CallEndedNormally(KindOfCall.GROUP, CallDirection.OUTGOING, false, true, NetworkMode.WIFI, 3, CauseForCallEnd.SELF, 3, _) => () }
      }
    }

    scenario("Meep initiates the call") {
      callSessionId += groupId -> CallSessionId("group-2-session-id")

      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = false, meep = true, foo = false)), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual OtherCalling
        group.hasUnjoinedCall shouldEqual true
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs Set(UserId("meep"))
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual true
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        groupVoice.isSilenced shouldEqual false
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined shouldEqual None
        tracking(groupVoice).established shouldEqual None
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.GROUP, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Foo joins") {
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = false, meep = true, foo = true)), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual OthersConnected
        group.hasUnjoinedCall shouldEqual true
        Option(groupVoice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual true
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        groupVoice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.GROUP, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Self joins") {
      spy.reset()
      callParticipants += groupId -> groupParticipants(self = true, meep = true, foo = true)
      groupVoice.join(spy.joinCallback)

      withDelay {
        groupVoice.getState shouldEqual DeviceJoining
        group.hasUnjoinedCall shouldEqual true
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        groupVoice.isSilenced shouldEqual false
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined should be ('defined)
        tracking(groupVoice).established shouldEqual None
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.GROUP, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
        spy.callJoinResult.value shouldEqual VCSCallJoined
      }

      establishMedia(groupId)

      withDelay {
        groupVoice.getState shouldEqual DeviceConnected
        group.hasUnjoinedCall shouldEqual false
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.GROUP, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 3, 3, d) if !d.isZero => () }
      }
    }

    scenario("Meep leaves") {
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = true, meep = false, foo = true)), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual DeviceConnected
        group.hasUnjoinedCall shouldEqual false
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller).value.getId shouldEqual "meep"
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs foo
        activeChannels.hasOngoingCall shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        groupVoice.isSilenced shouldEqual false
        tracking(groupVoice).initiated should be ('defined)
        tracking(groupVoice).joined should be ('defined)
        tracking(groupVoice).established should be ('defined)
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.GROUP, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 3, 3, d) if !d.isZero => () }
      }
    }

    scenario("Foo leaves, thus ending the call") {
      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = false, meep = false, foo = false)), None, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))

      withDelay {
        groupVoice.getState shouldEqual Idle
        group.hasUnjoinedCall shouldEqual false
        groupVoice.getKindOfCall shouldEqual KindOfCall.GROUP
        Option(groupVoice.getCaller) shouldEqual None
        idsOfAll(groupVoice.getParticipants: _*) shouldBe empty
        activeChannels.hasOngoingCall shouldEqual false
        activeChannels.hasIncomingCall shouldEqual false
        groupVoice.getCallSessionId shouldEqual "group-2-session-id"
        groupVoice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value should beMatching { case CallEndedNormally(KindOfCall.GROUP, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 3, CauseForCallEnd.OTHER, 3, _) => () }
      }
    }
  }

  feature("Group call errors") {
    lazy val group = convs.asScala.find(_.getType == IConversation.Type.GROUP).value
    lazy val groupVoice = group.getVoiceChannel

    scenario("Too many conversation members") {
      spy.reset()

      callStateError = None
      callJoinFailed = Some(JoinCallFailed("conv-too-big", None, Some(30), Some(25)))
      groupVoice.join(spy.joinCallback)

      withDelay {
        spy.callJoinResult.value shouldEqual ConversationTooBig(30, 25)
      }
    }

    scenario("Group call full") {
      spy.reset()

      callStateError = None
      callJoinFailed = Some(JoinCallFailed("voice-channel-full", Some(9), None, None))
      groupVoice.join(spy.joinCallback)

      withDelay {
        spy.callJoinResult.value shouldEqual VoiceChannelFull(9)
      }
    }
  }

  feature("Group calls - leaving conversations") {
    lazy val group = convs.asScala.find(_.getType == IConversation.Type.GROUP).value
    lazy val groupVoice = group.getVoiceChannel

    scenario("Leaving a group conversation while a call is ongoing will also leave the call") {
      callStateError = None
      callJoinFailed = None
      spy.reset()

      callParticipants += groupId -> groupParticipants(self = true, meep = false, foo = false)
      callSessionId += groupId -> CallSessionId("group-3-session-id")
      setupGroupCall()

      callParticipants += groupId -> groupParticipants(self = false, meep = false, foo = false)
      group.leave()
      withDelay {
        group.hasVoiceChannel shouldEqual false
        groupVoice.getState shouldEqual Idle
      }
    }

    scenario("Getting removed from a group conversation while a call is ongoing will also leave the call") {
      addMembersToGroupConversation(groupId, Seq(selfId), from = UserId("meep"))

      callSessionId += groupId -> CallSessionId("group-4-session-id")
      setupGroupCall()

      addNotification(MemberLeaveEvent(Uid(), groupId, EventId(300), new Date, UserId("meep"), Seq(selfId)))
      withDelay {
        group.hasVoiceChannel shouldEqual false
        groupVoice.getState shouldEqual Idle
      }
    }

    def setupGroupCall(): Unit = {
      groupVoice.join(spy.joinCallback)
      withDelay { group.hasVoiceChannel shouldEqual true }

      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = true, meep = true, foo = false)), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))
      withDelay { idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meep }

      establishMedia(groupId)
      withDelay { groupVoice.getState shouldEqual DeviceConnected }

      addNotification(CallStateEvent(Uid(), groupId, Some(groupParticipants(self = true, meep = true, foo = true)), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(groupId))))
      withDelay {
        idsOfAll(groupVoice.getParticipants: _*) should contain theSameElementsAs meepAndFoo
        groupVoice.getState shouldEqual DeviceConnected
      }
    }
  }

  feature("Multiple simultaneous calls") {
    lazy val foo = conv("foo")
    lazy val meep = conv("meep")

    scenario("Establish the first call") {
      callStateError = None
      callJoinFailed = None
      callParticipants.clear()
      spy.reset()

      incomingCall(meep, "ongoing-1")
      withDelay { meep.voice.getState shouldEqual OtherCalling }

      callParticipants += meep.event.convId -> participants(selfId -> true, UserId(meep.id) -> true)
      meep.voice.join(spy.joinCallback)
      withDelay { meep.voice.getState shouldEqual DeviceJoining }
      establishMedia(meep.event.convId)

      withDelay {
        meep.voice.getState shouldEqual DeviceConnected
        activeChannels.hasIncomingCall shouldEqual false
        activeChannels.hasOngoingCall shouldEqual true
        callingEventsSpy.latestEvent.value should beMatching { case CallEstablished(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, false, true, NetworkMode.WIFI, 2, 2, d) if !d.isZero => () }
      }
    }

    scenario("Receive incoming call while another call is ongoing") {
      incomingCall(foo, "incoming-1")
      withDelay {
        foo.voice.getState shouldEqual OtherCalling
        activeChannels.hasIncomingCall shouldEqual true
        activeChannels.getIncomingCall shouldEqual foo.voice

        notificationsSpy.ongoingCall.value.getConversationId shouldEqual "meep"
        notificationsSpy.ongoingCall.value.getState shouldEqual DeviceConnected

        notificationsSpy.incomingCall.value.getConversationId shouldEqual "foo"
        notificationsSpy.incomingCall.value.getState shouldEqual OtherCalling

        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = true, isConversationMuted = false)
      }
    }

    scenario("Silence incoming call") {
      foo.voice.silence()

      withDelay {
        foo.voice.getState shouldEqual OtherCalling
        foo.voice.isSilenced shouldEqual true
        activeChannels.hasIncomingCall shouldEqual false
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Other side hangs up incoming call") {
      cancelCall(foo)
      withDelay {
        foo.voice.getState shouldEqual Idle
        foo.voice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Leave ongoing call") {
      callParticipants.clear()
      meep.voice.leave()
      withDelay { meep.voice.getState shouldEqual Idle }
    }
  }

  feature("Ringtones") {
    lazy val meep = conv("meep")

    scenario("Normal ringtone is played for incoming call") {
      incomingCall(meep, "incoming-2")
      withDelay {
        meep.voice.getState shouldEqual OtherCalling
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = false)
      }
    }

    scenario("Ringtone stops when call is muted") {
      meep.voice.silence()
      withDelay {
        meep.voice.getState shouldEqual OtherCalling
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("Hang up") {
      cancelCall(meep)
      withDelay {
        meep.voice.getState shouldEqual Idle
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }

    scenario("No ringtone is played for incoming calls in muted conversations") {
      meep.conv.setMuted(true)
      withDelay { meep.conv.isMuted shouldEqual true }

      incomingCall(meep, "incoming-3")
      withDelay {
        meep.voice.getState shouldEqual OtherCalling
        meep.voice.isSilenced shouldEqual false
        callingEventsSpy.latestEvent.value shouldEqual IncomingRingingStarted(KindOfCall.ONE_TO_ONE, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI, inOngoingCall = false, isConversationMuted = true)
      }
    }

    scenario("Hang up again") {
      cancelCall(meep)
      withDelay {
        meep.voice.getState shouldEqual Idle
        callingEventsSpy.latestEvent.value shouldEqual RingingEnded(KindOfCall.ONE_TO_ONE, CallDirection.INCOMING, isVideoCall = false, isUiActive = true, networkMode = NetworkMode.WIFI)
      }
    }
  }

  def participants(ps: (UserId, Boolean)*): Set[CallParticipant] = ps.map { case (id, joined) => CallParticipant(id, joined = joined, props = Set.empty) }(breakOut)

  def deviceState(joined: Boolean) = Some(CallDeviceState(joined = joined, props = Set.empty))

  def groupParticipants(self: Boolean, meep: Boolean, foo: Boolean) = Set(
    CallParticipant(selfId, joined = self, props = Set.empty),
    CallParticipant(UserId("meep"), joined = meep, props = Set.empty),
    CallParticipant(UserId("foo"), joined = foo, props = Set.empty))

  def incomingCall(conv: ConvData, tag: String = "default"): Unit = {
    callSessionId += conv.event.convId -> CallSessionId(s"$tag-session-id")
    callStateEvent(conv, otherJoined = true)
  }

  def cancelCall(conv: ConvData): Unit = callStateEvent(conv)

  def joined(conv: ConvData): Unit = callStateEvent(conv, selfJoined = true, otherJoined = true, device = deviceState(joined = true))

  def transferHere(conv: ConvData, otherJoined: Boolean = true): Unit = transfer(conv, otherJoined, deviceJoined = true)

  def transferAway(conv: ConvData, otherJoined: Boolean = true): Unit = transfer(conv, otherJoined, deviceJoined = false)

  def transfer(conv: ConvData, otherJoined: Boolean, deviceJoined: Boolean): Unit = callStateEvent(conv, selfJoined = true,
    otherJoined = otherJoined, device = Some(CallDeviceState(joined = deviceJoined, props = Set.empty)))

  def callStateEvent(conv: ConvData, selfJoined: Boolean = false, otherJoined: Boolean = false, device: Option[CallDeviceState] = None) =
    addNotification(CallStateEvent(
      Uid(), conv.event.convId,
      Some(Set(CallParticipant(selfId, joined = selfJoined, props = Set.empty), CallParticipant(UserId(conv.id), joined = otherJoined, props = Set.empty))),
      device, cause = CauseForCallStateEvent.REQUESTED, Some(callSessionId(conv.event.convId))))

  def tracking(c: VoiceChannel): CallTrackingData = c.asInstanceOf[impl.VoiceChannel].data.tracking
}
