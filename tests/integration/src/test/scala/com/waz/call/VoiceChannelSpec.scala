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
package com.waz.call

import akka.pattern.ask
import com.waz.api._
import com.waz.model.ConversationData.ConversationType
import com.waz.model.VoiceChannelData.ChannelState
import com.waz.model.{RConvId, VoiceChannelDeactivateEvent}
import com.waz.provision.ActorMessage._
import com.waz.service.Timeouts
import com.waz.service.call.VoiceChannelService.CallJoined
import com.waz.testutils.CallJoinSpy
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.utils._
import com.waz.zms.CallService
import org.scalatest.{FeatureSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class VoiceChannelSpec extends FeatureSpec with Matchers with OptionValues with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/three_users_connected.json"

  lazy val convs = api.getConversations
  lazy val user = api.getSelf
  lazy val channels = api.getActiveVoiceChannels

  val spy = new CallJoinSpy

  lazy val auto2 = registerDevice("VoiceChannelSpec_auto2")
  lazy val auto3 = registerDevice("VoiceChannelSpec_auto3")

  var callConnectingTimeout = 1.minute

  override lazy val timeouts: Timeouts = new Timeouts {
    override val calling: Calling = new Calling {
      override def callConnectingTimeout = test.callConnectingTimeout
    }
  }

  def trackStates(channel: VoiceChannel)(body: => Unit): List[ChannelState] = {
    var states = List(channel.getState)
    val listener = new UpdateListener {
      override def updated(): Unit = if (states.head != channel.getState) states ::= channel.getState
    }
    channel.addUpdateListener(listener)
    try body finally channel.removeUpdateListener(listener)
    states.reverse
  }

  feature("VoiceChannel sync") {
    scenario("load initial voice channel") {
      withDelay(convs should not be empty)
      val conv = withDelay {
        returning(convs.find(_.getType == ConversationType.OneToOne)) {
          _ shouldBe 'defined
        }
      }.value

      val channel = conv.getVoiceChannel

      withDelay {
        channel.getState should not be ChannelState.Unknown
        channel.getParticipants shouldBe empty
      }

      info(s"participants: ${channel.getParticipants.toSeq}")
    }

    scenario("sync all") {
      withDelay(convs should have size 3)
      awaitUi(3.seconds)
    }
  }

  feature("Separate process") {
    scenario("sync in other process") {
      auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
      auto3 ? Login(provisionedEmail("auto3"), "auto3_pass") should eventually(be(Successful))
    }
  }

  feature("Group calls") {
    lazy val group = convs.find(_.getType == ConversationType.Group).value
    lazy val channel = group.getVoiceChannel

    scenario("load initial voice channel") {
      withDelay {
        channel.getState shouldBe ChannelState.Idle
        group.hasUnjoinedCall shouldEqual false
        channel.getParticipants shouldBe empty
      }
    }

    scenario("initiate group call") {
      trackStates(channel) {
        channel.join(spy.joinCallback)

        withDelay {
          channel.getState shouldEqual ChannelState.DeviceCalling
          group.hasUnjoinedCall shouldEqual true
          channels.hasOngoingCall shouldEqual true
          spy.callJoinResult.value shouldEqual CallJoined
        }
      } shouldEqual List(ChannelState.Idle, ChannelState.DeviceCalling) // no other states or updates
    }

    scenario("auto2 joins the group call") {
      auto2 ? AcceptCall should eventually(be(Successful))

      trackStates(channel) {
        withDelay {
          channel.getState shouldEqual ChannelState.DeviceConnected
          group.hasUnjoinedCall shouldEqual false
          channels.hasOngoingCall shouldEqual true
          channel.getParticipants should have size 1
        }
      } shouldEqual List(ChannelState.DeviceCalling, ChannelState.DeviceJoining, ChannelState.DeviceConnected)
    }

    scenario("auto3 joins the group call") {
      auto3 ? AcceptCall should eventually(be(Successful))

      trackStates(channel) {
        withDelay {
          channel.getState shouldEqual ChannelState.DeviceConnected
          group.hasUnjoinedCall shouldEqual false
          channels.hasOngoingCall shouldEqual true
          channel.getParticipants should have size 2
        }
      } shouldEqual List(ChannelState.DeviceConnected)
    }

    scenario("leave group call") {
      trackStates(channel) {
        channel.leave()

        withDelay {
          channel.getState shouldEqual ChannelState.OthersConnected
          group.hasUnjoinedCall shouldEqual true
          channels.hasOngoingCall shouldEqual false
          channels.hasIncomingCall shouldEqual false
          channel.getParticipants should have size 2
          channel.isSilenced shouldEqual false
        }
      } shouldEqual List(ChannelState.DeviceConnected, ChannelState.OthersConnected)
    }

    scenario("auto2 leaves group call, call ends") {
      trackStates(channel) {
        auto2 ? Disconnect should eventually(be(Successful))

        withDelay {
          channel.getState shouldEqual ChannelState.Idle
          group.hasUnjoinedCall shouldEqual false
          channels.hasOngoingCall shouldEqual false
          channels.hasIncomingCall shouldEqual false
          channel.getParticipants shouldBe empty
          channel.isSilenced shouldEqual false
        }
      } shouldEqual List(ChannelState.OthersConnected, ChannelState.Idle)
    }
  }

  feature("Incoming call") {

    scenario("receive incoming call") {
      awaitUi(5.seconds)
      auto2 ? StartCall(RConvId(user.getUser.getId)) should eventually(be(Successful))

      var state = None: Option[Boolean]
      val listener = new UpdateListener {
        override def updated(): Unit = state = Some(channels.hasIncomingCall)
      }
      channels.addUpdateListener(listener)
      withDelay {
        channels.hasIncomingCall shouldEqual true
      }
      withDelay { channels.getIncomingCall.getState shouldEqual ChannelState.OtherCalling }
      channels.hasOngoingCall shouldEqual false
      state shouldEqual Some(true)
    }

    scenario("accept incoming call") {
      spy.reset()
      channels.hasIncomingCall shouldEqual true
      val channel = channels.getIncomingCall
      channel.getState shouldEqual ChannelState.OtherCalling

      channel.join(spy.joinCallback)
      withDelay(channel.getState shouldEqual ChannelState.DeviceJoining)
      withDelay {
        channel.getState shouldEqual ChannelState.DeviceConnected
        channels.hasIncomingCall shouldEqual false
        channels.hasOngoingCall shouldEqual true
        spy.callJoinResult.value shouldEqual CallJoined
      }
    }

    scenario("receive another incoming call") {
      channels.hasOngoingCall shouldEqual true

      auto3 ? StartCall(RConvId(user.getUser.getId)) should eventually(be(Successful))

      awaitUi(3.seconds)
      channels.hasIncomingCall shouldEqual true

      auto3 ? Disconnect should eventually(be(Successful))
    }

    scenario("leave ongoing call") {
      channels.hasOngoingCall shouldEqual true
      val channel = channels.getOngoingCall
      channel.getState shouldEqual ChannelState.DeviceConnected

      trackStates(channel) {
        channel.leave()

        withDelay {
          channel.getState shouldEqual ChannelState.Idle
          channels.hasOngoingCall shouldEqual false
          channels.hasIncomingCall shouldEqual false
        }
      } shouldEqual List(ChannelState.DeviceConnected, ChannelState.Idle) // no other states or updates
    }

    scenario("silence incoming call") {
      awaitUi(3.seconds)
      auto2 ? StartCall(RConvId(user.getUser.getId)) should eventually(be(Successful))

      withDelay(channels.hasIncomingCall shouldEqual true)
      val channel = channels.getIncomingCall
      channel.getState shouldEqual ChannelState.OtherCalling

      awaitUi(1.second) // so that channel is reloaded

      trackStates(channel) {
        channel.silence()

        withDelay {
          channel.getState shouldEqual ChannelState.OtherCalling
          channel.isSilenced shouldEqual true
          channels.hasIncomingCall shouldEqual false
          channels.hasOngoingCall shouldEqual false
        }
      }.toSet shouldEqual Set(ChannelState.OtherCalling)

      awaitUi(1.seconds)

      auto2 ? Disconnect should eventually(be(Successful))
      awaitUi(1.seconds)
      withDelay {
        channel.getState shouldEqual ChannelState.Idle
        channel.isSilenced shouldEqual false
        channels.hasIncomingCall shouldEqual false
        channels.hasOngoingCall shouldEqual false
      }
    }

    scenario("miss incoming call") {
      auto2 ? StartCall(RConvId(user.getUser.getId)) should eventually(be(Successful))
      withDelay {
        channels.hasIncomingCall shouldEqual true
      }
      withPush({ case e: VoiceChannelDeactivateEvent => info(s"event: $e"); true }, zmessaging) {
        auto2 ? Disconnect should eventually(be(Successful))
      }
    }
  }

  feature("Outgoing call") {

    scenario("start a call") {
      spy.reset()
      auto2 ? ResetChannel should eventually(be(Successful))
      withDelay {
        channels.hasIncomingCall shouldEqual false
        channels.hasOngoingCall shouldEqual false
      }

      val userId = provisionedUserId("auto2")

      withDelay(convs should not be empty)
      val conv = convs.find(_.getId == userId.str).get
      val channel = conv.getVoiceChannel
      withDelay(channel.getState shouldEqual ChannelState.Idle)
      channel.join(spy.joinCallback)
      withDelay {
        channel.getState shouldEqual VoiceChannelState.SELF_CALLING
        conv.hasUnjoinedCall shouldEqual true
        channels.hasOngoingCall shouldEqual true
        channels.getOngoingCall shouldEqual channel
        spy.callJoinResult.value shouldEqual CallJoined
      }
    }

    scenario("other end accepts call") {
      val channel = channels.getOngoingCall
      channel.getState shouldEqual VoiceChannelState.SELF_CALLING

      auto2 ? AcceptCall should eventually(be(Successful))

      withDelay(channel.getState shouldEqual VoiceChannelState.SELF_CONNECTED)
      println("connected")
    }

    scenario("other end leaves the call") {
      channels.hasOngoingCall shouldEqual true
      val channel = channels.getOngoingCall
      channel.getState shouldEqual VoiceChannelState.SELF_CONNECTED

      auto2 ? Disconnect should eventually(be(Successful))

      withDelay(channel.getState shouldEqual VoiceChannelState.NO_ACTIVE_USERS)(20.seconds)
    }

    scenario("dismiss while calling") {
      spy.reset()
      val userId = concurrent.Await.result(auto2 ? GetUser, 5.seconds).asInstanceOf[Successful].response
      info(s"user: $userId")

      withDelay(convs should not be empty)
      val conv = convs.find(_.getId == userId).get
      val channel = conv.getVoiceChannel
      channel.getState shouldEqual ChannelState.Idle
      channel.join(spy.joinCallback)
      withDelay {
        channel.getState shouldEqual VoiceChannelState.SELF_CALLING
        conv.hasUnjoinedCall shouldEqual true
        channels.getOngoingCall shouldEqual channel
        spy.callJoinResult.value shouldEqual CallJoined
      }

      auto2 ? WaitCalling should eventually(be(Successful))

      awaitUi(1.second)

      trackStates(channel) {
        channel.leave()

        withDelay {
          channel.getState shouldEqual ChannelState.Idle
          conv.hasUnjoinedCall shouldEqual false
          channels.hasOngoingCall shouldEqual false
          channels.hasIncomingCall shouldEqual false
        }
      } shouldEqual List(ChannelState.DeviceCalling, ChannelState.Idle)

      auto2 ? WaitDisconnected should eventually(be(Successful))
    }

    scenario("Drop outgoing call after timeout") {
      spy.reset()
      callConnectingTimeout = 10.seconds
      try {

        val userId = concurrent.Await.result(auto2 ? GetUser, 5.seconds).asInstanceOf[Successful].response

        withDelay(convs should not be empty)
        val conv = convs.find(_.getId == userId).get
        val channel = conv.getVoiceChannel
        channel.getState shouldEqual ChannelState.Idle
        channel.join(spy.joinCallback)
        withDelay {
          channel.getState shouldEqual ChannelState.DeviceCalling
          conv.hasUnjoinedCall shouldEqual true
          spy.callJoinResult.value shouldEqual CallJoined
        }

        new CallService().onStartCommand(CallService.trackIntent(context, conv.id), 0, 1) // start service manually, since it's not started by Robolectric
        // call should be dropped after timeout
        withDelay {
          channel.getState shouldEqual ChannelState.Idle
        }(15.seconds)

      } finally {
        callConnectingTimeout = 1.minute
      }
    }

    scenario("Leave a call on logout")(pending)
  }
}
