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

import java.util.concurrent.atomic.AtomicInteger

import android.content.Context
import android.telephony.TelephonyManager
import com.waz._
import com.waz.api.{CauseForCallStateEvent, NetworkMode}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.Event.CallProperties
import com.waz.model.VoiceChannelData.{ChannelState, ConnectionState}
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.service.call.DefaultFlowManagerService.EstablishedFlows
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.VoiceChannelClient
import com.waz.sync.client.VoiceChannelClient.JoinCallFailed
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonEncoder
import com.waz.utils.events.EventContext
import com.waz.znet.JsonObjectResponse
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.json.JSONObject
import org.robolectric.Robolectric
import org.robolectric.shadows.ShadowTelephonyManager2
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.threeten.bp.Instant
import com.waz.ZLog.ImplicitTag._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.implicitConversions

class VoiceChannelServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with OptionValues with RobolectricTests with RobolectricUtils with GeneratorDrivenPropertyChecks { test =>

  lazy val selfUser = UserData("self user")
  lazy val user1 = UserData("user 1")
  lazy val user2 = UserData("user 2")

  implicit val timeout: Timeout = 5.seconds
  implicit val ec: EventContext = EventContext.Global

  lazy val conv = ConversationData(ConvId(user1.id.str), RConvId(), Some("convName"), selfUser.id, ConversationType.OneToOne)
  lazy val conv1 = ConversationData(ConvId(user2.id.str), RConvId(), Some("convName 1"), selfUser.id, ConversationType.OneToOne)

  var service: MockZMessaging = _
  var sync: SyncServiceHandle = _
  var messageSync = None: Option[MessageId]

  var response: Either[JoinCallFailed, CallStateEvent] = _
  var request = Option.empty[(CallDeviceState, CauseForCallStateEvent)]

  var shadowTelManager: ShadowTelephonyManager2 = _
  @volatile var currentlyActiveChannel: Option[VoiceChannelData] = None

  before {
    ZMessaging.context = Robolectric.application
    shadowTelManager = Robolectric.shadowOf(Robolectric.application.getSystemService(Context.TELEPHONY_SERVICE).asInstanceOf[TelephonyManager]).asInstanceOf[ShadowTelephonyManager2]
    shadowTelManager.callState = TelephonyManager.CALL_STATE_IDLE

    messageSync = None
    request = None
    response = Left(JoinCallFailed("test-default", None, None, None))

    service = new MockZMessaging(selfUserId = selfUser.id) {

      override lazy val sync = new EmptySyncService {
        override def postMessage(id: MessageId, conv: ConvId, time: Instant) = {
          messageSync = Some(id)
          super.postMessage(id, conv, time)
        }
      }

      override lazy val voiceClient: VoiceChannelClient = new VoiceChannelClient(zNetClient) {
        override def updateSelfCallState(id: RConvId, deviceState: CallDeviceState, cause: CauseForCallStateEvent): ErrorOrResponse[Either[JoinCallFailed, CallStateEvent]] = {
          request = Some((deviceState, cause))
          CancellableFuture.successful(Right(response))
        }
      }

      override lazy val flowmanager: DefaultFlowManagerService = new DefaultFlowManagerService(context, zNetClient, websocket, prefs, network) {
        override lazy val flowManager = None
      }

      insertUsers(Seq(selfUser, user1, user2))
      Seq(conv, conv1) foreach insertConv

      push.connectedPushPromise.trySuccess(push)
      network.networkMode ! NetworkMode.WIFI

      voice.content.activeChannel { currentlyActiveChannel = _ }
    }

    currentlyActiveChannel = None
    Await.result(service.convsContent.addConversationMembers(conv.id, selfUser.id, Seq(user1.id)), timeout)
    Await.result(service.convsContent.addConversationMembers(conv1.id, selfUser.id, Seq(user2.id)), timeout)
  }

  after {
    Await.result(service.db.close(), 10.seconds)
    Robolectric.application.getDatabasePath(service.db.dbHelper.getDatabaseName).delete()
  }

  def idleEvent(id: RConvId = conv.remoteId, idx: Option[CallSequenceNumber] = None) = CallStateEvent(id, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = idx)
  def incomingCall(id: RConvId = conv.remoteId, idx: Option[CallSequenceNumber] = None) = CallStateEvent(id, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = idx)
  def cancelledCall(idx: Option[CallSequenceNumber] = None) = CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = false))), device = deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = idx)

  implicit def int_to_optSeqNum(idx: Int): Option[CallSequenceNumber] = Some(CallSequenceNumber(idx))

  def processEvents(events: CallStateEvent*) = {
    events foreach { ev =>
      Await.result(service.voice.handleCallStateEvent(ev), timeout)
    }
  }

  def listChannels = service.voiceContent.channels.values.toList
  def listParticipants = service.voiceStorage.participants.values.flatten
  def insertConv(conv: ConversationData) = Await.result(service.convsStorage.insert(conv), timeout)
  def getConv(id: ConvId) = Await.result(service.convsContent.convById(id), timeout)

  def assertSingleChannel(body: VoiceChannelData => Unit) = withDelay {
    val channels = listChannels
    channels should have size 1
    val channel = channels.head
    channel.id shouldEqual conv.id
    body(Await.result(channel.getData, timeout))
  }

  def assertParticipants(ps: VoiceParticipantData*) = listParticipants.map(_.copy(idx = 0)).toSet shouldEqual ps.toSet

  implicit def user_to_participant(userId: UserId): VoiceParticipantData = VoiceParticipantData(conv.id, userId, ConnectionState.Idle)
  implicit def user_state_to_participant(p: (UserId, ConnectionState)): VoiceParticipantData =
    p match { case (userId, state) => VoiceParticipantData(conv.id, userId, state) }

  feature("event handling") {
    scenario("handle initial idle state event") {
      service.dispatchEvent(idleEvent())

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.Idle
        channel.deviceState shouldEqual ConnectionState.Idle
      }

      assertParticipants(selfUser.id, user1.id)
    }

    scenario("handle initial event for multiple conversations") {
      service.dispatchEvent(idleEvent())

      val conv1 = insertConv(ConversationData(ConvId(), RConvId(), Some("convName 1"), selfUser.id, ConversationType.Group))
      val conv2 = insertConv(ConversationData(ConvId(), RConvId(), Some("convName 1"), selfUser.id, ConversationType.Group))

      service.dispatchEvent(idleEvent(conv1.remoteId))
      service.dispatchEvent(idleEvent(conv2.remoteId))

      withDelay {
        listChannels should have size 3
        listChannels.map(_.active) shouldEqual Seq(false, false, false)
      }
    }

    scenario("incoming calls should produce notifications") {
      val spy = new IncomingChannelSpy
      checkIncomingForNotification(conv.id)
      withDelay { spy.channels.value should have size 1 }
    }

    scenario("handle incoming call") {
      service.dispatchEvent(idleEvent())
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))
      service.dispatchEvent(incomingCall())

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.tracking.cause shouldEqual CauseForCallStateEvent.REQUESTED
        channel.state shouldEqual ChannelState.OtherCalling
        channel.deviceState shouldEqual ConnectionState.Idle
      }

      assertParticipants(selfUser.id, user1.id -> ConnectionState.Connected)
    }

    scenario("queue up second incoming call") {
      service.dispatchEvent(idleEvent())
      withDelay(listChannels.filter(_.active) should be(empty))

      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))
      service.dispatchEvent(incomingCall())

      withDelay(listChannels.filter(_.active) should have size 1)
      service.dispatchEvent(CallStateEvent(conv1.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user2.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv1.remoteId, Set(user2.id))

      awaitUi(100.millis)
      listChannels.filter(_.active).foreach { channel =>
        withClue(s"for channel ${channel.id}") {
          channel.data.muted shouldEqual false
          channel.data.tracking.cause shouldEqual CauseForCallStateEvent.REQUESTED
          channel.data.state shouldEqual ChannelState.OtherCalling
          channel.data.deviceState shouldEqual ConnectionState.Idle
        }
      }

      assertParticipants(
        VoiceParticipantData(conv.id, selfUser.id, ConnectionState.Idle),
        VoiceParticipantData(conv.id, user1.id, ConnectionState.Connected),
        VoiceParticipantData(conv1.id, selfUser.id, ConnectionState.Idle),
        VoiceParticipantData(conv1.id, user2.id, ConnectionState.Connected))
    }

    scenario("interrupt call from other side") {
      service.dispatchEvent(idleEvent())
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))
      service.dispatchEvent(incomingCall())
      withDelay(listChannels.filter(_.active) should have size 1)
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.INTERRUPTED))

      withDelay {
        val channel = listChannels.find(_.id == conv.id).value
        channel.id shouldEqual conv.id
        channel.data.state shouldEqual ChannelState.Idle
        channel.data.deviceState shouldEqual ConnectionState.Idle
        channel.data.tracking.cause shouldEqual CauseForCallStateEvent.INTERRUPTED
      }
    }

    scenario("start a call from current device") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.DeviceCalling
        channel.deviceState shouldEqual ConnectionState.Connecting
      }

      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id)
    }

    scenario("others volume changes") {
      var volumeChanged = Option.empty[Float]
      service.voice.volumeChanged(conv.id, user1.id).on(Threading.Ui) { v => volumeChanged = Some(v) }

      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onVolumeChanged ! (conv.remoteId, user1.id, 0.25f)
      withDelay { volumeChanged shouldEqual Some(0.25f) }
    }

    scenario("own volume changes") {
      var volumeChanged = Option.empty[Float]
      service.voice.volumeChanged(conv.id, selfUser.id).on(Threading.Ui) { v => volumeChanged = Some(v) }

      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))

      service.flowmanager.onVolumeChanged ! (conv.remoteId, UserId("self"), 0.25f)
      withDelay { volumeChanged shouldEqual Some(0.25f) }
    }

    scenario("start a call from other device") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.UserCalling
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id)
    }

    scenario("accept incoming call on current device") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.DeviceJoining
        channel.deviceState shouldEqual ConnectionState.Connecting
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)
    }

    scenario("accept incoming call on other device and check for voice channel notification on another incoming call") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.UserConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)

      withNotification(conv1.id) {
        service.dispatchEvent(CallStateEvent(conv1.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user2.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      }
      listChannels.filter(_.ongoing) should have size 1
      listChannels.filter(_.active) should have size 2
    }

    scenario("second incoming call (different conversation) with one already accepted on other device") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))

      withDelay {
        listChannels.map(_.data.state) shouldEqual Seq(ChannelState.UserConnected)
      }

      service.dispatchEvent(CallStateEvent(conv1.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))

      withDelay {
        val channels = listChannels
        channels should have size 2
        channels.filter(_.active) should have size 2
        channels.filter(_.ongoing) should have size 1
        channels.find(_.data.state == ChannelState.UserConnected) should be('defined)
        val channel = channels.find(_.data.state == ChannelState.UserConnected).get
        channel.id shouldEqual conv.id
      }
    }

    scenario("other user accepts call started from current device then we transfer it to other device") {
      // start calling
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.DeviceCalling
        channel.deviceState shouldEqual ConnectionState.Connecting
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id)

      // media established while calling
      service.flowmanager.onMediaEstablished ! conv.remoteId
      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.DeviceCalling
        channel.deviceState shouldEqual ConnectionState.Connected
      }

      // other user joins
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.DeviceConnected
        channel.deviceState shouldEqual ConnectionState.Connected
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)

      // transfer to other device
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.UserConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)
    }

    scenario("other user accepts call started from other device and we transfer the call to current device then the call is ended") {
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.UserConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)

      // transfer to current device
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.DeviceJoining
        channel.deviceState shouldEqual ConnectionState.Connecting
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)

      // media established
      service.flowmanager.onMediaEstablished ! conv.remoteId

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.DeviceConnected
        channel.deviceState shouldEqual ConnectionState.Connected
      }
      assertParticipants(selfUser.id -> ConnectionState.Connected, user1.id -> ConnectionState.Connected)

      //call is ended from other side
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = false), participant(selfUser.id, joined = false))), deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.Idle
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(selfUser.id, user1.id)
    }

    scenario("other users create and accept a call in group conversation, user joins group call then leaves it") {
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user2.id, joined = true), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user2.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id, user2.id))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.OthersConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(user1.id -> ConnectionState.Connected, user2.id -> ConnectionState.Connected)

      // join on current device
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user2.id, joined = true), participant(user1.id, joined = true), participant(selfUser.id, joined = true))), deviceState(joined = true), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.state shouldEqual ChannelState.DeviceJoining
        channel.deviceState shouldEqual ConnectionState.Connecting
      }
      assertParticipants(user1.id -> ConnectionState.Connected, user2.id -> ConnectionState.Connected, selfUser.id -> ConnectionState.Connected)

      // media established
      service.flowmanager.onMediaEstablished ! conv.remoteId
      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.DeviceConnected
        channel.deviceState shouldEqual ConnectionState.Connected
      }
      assertParticipants(user1.id -> ConnectionState.Connected, user2.id -> ConnectionState.Connected, selfUser.id -> ConnectionState.Connected)

      // leave
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user2.id, joined = true), participant(user1.id, joined = true), participant(selfUser.id, joined = false))), deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.OthersConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(user1.id -> ConnectionState.Connected, user2.id -> ConnectionState.Connected, selfUser.id -> ConnectionState.Idle)


      //join on other device
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user2.id, joined = true), participant(user1.id, joined = true), participant(selfUser.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.UserConnected
        channel.deviceState shouldEqual ConnectionState.Idle
      }
      assertParticipants(user1.id -> ConnectionState.Connected, user2.id -> ConnectionState.Connected, selfUser.id -> ConnectionState.Connected)
    }

    def checkIncomingForNotification(id: ConvId): Unit = {
      service.dispatchEvent(idleEvent())

      withNotification(id) {
        service.dispatchEvent(incomingCall())
      }
    }
  }

  feature("handling of events received out of order") {

    scenario("ignore single out of order call state event") {
      service.dispatchEvent(idleEvent(idx = 1))
      assertSingleChannel { _.state shouldEqual ChannelState.Idle }
      val spy = new IncomingChannelSpy

      service.dispatchEvent(incomingCall(idx = 5))
      spy.numUpdates.get shouldEqual 2
      assertSingleChannel { _.state == ChannelState.OtherCalling }

      service.dispatchEvent(cancelledCall(idx = 4))
      assertSingleChannel { _.state == ChannelState.OtherCalling }
      spy.numUpdates.get shouldEqual 2

      service.dispatchEvent(cancelledCall(idx = None))
      spy.numUpdates.get shouldEqual 3
      assertSingleChannel { _.state == ChannelState.Idle }

      service.dispatchEvent(incomingCall(idx = 3))
      spy.numUpdates.get shouldEqual 4
      assertSingleChannel { _.state == ChannelState.OtherCalling }

      service.dispatchEvent(cancelledCall(idx = 2))
      spy.numUpdates.get shouldEqual 4
      assertSingleChannel { _.state == ChannelState.OtherCalling }
    }

    scenario("ignore multiple out of order call state events") {
      service.dispatchEvent(idleEvent(idx = 10))
      assertSingleChannel { _.state shouldEqual ChannelState.Idle }
      val spy = new IncomingChannelSpy

      service.dispatchEvents(Seq(incomingCall(idx = 11), cancelledCall(idx = None), cancelledCall(idx = 3), incomingCall(idx = 3)))
      spy.numUpdates.get shouldEqual 2
      assertSingleChannel { _.state == ChannelState.OtherCalling }
    }

    scenario("ignore out of order events when mixing own state transitions with incoming events") {
      // call other
      service.dispatchEvent(idleEvent(idx = 1))

      val spy = new ActiveChannelSpy

      response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 2))
      Await.result(service.voice.joinVoiceChannel(conv.id), 100.millis)

      spy.assertChannelState(2, ChannelState.DeviceCalling)

      // other accepts and we cancel (at nearly the same time)
      response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = false))), device = deviceState(joined = false), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 4))
      service.voice.leaveVoiceChannel(conv.id)
      awaitUi(100.millis)
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 3))

      spy.assertChannelState(3, ChannelState.Idle)
    }
  }

  feature("automatically drop second call") {

    scenario("queue incoming when calling") {
      withNotification(conv.id) { service.dispatchEvent(idleEvent()) }
      service.voice.joinVoiceChannel(conv.id)

      service.dispatchEvent(CallStateEvent(conv1.remoteId, Some(Set(participant(user2.id, joined = true), participant(selfUser.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      withDelay {
        listChannels.filter(_.ongoing).map(_.id) shouldEqual Seq(conv.id)
        listChannels.filter(_.active).map(_.id) should contain(conv1.id)
        incoming.value.map(_.id) should contain(conv1.id)
      }
    }

    scenario("queue incoming when connected") {
      service.dispatchEvent(idleEvent())
      service.voice.joinVoiceChannel(conv.id)
      service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      withDelay { listChannels.filter(_.active).map(_.id) shouldEqual Seq(conv.id) }

      service.dispatchEvent(CallStateEvent(conv1.remoteId, Some(Set(participant(user2.id, joined = true), participant(selfUser.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      withDelay {
        listChannels.filter(_.ongoing).map(_.id) shouldEqual Seq(conv.id)
        listChannels.filter(_.active).map(_.id) should contain(conv1.id)
        incoming.value.map(_.id) should contain(conv1.id)
      }
    }

    scenario("silence incoming call when in GSM call (don't notify backend)") {
      withNotification(conv.id) { service.dispatchEvent(idleEvent()) }
      shadowTelManager.callState = TelephonyManager.CALL_STATE_RINGING

      service.dispatchEvent(incomingCall())
      withDelay {
        request shouldEqual None
      }
      withDelay {
        listChannels.filter(_.active) shouldBe empty
        incoming.value shouldBe empty
      }
    }

    scenario("drop outgoing when in GSM call") {
      withNotification(conv.id) { service.dispatchEvent(idleEvent()) }
      shadowTelManager.callState = TelephonyManager.CALL_STATE_RINGING

      val payload = JsonObjectResponse(JsonEncoder { o =>
        o.put("self", CallDeviceState.Encoder(CallDeviceState(joined = true, props = Set.empty)))
        o.put("cause", CauseForCallStateEvent.REQUESTED.asJson)
        o.put("participants", new JSONObject())
      })

      response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 2))
      service.voice.joinVoiceChannel(conv.id)

      withDelay {
        request.value shouldEqual (CallDeviceState(joined = false, props = Set.empty), CauseForCallStateEvent.INTERRUPTED)
        listChannels.filter(_.active) should be(empty)
      }
    }

    scenario("drop ongoing when GSM call comes in") {
      withNotification(conv.id) { service.dispatchEvent(idleEvent()) }
      withNotification(conv.id) { service.dispatchEvent(incomingCall()) }

      response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 2))
      service.voice.joinVoiceChannel(conv.id)

      withDelay {
        listChannels.map(_.data.state) shouldEqual Seq(ChannelState.DeviceJoining)
      }

      shadowTelManager.callState = TelephonyManager.CALL_STATE_RINGING
      Option(shadowTelManager.getListener).foreach(_.onCallStateChanged(TelephonyManager.CALL_STATE_RINGING, ""))

      withDelay {
        listChannels.map(_.data.state) shouldEqual Seq(ChannelState.Idle)
        listChannels.map(_.data.tracking.cause).head shouldEqual CauseForCallStateEvent.INTERRUPTED
      }
    }

    def incoming: Option[Vector[VoiceChannelData]] = service.voiceContent.activeChannels.map(_.incoming).currentValue
  }

  feature("ui notifications") {
    scenario("Notify ui on incoming call") {
      service.dispatchEvent(idleEvent())

      val incomingSpy = new FirstIncomingChannelSpy
      val activeSpy = new ActiveChannelSpy
      withNotification(conv.id) {
        service.dispatchEvent(incomingCall())
      }
      withDelay {
        currentlyActiveChannel shouldEqual None
        incomingSpy.numUpdates.get shouldEqual 2
        activeSpy.numUpdates.get shouldEqual 1
        firstIncoming shouldBe 'defined
      }
    }

    scenario("Notify ui on incoming call end") {
      val incomingSpy = new FirstIncomingChannelSpy
      val activeSpy = new ActiveChannelSpy
      withUiNotificationOnCall(conv.id) {
        service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = false), participant(selfUser.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED))
      }
      withDelay {
        currentlyActiveChannel shouldEqual None
        incomingSpy.numUpdates.get should (be > 1 and be <= 3)
        activeSpy.numUpdates.get shouldEqual 1
        firstIncoming shouldEqual None
      }
    }

    scenario("Notify ui on call accept") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())

      val incomingSpy = new FirstIncomingChannelSpy
      val activeSpy = new ActiveChannelSpy

      withUiNotificationOnCall(conv.id) {
        response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED, device = deviceState(joined = true)))
        service.voice.joinVoiceChannel(conv.id)
      }

      withDelay {
        currentlyActiveChannel shouldBe 'defined
        incomingSpy.numUpdates.get should be > 1 // initial none, then update to incoming, then back to none (as it's promoted to ongoing)
        activeSpy.assertChannelState(2, ChannelState.DeviceJoining)
        firstIncoming shouldEqual None
      }
    }

    scenario("Notify ui on call accept on other device") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())

      val incomingSpy = new FirstIncomingChannelSpy
      val activeSpy = new ActiveChannelSpy

      withUiNotificationOnCall(conv.id) {
        service.dispatchEvent(CallStateEvent(conv.remoteId, Some(Set(participant(user1.id, joined = true), participant(selfUser.id, joined = true))), cause = CauseForCallStateEvent.REQUESTED))
      }

      withDelay {
        currentlyActiveChannel shouldBe 'defined
        incomingSpy.numUpdates.get should be > 1 // initial None, then update to one incoming, then back to none (as it's promoted to ongoing)
        activeSpy.assertChannelState(2, ChannelState.UserConnected)
        firstIncoming shouldEqual None
      }
    }

    scenario("Notify ui on call end") {
      val incomingSpy = new FirstIncomingChannelSpy
      val activeSpy = new ActiveChannelSpy

      withUiNotificationOnCall(conv.id) {
        response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = false), participant(user1.id, joined = false))), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 2))
        service.voice.leaveVoiceChannel(conv.id)
      }
      withDelay {
        currentlyActiveChannel shouldEqual None
        incomingSpy.numUpdates.get should (be <= 3 and be > 1)
        activeSpy.numUpdates.get shouldEqual 1
        firstIncoming shouldEqual None
      }
    }

    def withUiNotificationOnCall(id: ConvId)(body: => Unit) = {
      service.dispatchEvent(idleEvent())
      val spy = new FirstIncomingChannelSpy

      withNotification(conv.id) {
        service.dispatchEvent(incomingCall())
      }

      withNotification(id) { body }
    }

    def firstIncoming: Option[VoiceChannelData] = service.voiceContent.activeChannels.map(_.incoming).currentValue.flatMap(_.headOption)
  }

  feature("silencing") {

    scenario("silence incoming call") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))

      assertSingleChannel(_.silenced shouldEqual false)

      withNotification(conv.id) {
        service.voice.silenceVoiceChannel(conv.id)
      }

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.silenced shouldEqual true
        channel.state shouldEqual ChannelState.OtherCalling
        channel.deviceState shouldEqual ConnectionState.Idle
      }

      assertParticipants(selfUser.id, user1.id -> ConnectionState.Connected)
    }

    scenario("unsilence a call") {
      service.dispatchEvent(idleEvent())
      service.flowmanager.onFlowsEstablished ! EstablishedFlows(conv.remoteId, Set(user1.id))
      service.dispatchEvent(incomingCall())

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.OtherCalling
        channel.silenced shouldEqual false
      }

      service.voice.silenceVoiceChannel(conv.id)
      awaitUi(100.millis)
      assertSingleChannel(_.silenced shouldEqual true)

      withNotification(conv.id) {
        service.voice.unsilenceVoiceChannel(conv.id)
      }

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.silenced shouldEqual false
        channel.state shouldEqual ChannelState.OtherCalling
        channel.deviceState shouldEqual ConnectionState.Idle
      }

      assertParticipants(selfUser.id, user1.id -> ConnectionState.Connected)
    }

    scenario("unsilence when joining") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())
      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.OtherCalling
        channel.silenced shouldEqual false
      }

      service.voice.silenceVoiceChannel(conv.id)
      awaitUi(100.millis)
      assertSingleChannel(_.silenced shouldEqual true)

      withNotification(conv.id) {
        service.voice.joinVoiceChannel(conv.id)
      }

      assertSingleChannel { channel =>
        channel.muted shouldEqual false
        channel.silenced shouldEqual false
        channel.state shouldEqual ChannelState.DeviceJoining
        channel.deviceState shouldEqual ConnectionState.Connecting
      }
    }

    scenario("unsilence when other cancels the call") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())
      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.OtherCalling
        channel.silenced shouldEqual false
      }

      service.voice.silenceVoiceChannel(conv.id)
      awaitUi(100.millis)
      assertSingleChannel(_.silenced shouldEqual true)

      service.dispatchEvent(cancelledCall())
      awaitUi(100.millis)
      assertSingleChannel(_.silenced shouldEqual false)
    }
  }

  feature("Error handling") {

    scenario("Drop incoming call on flow manager error") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.OtherCalling
      }

      awaitUi(50.millis)
      service.flowmanager.onFlowManagerError ! (conv.remoteId, 1)

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.Idle
        channel.tracking.cause shouldEqual CauseForCallStateEvent.FLOW_ERROR
      }
    }

    scenario("Drop ongoing call on flow manager error") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(incomingCall())

      assertSingleChannel { channel => channel.state shouldEqual ChannelState.OtherCalling }
      response = Right(CallStateEvent(conv.remoteId, Some(Set(participant(selfUser.id, joined = true), participant(user1.id, joined = true))), device = Some(CallDeviceState(joined = true, Set())), cause = CauseForCallStateEvent.REQUESTED, sequenceNumber = 2))
      service.voice.joinVoiceChannel(conv.id)
      assertSingleChannel { channel => channel.state shouldEqual ChannelState.DeviceJoining }
      service.flowmanager.onMediaEstablished ! conv.remoteId
      assertSingleChannel { channel => channel.state shouldEqual ChannelState.DeviceConnected }

      awaitUi(50.millis)
      service.flowmanager.onFlowManagerError ! (conv.remoteId, 1)

      assertSingleChannel { channel =>
        channel.state shouldEqual ChannelState.Idle
        channel.tracking.cause shouldEqual CauseForCallStateEvent.FLOW_ERROR
      }
    }
  }

  feature("Incoming calls (multiple channels)") {
    scenario("a single incoming call") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(idleEvent(conv1.remoteId))
      val spy = new IncomingChannelSpy

      service.dispatchEvent(incomingCall())

      withDelay {
        spy.channels.value.map(_.id) shouldEqual Vector(conv.id)
      }
    }

    scenario("multiple incoming calls") {
      service.dispatchEvent(idleEvent())
      service.dispatchEvent(idleEvent(conv1.remoteId))
      val spy = new IncomingChannelSpy

      service.dispatchEvent(incomingCall())
      service.dispatchEvent(incomingCall(conv1.remoteId))

      withDelay {
        spy.channels.value.map(_.id) shouldEqual Vector(conv.id, conv1.id)
      }
    }
  }

  private def withNotification[T](id: ConvId)(op: => T): T = {
    var notified = false
    service.voice.voiceChannelSignal(id).on(Threading.Ui) { _ => notified = true }
    try op
    finally withDelay(notified shouldEqual true)
  }
  private def participant(id: UserId, joined: Boolean, props: CallProperties = Set.empty) = CallParticipant(id, joined = joined, props = props)
  private def deviceState(joined: Boolean, props: CallProperties = Set.empty): Option[CallDeviceState] = Some(CallDeviceState(joined = joined, props = props))

  class ActiveChannelSpy {
    val numUpdates = new AtomicInteger(0)
    service.voiceContent.activeChannel { _ => numUpdates.incrementAndGet() }

    def assertChannelState(expectedNumUpdates: Int, expectedState: ChannelState):  Unit = assertSingleChannel { channel =>
      withClue(s"After $expectedNumUpdates, channel state should be $expectedState") {
        numUpdates.get() shouldEqual expectedNumUpdates
        channel.state shouldEqual expectedState
      }
    }
  }

  class IncomingChannelSpy {
    @volatile var channels = Option.empty[Vector[VoiceChannelData]]
    val numUpdates = new AtomicInteger(0)

    service.voiceContent.activeChannels { ch =>
      channels = Some(ch.incoming)
      numUpdates.incrementAndGet
    }

    def reset(): Unit = {
      channels = None
      numUpdates.set(0)
    }
  }

  class FirstIncomingChannelSpy {
    val numUpdates = new AtomicInteger(0)
    service.voiceContent.activeChannels.map(_.incoming.headOption) { _ => numUpdates.incrementAndGet() }
  }
}
