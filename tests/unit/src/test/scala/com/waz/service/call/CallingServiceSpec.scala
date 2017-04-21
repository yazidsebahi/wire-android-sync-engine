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

import com.waz.api.VoiceChannelState.{OTHER_CALLING, SELF_CALLING, SELF_CONNECTED}
import com.waz.api.{NetworkMode, VoiceChannelState}
import com.waz.content.MembersStorage
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{UserId, _}
import com.waz.service.call.AvsV3.ClosedReason
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.{MediaManagerService, NetworkModeService}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Context
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}
import org.threeten.bp.Instant

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

class CallingServiceSpec extends FeatureSpec with Matchers with MockFactory with BeforeAndAfterAll with AndroidFreeSpec {

  val defaultDuration = 5.seconds
  implicit val eventContext = EventContext.Implicits.global
  implicit val executionContext = new SerialDispatchQueue(name = "CallingServiceSpec")

  val context        = mock[Context]
  val avsMock        = mock[AvsV3]
  val flows          = mock[FlowManagerService]
  val mm             = mock[MediaManagerService]
  val network        = mock[NetworkModeService]
  val convs          = mock[ConversationsContentUpdater]
  val members        = mock[MembersStorage]
  val callLogService = mock[CallLogService]
  val messages       = mock[MessagesService]

  lazy val selfUser = UserData("self user")
  lazy val user1 = UserData("Johnny Cash")
  lazy val groupId1 = RConvId()
  lazy val conv1 = ConversationData(ConvId(groupId1.str), groupId1, Some("convName"), selfUser.id, ConversationType.Group)

  feature("Basics") {
    scenario("CallingService intialization") {
      val service = initCallingService()
      service.onReady(3)
      Await.result(service.v3Available.head, defaultDuration) shouldEqual true
      Await.result(service.requestedCallVersion.head, defaultDuration) shouldEqual 3
    }

    scenario("Incoming call") {
      (convs.convByRemoteId _).expects(*).once().returning(Future.successful(Some(conv1)))
      val service = initCallingService()

      signalTest(service.currentCall) { info =>
        info.state == OTHER_CALLING &&
          info.convId.contains(ConvId(groupId1.str))
      } {
        service.onIncomingCall(conv1.remoteId, user1.id, videoCall = false, shouldRing = true)
      }
    }

    scenario("Outgoing call") {
      (avsMock.startCall _).expects(groupId1, false, true).once().returning(Future.successful(0))
      (convs.convById _).expects(*).once().returning(Future.successful(Some(conv1)))
      (members.getByConv _).expects(*).once().returning(Future.successful(scala.collection.immutable.IndexedSeq(ConversationMemberData(user1.id, conv1.id))))

      val service = initCallingService()

      signalTest(service.currentCall) { info =>
        info.state == SELF_CALLING &&
          info.convId.contains(ConvId(groupId1.str))
      } {
        service.startCall(ConvId(groupId1.str), isVideo = false)
      }
    }
  }

  feature("Simultaneous calls") {

    scenario("Current: Receive incoming call while 1:1 call ongoing") {

      val ongoingUserId = UserId()
      val ongoingConv = ConversationData(ConvId(ongoingUserId.str), RConvId(ongoingUserId.str), Some("Ongoing Conv"), selfUser.id, ConversationType.OneToOne)

      val incomingUserId = UserId()
      val incomingConv = ConversationData(ConvId(incomingUserId.str), RConvId(incomingUserId.str), Some("Incoming Conv"), selfUser.id, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().onCall { rConvId: RConvId =>
        Future.successful(rConvId match {
          case ongoingConv.remoteId => Some(ongoingConv)
          case incomingConv.remoteId => Some(incomingConv)
          case _ => None
        })
      }
      (convs.convById _).expects(ConvId(ongoingUserId.str)).anyNumberOfTimes().returning(Future.successful(Some(ongoingConv)))

      val service = initCallingService()

      val checkpoint = callCheckpoint(service)(_.contains(ongoingConv.id))(cur => cur.state == SELF_CONNECTED && cur.others.contains(ongoingUserId))

      service.onIncomingCall(ongoingConv.remoteId, ongoingUserId, videoCall = false, shouldRing = true)
      (avsMock.answerCall _).expects(*, *).once().onCall { (_, _) =>
        service.onEstablishedCall(ongoingConv.remoteId, ongoingUserId)
      }
      service.startCall(ongoingConv.id)
      service.onIncomingCall(incomingConv.remoteId, incomingUserId, videoCall = false, shouldRing = false) //Receive the second call after first is established

      Await.ready(checkpoint.head, defaultDuration)
    }

    scenario("With a background group call, receive a 1:1 call, finish it, and then still join the group call afterwards") {

      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), selfUser.id, ConversationType.Group)

      val otoUser = UserId("otoUser")
      val otoConv = ConversationData(ConvId(otoUser.str), RConvId(otoUser.str), Some("1:1 Conv"), selfUser.id, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().onCall { rConvId: RConvId =>
        Future.successful(rConvId match {
          case groupConv.remoteId => Some(groupConv)
          case otoConv.remoteId => Some(otoConv)
          case _ => None
        })
      }

      (convs.convById _).expects(*).anyNumberOfTimes().onCall { convId: ConvId =>
        Future.successful(convId match {
          case groupConv.id => Some(groupConv)
          case otoConv.id => Some(otoConv)
          case _ => None
        })

      }

      (avsMock.setVideoSendActive _).expects(otoConv.remoteId, false).anyNumberOfTimes()
      val service = initCallingService()

      //Checkpoint 1: Receive and reject a group call
      val checkpoint1 = callCheckpoint(service)(_.contains(groupConv.id))(CallInfo.IsIdle.unapply)

      service.onIncomingCall(groupConv.remoteId, groupMember1, videoCall = false, shouldRing = true)
      (avsMock.rejectCall _).expects(*, *).anyNumberOfTimes()
      service.endCall(groupConv.id) //user rejects the group call


      println("checkpoint1")
      Await.ready(checkpoint1.head, defaultDuration)

      //Checkpoint 2: Receive and accept a 1:1 call
      val checkpoint2 = callCheckpoint(service)(act => act.contains(groupConv.id) && act.contains(otoConv.id))(curr => curr.others.contains(otoUser) && curr.state == VoiceChannelState.SELF_CONNECTED)

      service.onIncomingCall(otoConv.remoteId, otoUser, videoCall = false, shouldRing = true)
      (avsMock.answerCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onEstablishedCall(otoConv.remoteId, otoUser)
      }
      service.startCall(otoConv.id) //user accepts 1:1 call

      println("checkpoint2")
      Await.ready(checkpoint2.head, defaultDuration)

      val checkpoint3 = callCheckpoint(service)(_.contains(groupConv.id))(curr => curr.others == Set(groupMember1, groupMember2) && curr.state == VoiceChannelState.SELF_CONNECTED)
      
      (avsMock.endCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onClosedCall(ClosedReason.Normal, otoConv.remoteId, otoUser, "")
      }
      service.endCall(otoConv.id)

      (avsMock.answerCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onEstablishedCall(groupConv.remoteId, groupMember1)
        service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      }
      service.startCall(groupConv.id)

      println("checkpoint3")
      Await.ready(checkpoint3.head, defaultDuration)

    }
  }

  def callCheckpoint(service: CallingService)(activeCheck: Map[ConvId, CallInfo] => Boolean)(currentCheck: CallInfo => Boolean) =
    (for {
      active <- service.availableCalls
      current <- service.currentCall
    } yield (active, current)).filter { case (active, current) =>
      activeCheck(active) && currentCheck(current)
    }

  def signalTest[A](signal: Signal[A])(test: A => Boolean)(trigger: => Unit): Unit = {
    signal.disableAutowiring()
    trigger
    Await.ready(signal.filter(test).head, defaultDuration)
  }

  def initCallingService(context:        Context                     = context,
                         self:           UserId                      = UserId(),
                         avs:            AvsV3                       = avsMock,
                         convs:          ConversationsContentUpdater = convs,
                         members:        MembersStorage              = members,
                         flows:          FlowManagerService          = flows,
                         messages:       MessagesService             = messages,
                         media:          MediaManagerService         = mm,
                         callLogService: CallLogService              = callLogService,
                         network:        NetworkModeService          = network) = {
    val initPromise = Promise[Unit]()
    (context.startService _).expects(*).anyNumberOfTimes().returning(null)
    (avs.available _).expects().once().returning(Future.successful({}))
    (flows.flowManager _).expects().once().returning(None)
    (media.mediaManager _).expects().once().returning(None)
    (callLogService.addEstablishedCall _).expects(*, *, *).anyNumberOfTimes().returning(Future.successful({}))
    (messages.addMissedCallMessage(_:RConvId, _:UserId, _:Instant)).expects(*, *, *).anyNumberOfTimes().returning(Future.successful(None))
    (messages.addMissedCallMessage(_:ConvId, _:UserId, _:Instant)).expects(*, *, *).anyNumberOfTimes().returning(Future.successful(None))
    (messages.addSuccessfulCallMessage _).expects(*, *, *, *).anyNumberOfTimes().returning(Future.successful(None))
    (network.networkMode _).expects().once().returning(Signal.empty[NetworkMode])
    (avs.init _).expects(*).once().onCall{ service: CallingService =>
      initPromise.success({})
      initPromise.future
    }
    val service = new DefaultCallingService(context, self, avs, convs, members, null, flows, messages, media, null, callLogService, network, null)
    Await.ready(initPromise.future, defaultDuration)
    service
  }
}
