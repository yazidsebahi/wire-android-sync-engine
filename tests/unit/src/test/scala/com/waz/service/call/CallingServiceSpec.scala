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

import com.waz.api.NetworkMode
import com.waz.content.MembersStorage
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{UserId, _}
import com.waz.service.call.AvsV3.ClosedReason.{AnsweredElsewhere, Normal, StillOngoing}
import com.waz.service.call.CallInfo.CallState._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.{MediaManagerService, NetworkModeService}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Context
import org.threeten.bp.Instant

import scala.concurrent.{Future, Promise}

class CallingServiceSpec extends AndroidFreeSpec {

  implicit val eventContext = EventContext.Implicits.global
  implicit val executionContext = new SerialDispatchQueue(name = "CallingServiceSpec")

  val context        = mock[Context]
  val avs            = mock[AvsV3]
  val flows          = mock[FlowManagerService]
  val members        = mock[MembersStorage]
  val media          = mock[MediaManagerService]
  val network        = mock[NetworkModeService]
  val convs          = mock[ConversationsContentUpdater]
  val callLogService = mock[CallLogService]
  val messages       = mock[MessagesService]

  val self     = UserId("selfUserId")
  val account  = AccountId(self.str)

  feature("Basics") {
    scenario("CallingService intialization") {
      val service = initCallingService()
      service.onReady(3)
      result(service.v3Available.head) shouldEqual true
      result(service.requestedCallVersion.head) shouldEqual 3
    }

    scenario("Incoming call 1:1 call goes through SelfJoining to become SelfConnected") {
      val otherUser = UserId("otherUser")
      val _1t1Conv = ConversationData(ConvId(), RConvId(), Some("1:1 Conv"), self, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfJoining))
      val checkpoint2 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfConnected && cur.others == Set(otherUser)))

      service.onIncomingCall(_1t1Conv.remoteId, otherUser, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(*, *).once().onCall { (_, _) =>
        service.onEstablishedCall(_1t1Conv.remoteId, otherUser)
      }
      service.startCall(_1t1Conv.id)
      result(checkpoint1.head)
      result(checkpoint2.head)
    }

    scenario("Incoming group call goes through SelfJoining to become SelfConnected") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfJoining))
      val checkpoint2 = callCheckpoint(service, _.contains(groupConv.id),
        _.exists(cur => cur.convId == groupConv.id && cur.state == SelfConnected && cur.others == Set(groupMember1, groupMember2)))

      service.onIncomingCall(groupConv.remoteId, groupMember1, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(*, *).once().onCall { (_, _) =>
        service.onEstablishedCall(groupConv.remoteId, groupMember1)
        service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      }
      service.startCall(groupConv.id)
      result(checkpoint1.head)
      result(checkpoint2.head)
    }

    scenario("Outgoing 1:1 call goes through SelfCalling to SelfJoining to SelfConnected") {
      val otherUser = UserId("otherUser")
      val _1t1Conv = ConversationData(ConvId(otherUser.str), RConvId(), Some("1:1 Conv"), self, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfCalling && cur.caller == self && cur.others == Set(otherUser)))
      val checkpoint2 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfJoining && cur.caller == self && cur.others == Set(otherUser)))
      val checkpoint3 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(otherUser)))

      (avs.startCall _).expects(*, *, *).once().returning(Future.successful(0))

      service.startCall(_1t1Conv.id)
      result(checkpoint1.head)

      service.onOtherSideAnsweredCall(_1t1Conv.remoteId)
      result(checkpoint2.head)

      service.onEstablishedCall(_1t1Conv.remoteId, otherUser)
      result(checkpoint3.head)
    }

    scenario("Outgoing group call goes through SelfCalling to SelfJoining to SelfConnected") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))

      val service = initCallingService()

      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfCalling && cur.caller == self && cur.others == Set(self)))
      val checkpoint2 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfJoining && cur.caller == self && cur.others == Set(self)))
      val checkpoint3 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(groupMember1)))
      val checkpoint4 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(groupMember1, groupMember2)))

      (avs.startCall _).expects(*, *, *).once().returning(Future.successful(0))

      service.startCall(groupConv.id)
      result(checkpoint1.head)

      service.onOtherSideAnsweredCall(groupConv.remoteId)
      result(checkpoint2.head)

      //TODO which user from a group conversation gets passed down here?
      service.onEstablishedCall(groupConv.remoteId, groupMember1)
      println(result(service.currentCall.map(_.get.others).head))
      result(checkpoint3.head)

      service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      result(checkpoint4.head)
    }

    scenario("Team conversation with only 1 other member should be treated as 1:1 conversation - outgoing") {
      val otherUser = UserId("otherUser")
      val _1t1Conv = ConversationData(ConvId(), RConvId(), Some("1:1 Team Conv"), self, ConversationType.Group, Some(TeamId())) //all team convs are goup by type

      (convs.convByRemoteId _).expects(_1t1Conv.remoteId).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (convs.convById _).expects(_1t1Conv.id).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (members.getByConvs _).expects(Set(_1t1Conv.id)).twice().returning(Future.successful(IndexedSeq(otherUser, self).map(u => ConversationMemberData(u, _1t1Conv.id))))
      (avs.startCall _).expects(_1t1Conv.remoteId, false, false).once().returning(Future.successful(0))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfCalling && cur.caller == self && cur.others == Set(otherUser) && !cur.isGroup))
      val checkpoint2 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfJoining && cur.caller == self && cur.others == Set(otherUser) && !cur.isGroup))
      val checkpoint3 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(otherUser) && !cur.isGroup))

      service.startCall(_1t1Conv.id)
      result(checkpoint1.head)

      service.onOtherSideAnsweredCall(_1t1Conv.remoteId)
      result(checkpoint2.head)

      service.onEstablishedCall(_1t1Conv.remoteId, otherUser)
      result(checkpoint3.head)
    }

    scenario("Team conversation with only 1 other member should be treated as 1:1 conversation - incoming") {
      val otherUser = UserId("otherUser")
      val _1t1Conv = ConversationData(ConvId(), RConvId(), Some("1:1 Team Conv"), self, ConversationType.Group, Some(TeamId()))

      (convs.convByRemoteId _).expects(_1t1Conv.remoteId).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (convs.convById _).expects(_1t1Conv.id).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (members.getByConvs _).expects(Set(_1t1Conv.id)).repeated(3).returning(Future.successful(IndexedSeq(otherUser, self).map(u => ConversationMemberData(u, _1t1Conv.id))))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfJoining && !cur.isGroup))
      val checkpoint2 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == SelfConnected && cur.others == Set(otherUser) && !cur.isGroup))

      service.onIncomingCall(_1t1Conv.remoteId, otherUser, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(_1t1Conv.remoteId, false).once().onCall { (_, _) =>
        service.onEstablishedCall(_1t1Conv.remoteId, otherUser)
      }
      service.startCall(_1t1Conv.id)
      result(checkpoint1.head)
      result(checkpoint2.head)
    }

    scenario("Group Team conversation treated as group call") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group, Some(TeamId()))

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (members.getByConvs _).expects(Set(groupConv.id)).once().returning(Future.successful(IndexedSeq(groupMember1, groupMember2, self).map(u => ConversationMemberData(u, groupConv.id))))

      val service = initCallingService()

      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfCalling && cur.caller == self && cur.others == Set(self) && cur.isGroup))
      val checkpoint2 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfJoining && cur.caller == self && cur.others == Set(self) && cur.isGroup))
      val checkpoint3 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(groupMember1) && cur.isGroup))
      val checkpoint4 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfConnected && cur.caller == self && cur.others == Set(groupMember1, groupMember2) && cur.isGroup))

      (avs.startCall _).expects(*, *, *).once().returning(Future.successful(0))

      service.startCall(groupConv.id)
      result(checkpoint1.head)

      service.onOtherSideAnsweredCall(groupConv.remoteId)
      result(checkpoint2.head)

      //TODO which user from a group conversation gets passed down here?
      service.onEstablishedCall(groupConv.remoteId, groupMember1)
      println(result(service.currentCall.map(_.get.others).head))
      result(checkpoint3.head)

      service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      result(checkpoint4.head)
    }
  }

  feature("Ending calls") {
    scenario("Leave group call that will continue running in the background") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(_.state == SelfConnected))
      val checkpoint2 = callCheckpoint(service, _.contains(groupConv.id), _.isEmpty)

      service.onIncomingCall(groupConv.remoteId, groupMember1, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(*, *).once().onCall { (_, _) =>
        service.onEstablishedCall(groupConv.remoteId, groupMember1)
        service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      }
      service.startCall(groupConv.id)
      result(checkpoint1.head)

      (avs.endCall _).expects(*, *).once().onCall { (rId, isGroup) =>
        service.onClosedCall(StillOngoing, groupConv.remoteId, Instant.now, groupMember1)
      }
      service.endCall(groupConv.id) //avs won't call the ClosedHandler if a group call is still ongoing in the background
      result(checkpoint2.head)
    }

    scenario("Incoming group call answered on another device") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(_.state == OtherCalling))
      val checkpoint2 = callCheckpoint(service, _.isEmpty, _.isEmpty)

      service.onIncomingCall(groupConv.remoteId, groupMember1, videoCall = false, shouldRing = true)
      result(checkpoint1.head)

      service.onClosedCall(AnsweredElsewhere, groupConv.remoteId, Instant.now, groupMember1)
      result(checkpoint2.head)
    }

    scenario("Reject incoming 1:1 call should remove it from activeCall, and then also from backgroundCalls after timeout") {
      val otherUser = UserId("otherUser")
      val _1t1Conv = ConversationData(ConvId(), RConvId(), Some("1:1 Conv"), self, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(_1t1Conv)))

      val service = initCallingService()
      val checkpoint1 = callCheckpoint(service, _.contains(_1t1Conv.id), _.exists(cur => cur.convId == _1t1Conv.id && cur.state == OtherCalling))
      val checkpoint2 = callCheckpoint(service, _.contains(_1t1Conv.id), _.isEmpty)
      val checkpoint3 = callCheckpoint(service, _.isEmpty, _.isEmpty)

      service.onIncomingCall(_1t1Conv.remoteId, otherUser, videoCall = false, shouldRing = true)
      result(checkpoint1.head)

      (avs.rejectCall _).expects(*, *).once().onCall { (_, _) =>
        service.onClosedCall(StillOngoing, _1t1Conv.remoteId, Instant.now, otherUser)
      }
      service.endCall(_1t1Conv.id)
      result(checkpoint2.head)

      service.onClosedCall(Normal, _1t1Conv.remoteId, Instant.now, otherUser)
      result(checkpoint3.head)
    }

    scenario("Cancel outgoing group call should remove it from background and active calls") {
      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))
      (convs.convById _).expects(*).anyNumberOfTimes().returning(Future.successful(Some(groupConv)))

      val service = initCallingService()

      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.exists(cur => cur.convId == groupConv.id && cur.state == SelfCalling && cur.caller == self && cur.others == Set(self)))
      val checkpoint2 = callCheckpoint(service, _.isEmpty, _.isEmpty)

      (avs.startCall _).expects(*, *, *).once().returning(Future.successful(0))
      service.startCall(groupConv.id)
      result(checkpoint1.head)

      (avs.endCall _).expects(groupConv.remoteId, true).once().onCall { (_, _) =>
        service.onClosedCall(Normal, groupConv.remoteId, Instant.now, groupMember1)
      }
      service.endCall(groupConv.id)
      result(checkpoint2.head)
    }
  }

  feature("Simultaneous calls") {

    scenario("Receive incoming call while 1:1 call ongoing") {

      val ongoingUserId = UserId()
      val ongoingConv = ConversationData(ConvId(ongoingUserId.str), RConvId(ongoingUserId.str), Some("Ongoing Conv"), self, ConversationType.OneToOne)

      val incomingUserId = UserId()
      val incomingConv = ConversationData(ConvId(incomingUserId.str), RConvId(incomingUserId.str), Some("Incoming Conv"), self, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().onCall { rConvId: RConvId =>
        Future.successful(rConvId match {
          case ongoingConv.remoteId => Some(ongoingConv)
          case incomingConv.remoteId => Some(incomingConv)
          case _ => None
        })
      }
      (convs.convById _).expects(ConvId(ongoingUserId.str)).anyNumberOfTimes().returning(Future.successful(Some(ongoingConv)))

      val service = initCallingService()

      val checkpoint = callCheckpoint(service, _.contains(ongoingConv.id), cur => cur.exists(_.state == SelfConnected) && cur.exists(_.others.contains(ongoingUserId)))

      service.onIncomingCall(ongoingConv.remoteId, ongoingUserId, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(*, *).once().onCall { (_, _) =>
        service.onEstablishedCall(ongoingConv.remoteId, ongoingUserId)
      }
      service.startCall(ongoingConv.id)
      service.onIncomingCall(incomingConv.remoteId, incomingUserId, videoCall = false, shouldRing = false) //Receive the second call after first is established

      result(checkpoint.head)
    }

    scenario("With a background group call, receive a 1:1 call, finish it, and then still join the group call afterwards") {

      val groupMember1 = UserId("groupUser1")
      val groupMember2 = UserId("groupUser2")
      val groupConv = ConversationData(ConvId(), RConvId(), Some("Group Conv"), self, ConversationType.Group)

      val otoUser = UserId("otoUser")
      val otoConv = ConversationData(ConvId(otoUser.str), RConvId(otoUser.str), Some("1:1 Conv"), self, ConversationType.OneToOne)

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

      (avs.setVideoSendActive _).expects(otoConv.remoteId, false).anyNumberOfTimes()
      val service = initCallingService()

      //Checkpoint 1: Receive and reject a group call
      val checkpoint1 = callCheckpoint(service, _.contains(groupConv.id), _.isEmpty)

      service.onIncomingCall(groupConv.remoteId, groupMember1, videoCall = false, shouldRing = true)
      (avs.rejectCall _).expects(*, *).anyNumberOfTimes().onCall { (_, _) =>
        service.onClosedCall(StillOngoing, groupConv.remoteId, Instant.now, groupMember1)
      }
      service.endCall(groupConv.id) //user rejects the group call

      result(checkpoint1.head)

      //Checkpoint 2: Receive and accept a 1:1 call
      val checkpoint2 = callCheckpoint(service,
        act => act.contains(groupConv.id) && act.contains(otoConv.id),
        _.exists(curr => curr.others.contains(otoUser) && curr.state == SelfConnected))

      service.onIncomingCall(otoConv.remoteId, otoUser, videoCall = false, shouldRing = true)
      (avs.answerCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onEstablishedCall(otoConv.remoteId, otoUser)
      }
      service.startCall(otoConv.id) //user accepts 1:1 call

      result(checkpoint2.head)

      val checkpoint3 = callCheckpoint(service, _.contains(groupConv.id), _.exists(curr => curr.others == Set(groupMember1, groupMember2) && curr.state == SelfConnected))

      (avs.endCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onClosedCall(Normal, otoConv.remoteId, Instant.now, otoUser)
      }
      service.endCall(otoConv.id)

      (avs.answerCall _).expects(*, *).once().onCall { (rId, _) =>
        service.onEstablishedCall(groupConv.remoteId, groupMember1)
        service.onGroupChanged(groupConv.remoteId, Set(groupMember1, groupMember2))
      }
      service.startCall(groupConv.id)

      result(checkpoint3.head)
    }
  }

  def callCheckpoint(service: CallingService, activeCheck: Map[ConvId, CallInfo] => Boolean, currentCheck: Option[CallInfo] => Boolean) =
    (for {
      active <- service.availableCalls
      current <- service.currentCall
    } yield (active, current)).filter { case (active, current) =>
      activeCheck(active) && currentCheck(current)
    }

  def signalTest[A](signal: Signal[A])(test: A => Boolean)(trigger: => Unit): Unit = {
    signal.disableAutowiring()
    trigger
    result(signal.filter(test).head)
  }

  def initCallingService() = {
    val initPromise = Promise[Unit]()
    (context.startService _).expects(*).anyNumberOfTimes().returning(true)
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
    val service = new CallingService(context, account, self, avs, convs, members, null, flows, messages, media, null, callLogService, network, null)
    result(initPromise.future)
    service
  }
}
