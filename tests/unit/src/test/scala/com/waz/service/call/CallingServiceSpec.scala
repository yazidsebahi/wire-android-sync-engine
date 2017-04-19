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
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.{MediaManagerService, NetworkModeService}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Context
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

class CallingServiceSpec extends FeatureSpec with Matchers with MockFactory with BeforeAndAfterAll with AndroidFreeSpec {

  val defaultDuration = 5.seconds
  implicit val eventContext = EventContext.Implicits.global
  implicit val executionContext = new SerialDispatchQueue(name = "CallingServiceSpec")

  val context = mock[Context]
  val avsMock = mock[AvsV3]
  val flows   = mock[FlowManagerService]
  val mm      = mock[MediaManagerService]
  val network = mock[NetworkModeService]
  val convs   = mock[ConversationsContentUpdater]
  val members = mock[MembersStorage]

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
        service.startCall(ConvId(groupId1.str), isVideo = false, isGroup = true)
      }
    }
  }

  feature("Simultaneous calls") {

    scenario("Receive incoming call while 1:1 call ongoing") {

      val ongoingUserId = UserId("user1")
      val ongoingConv = ConversationData(ConvId(ongoingUserId.str), RConvId(ongoingUserId.str), Some("Ongoing Conv"), selfUser.id, ConversationType.OneToOne)

      val incomingUserId = UserId("user2")
      val incomingConv = ConversationData(ConvId(incomingUserId.str), RConvId(incomingUserId.str), Some("Incoming Conv"), selfUser.id, ConversationType.OneToOne)

      (convs.convByRemoteId _).expects(*).anyNumberOfTimes().onCall { rConvId: RConvId =>
        Future.successful(rConvId match {
          case ongoingConv.remoteId => Some(ongoingConv)
          case incomingConv.remoteId => Some(incomingConv)
          case _ => None
        })
      }
      (convs.convById _).expects(ConvId(ongoingUserId.str)).once().returning(Future.successful(Some(ongoingConv)))
      (avsMock.answerCall _).expects(ongoingConv.remoteId, false).once()
      (avsMock.setVideoSendActive _).expects(ongoingConv.remoteId, false).once()

      val service = initCallingService()

      signalTest(service.currentCall) { info =>
        info.state == SELF_CONNECTED &&
          info.others.contains(ongoingUserId) &&
          !info.others.contains(incomingUserId)
      } {
        service.onIncomingCall(ongoingConv.remoteId, ongoingUserId, videoCall = false, shouldRing = true)
        service.acceptCall(ongoingConv.id, isGroup = false)
        service.onEstablishedCall(ongoingConv.remoteId, ongoingUserId)
        //Receive the second call after first is established
        service.onIncomingCall(incomingConv.remoteId, incomingUserId, videoCall = false, shouldRing = false)
      }
    }
  }

  def signalTest[A](signal: Signal[A])(test: A => Boolean)(trigger: => Unit): Unit = {
    signal.disableAutowiring()
    trigger
    Await.ready(signal.filter(test).head, defaultDuration)
  }

  def initCallingService(context: Context                     = context,
                         self:    UserId                      = UserId(),
                         avs:     AvsV3                       = avsMock,
                         convs:   ConversationsContentUpdater = convs,
                         members: MembersStorage              = members,
                         flows:   FlowManagerService          = flows,
                         media:   MediaManagerService         = mm,
                         network: NetworkModeService          = network) = {
    val initPromise = Promise[Unit]()
    (context.startService _).expects(*).anyNumberOfTimes().returning(null)
    (avs.available _).expects().once().returning(Future.successful({}))
    (flows.flowManager _).expects().once().returning(None)
    (media.mediaManager _).expects().once().returning(None)
    (network.networkMode _).expects().once().returning(Signal.empty[NetworkMode])
    (avs.init _).expects(*).once().onCall{ service: CallingService =>
      initPromise.success({})
      initPromise.future
    }
    val service = new DefaultCallingService(context, self, avs, convs, members, null, flows, null, media, null, null, network, null)
    Await.ready(initPromise.future, defaultDuration)
    service
  }
}
