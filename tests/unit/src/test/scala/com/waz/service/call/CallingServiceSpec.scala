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

import com.waz.ZLog._
import com.waz.api.{NetworkMode, VoiceChannelState}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{UserId, _}
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.{MediaManagerService, NetworkModeService}
import com.waz.utils.events.Signal
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.Future

class CallingServiceSpec extends FeatureSpec with Matchers with MockFactory {

  private implicit val tag: LogTag = logTagFor[CallingServiceSpec]

  val avsMock = mock[AvsV3]
  val flows   = mock[FlowManagerService]
  val mm      = mock[MediaManagerService]
  val network = mock[NetworkModeService]
  val convs   = mock[ConversationsContentUpdater]


  lazy val selfUser = UserData("self user")
  lazy val user1 = UserData("Johnny Cash")
  lazy val conv1 = ConversationData(ConvId(user1.id.str), RConvId(), Some("convName"), selfUser.id, ConversationType.Group)


  feature("Group tests with features") {
    scenario("CallingService intialization") {
      (avsMock.available _).expects().once().returning(Future.successful({}))
      (flows.flowManager _).expects().once().returning(None)
      (mm.mediaManager _).expects().once().returning(None)
      (network.networkMode _).expects().once().returning(Signal.empty[NetworkMode])
      (avsMock.init _).expects(*).once().onCall{ service: CallingService =>
        println("Init was called, hurray!")
        Future.successful({})
      }
      new DefaultCallingService(null, UserId(), avsMock, null, null, null, flows, null, mm, null, null, network, null)
    }

    scenario("Incoming call") {
      (avsMock.available _).expects().once().returning(Future.successful({}))
      (flows.flowManager _).expects().once().returning(None)
      (mm.mediaManager _).expects().once().returning(None)
      (network.networkMode _).expects().once().returning(Signal.empty[NetworkMode])
      (convs.convByRemoteId _).expects(*).once().returning(Future.successful(Some(conv1)))

      (avsMock.init _).expects(*).once().onCall{ service: CallingService =>
        service.currentCall.disableAutowiring()

        service.currentCall.currentValue.get.state shouldEqual VoiceChannelState.NO_ACTIVE_USERS

        service.onIncomingCall(conv1.remoteId, user1.id, false, true)
        Thread.sleep(50)
        service.currentCall.currentValue.get.state shouldEqual VoiceChannelState.OTHER_CALLING
        Future.successful({})
      }

      new DefaultCallingService(null, UserId(), avsMock, convs, null, null, flows, null, mm, null, null, network, null)
    }

  }
}
