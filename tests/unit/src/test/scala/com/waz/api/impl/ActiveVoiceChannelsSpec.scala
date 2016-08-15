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
package com.waz.api.impl

import com.waz.RobolectricUtils
import com.waz.api.CauseForCallStateEvent
import com.waz.model.ConversationData.ConversationType
import com.waz.model.VoiceChannelData.ChannelState
import com.waz.model._
import com.waz.testutils.{MockUiModule, MockZMessaging, UpdateSpy}
import com.waz.threading.Threading
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class ActiveVoiceChannelsSpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with OptionValues with RobolectricTests with RobolectricUtils {

  lazy val selfId = UserId()
  lazy val zmessaging = new MockZMessaging(selfUserId = selfId)
  implicit lazy val ui = new MockUiModule(zmessaging)

  val conv = ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    ui.onCreate(testContext)
    ui.onResume()

    Await.ready(zmessaging.convsStorage.insert(conv), 5.seconds)
  }

  private implicit lazy val dispatcher = Threading.Background

  lazy val channels = new ActiveVoiceChannels
  lazy val spy = UpdateSpy(channels)

  feature("Incoming calls") {
    scenario("Add voice channel on incoming call") {
      channels.hasIncomingCall shouldEqual false
      channels.hasOngoingCall shouldEqual false

      val user = UserId()
      spy.reset()
      push(user, joined = true)
      withDelay {
        channels.hasIncomingCall shouldEqual true
        channels.hasOngoingCall shouldEqual false
        spy.numberOfTimesCalled should be > 0
      }

      val incoming = Option(channels.getIncomingCall).value
      withDelay {
        incoming.getState shouldEqual ChannelState.OtherCalling
        incoming.getConversation.getId shouldEqual conv.id.str
        incoming.isVideoCall shouldEqual true
      }

      spy.reset()
      push(user, joined = false)

      withDelay {
        channels.hasIncomingCall shouldEqual false
        channels.hasOngoingCall shouldEqual false
        spy.numberOfTimesCalled should be > 0
      }
    }
  }

  private def push(user: UserId, joined: Boolean): Unit =
    zmessaging.eventPipeline(Vector(CallStateEvent(Uid(), conv.remoteId, Some(Set(CallParticipant(selfId, joined = false, Set.empty), CallParticipant(user, joined = joined, Set(CallProperty.SendsVideo)))), device = None, CauseForCallStateEvent.REQUESTED).withCurrentLocalTime()))
}
