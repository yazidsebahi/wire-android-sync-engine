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
package com.waz.tracking

import akka.pattern.ask
import com.waz.api.NotificationsHandler.NotificationsHandlerFactory
import com.waz.api.{CallingEventsHandler, _}
import com.waz.model.VoiceChannelData.ChannelState
import com.waz.model.otr.ClientId
import com.waz.provision.ActorMessage.{Login, Successful, _}
import com.waz.service
import com.waz.service._
import com.waz.service.call.AvsMetrics
import com.waz.testutils.CallJoinSpy
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.{FeatureSpec, Matchers, OptionValues}

import scala.concurrent.Promise
import scala.concurrent.duration._

class TrackingEventsSpec extends FeatureSpec with Matchers with OptionValues with ProvisionedApiSpec with ThreadActorSpec { test =>
  import com.waz.threading.Threading.Implicits.Background
  override val provisionFile = "/two_users_connected.json"

  lazy val convs = api.getConversations
  lazy val user = api.getSelf
  lazy val channels = api.getActiveVoiceChannels
  lazy val tracking = api.zmessaging.map(_.map(_.trackingEvents))

  val spy = new CallJoinSpy
  val handler = new MockEventsHandler

  override lazy val zmessagingFactory = new ZMessagingFactory(globalModule) {

    override def zmessaging(clientId: ClientId, user: UserModule): service.ZMessaging =
      new ApiZMessaging(clientId, user) {

        override def handlerFactory: NotificationsHandlerFactory = new NotificationsHandlerFactory {
          override def getCallingEventsHandler: CallingEventsHandler = ???
          override def getTrackingEventsHandler: TrackingEventsHandler = handler
        }
      }
  }

  lazy val auto2 = registerDevice("VoiceChannelSpec_auto2")

  var callConnectingTimeout = 1.minute

  override lazy val timeouts: Timeouts = new Timeouts {
    override val calling: Calling = new Calling {
      override def callConnectingTimeout = test.callConnectingTimeout
    }
  }

  feature("Avs Metrics") {
    scenario("Pass AVS metrics to UI") {
      spy.reset()
      callConnectingTimeout = 10.seconds
      zmessaging.prefs.analyticsEnabledPref := true

      try {
        auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
        val userId = concurrent.Await.result(auto2 ? GetUser, 5.seconds).asInstanceOf[Successful].response

        withDelay(convs should not be empty)
        val conv = convs.find(_.getId == userId).get
        val channel = conv.getVoiceChannel
        channel.getState shouldEqual ChannelState.Idle
        channel.join(spy.joinCallback)

        auto2 ? AcceptCall should eventually(be(Successful))

        withDelay {
          channel.getState shouldEqual ChannelState.DeviceConnected
        }

        channel.leave()

        val metrics = handler.avsMetrics.future.await("")
        metrics.convId should equal(conv.data.remoteId.str)
        metrics.json.length() should be > 0
      } finally {
        callConnectingTimeout = 1.minute
      }
    }
  }

}

class MockEventsHandler extends TrackingEventsHandler {

  val avsMetrics = Promise[AvsMetrics]()

  override def onTrackingEvent(event: TrackingEvent): Unit = ???

  override def onAvsMetricsEvent(avsMetrics: AvsMetrics): Unit = {
    println(s"onAvsMetricsEvent: $avsMetrics")
    this.avsMetrics.success(avsMetrics)
  }
}
