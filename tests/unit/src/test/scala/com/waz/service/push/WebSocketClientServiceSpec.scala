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
package com.waz.service.push

import com.waz.RobolectricUtils
import com.waz.model.otr.ClientId
import com.waz.service._
import com.waz.testutils.DefaultPatienceConfig
import com.waz.utils.events.EventContext.Implicits.global
import com.waz.znet.WebSocketClient
import com.waz.znet.ZNetClient.EmptyClient
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._

class WebSocketClientServiceSpec extends FeatureSpec with Matchers with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig {

  val timeout = 250.millis
  val timeouts = new Timeouts {
    override val webSocket: WebSocket = new WebSocket {
      override def inactivityTimeout: Timeout = timeout
      override def connectionTimeout: Timeout = timeout
    }
  }

  lazy val lifecycle = new ZmsLifecycle
  lazy val network = new NetworkModeService(context)

  lazy val service = new WebSocketClientService(lifecycle, new EmptyClient, network, BackendConfig.EdgeBackend, ClientId(), timeouts)


  feature("active client") {
    var client = Option.empty[WebSocketClient]

    scenario("client is created when id is set and lifecycle is active") {
      service.client { client = _ }

      lifecycle.lifecycleState ! LifecycleState.Active

      client shouldBe 'defined
    }

    scenario("client is not destroyed if lifecycle is paused for short time") {
      lifecycle.lifecycleState ! LifecycleState.Idle

      awaitUi(50.millis)
      client shouldBe 'defined

      lifecycle.lifecycleState ! LifecycleState.Active
      awaitUi(250.millis)
      client shouldBe 'defined
    }

    scenario("client is destroyed after delay when lifecycle is paused") {
      lifecycle.lifecycleState ! LifecycleState.Idle

      awaitUi(50.millis)
      client shouldBe 'defined

      withDelay {
        client shouldBe empty
      }
    }
  }

  feature("Connection error signaling") {

    @volatile var error = false

    scenario("report error when client stays unconnected for 3 seconds") {
      service.connectionError { error = _ }
      lifecycle.lifecycleState ! LifecycleState.UiActive

      service.wsActive.currentValue shouldBe 'defined
      awaitUi(50.millis)

      error shouldEqual false

      withDelay {
        service.connectionError.currentValue shouldEqual Some(true)
        error shouldEqual true
      }
    }
  }
}
