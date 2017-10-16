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

import com.waz.api.NetworkMode
import com.waz.model.AccountId
import com.waz.model.otr.ClientId
import com.waz.service.push.WebSocketClientService.webSocketUri
import com.waz.service.{BackendConfig, NetworkModeService, ZmsLifeCycle}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.CancellableFuture
import com.waz.utils.events.Signal
import com.waz.znet.{ContentEncoder, HttpClient, WireWebSocket}

class WebSocketClientServiceSpec extends AndroidFreeSpec {


  val account    = AccountId("account")
  val client     = ClientId("client")
  val lifeCycle  = mock[ZmsLifeCycle]
  val httpClient = mock[HttpClient]
  val network    = mock[NetworkModeService]
  val token      = mock[PushTokenService]


  val accLoggedIn = Signal(false)
  val idle = Signal(false)
  val networkMode = Signal(NetworkMode.WIFI)
  val pushActive = Signal(false)

  scenario("Open websocket on app in active state") {

    (httpClient.websocket _).expects(account, webSocketUri(client, BackendConfig.StagingBackend), null).returning(CancellableFuture.successful(testWebSocket("1")))


    val service = getService

    println(result(service.client.head))


  }

  def testWebSocket(id: String) = new WireWebSocket {

    override def onError = ???

    override def connected = ???

    override def pingPong() = ???

    override def lastReceivedTime = ???

    override def onMessage = ???

    override def close() = ???

    override def send[A: ContentEncoder](msg: A) = ???

    override def onConnectionLost = ???
  }

  def getService = {

    (lifeCycle.idle _).expects().anyNumberOfTimes().returning(idle)
    (lifeCycle.accLoggedIn _).expects(account).anyNumberOfTimes().returning(accLoggedIn)
    (network.networkMode _).expects().anyNumberOfTimes().returning(networkMode)
    (token.pushActive _).expects().anyNumberOfTimes().returning(pushActive)

    new WebSocketClientServiceImpl(null, account, lifeCycle, httpClient, null, network, BackendConfig.StagingBackend, client, token)
  }

}
