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
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.EventContext
import com.waz.utils.events.Signal
import com.waz.znet.{ContentEncoder, HttpClient, StringResponse, WireWebSocket}

import scala.concurrent.Future
import scala.concurrent.duration._

class WebSocketClientServiceSpec extends AndroidFreeSpec {

  import Threading.Implicits.Background
  import EventContext.Implicits.global

  val accountId  = AccountId("account")
  val clientId   = ClientId("client")
  val lifeCycle  = mock[ZmsLifeCycle]
  val httpClient = mock[HttpClient]
  val network    = mock[NetworkModeService]
  val token      = mock[PushTokenService]

  val accLoggedIn = Signal(false)
  val idle        = Signal(false)
  val networkMode = Signal(NetworkMode.WIFI)
  val pushActive  = Signal(false)

  scenario("Open websocket on account in active state and receive message") {

    accLoggedIn ! true
    val service = getService

    val client = result(service.client.filter(_.contains(testWebSocket)).head).get

    testWebSocket.connected ! true
    result(service.connected.filter(_ == true).head)

    val res = client.onMessage.next
    testWebSocket.onMessage ! StringResponse("test")
    result(res).asInstanceOf[StringResponse].value shouldEqual "test"
  }

  scenario("Account goes into background with push active should disable websocket, coming back should re-enable") {
    WebSocketClientService.inactivityTimeout = 0.seconds

    pushActive ! true
    accLoggedIn ! true
    val service = getService

    testWebSocket.connected ! true
    result(service.client.filter(_.contains(testWebSocket)).head)
    result(service.connected.filter(_ == true).head)


    accLoggedIn ! false
    result(service.client.filter(_.isEmpty).head)
    result(service.connected.filter(_ == false).head)
    testWebSocket.closeCount shouldEqual 1

    accLoggedIn ! true
    testWebSocket.connected ! true
    result(service.client.filter(_.contains(testWebSocket)).head)
    result(service.connected.filter(_ == true).head)
  }

  val testWebSocket = new WireWebSocket {
    var closeCount = 0
    override def pingPong() = ???
    override def close() = Future.successful(closeCount += 1)
    override def send[A: ContentEncoder](msg: A) = ???
  }

  def getService = {
    (httpClient.websocket _).expects(accountId, webSocketUri(clientId, BackendConfig.StagingBackend), null).anyNumberOfTimes().returning(CancellableFuture.successful(testWebSocket))
    (lifeCycle.idle _).expects().anyNumberOfTimes().returning(idle)
    (lifeCycle.accLoggedIn _).expects(accountId).anyNumberOfTimes().returning(accLoggedIn)
    (network.networkMode _).expects().anyNumberOfTimes().returning(networkMode)
    (token.pushActive _).expects().anyNumberOfTimes().returning(pushActive)
    new WebSocketClientServiceImpl(null, accountId, lifeCycle, httpClient, null, network, BackendConfig.StagingBackend, clientId, token)
  }

}
