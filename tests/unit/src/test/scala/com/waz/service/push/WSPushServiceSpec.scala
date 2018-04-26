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

import java.net.URL

import com.waz.ZLog.ImplicitTag._
import com.waz.api.impl.ErrorResponse
import com.waz.model.UserId
import com.waz.service.push.WSPushServiceImpl.RequestCreator
import com.waz.specs.ZMockSpec
import com.waz.utils.{Backoff, ExponentialBackoff}
import com.waz.utils.events.{EventContext, EventStream, SourceStream}
import com.waz.znet.AuthenticationManager.AccessToken
import com.waz.znet.WebSocketFactory.SocketEvent
import com.waz.znet._

import scala.concurrent.Future

class WSPushServiceSpec extends ZMockSpec {

  import com.waz.utils.events.EventContext.Implicits.global

  private val accessTokenProvider = mock[AccessTokenProvider]
  private val webSocketFactory = mock[WebSocketFactory]
  private val webSocket = mock[WebSocket]

  private val accessTokenSuccess = Future.successful(Right(AccessToken("token", "type")))
  private val accessTokenError = Future.successful(Left(ErrorResponse.InternalError))
  private val httpRequest = HttpRequest2Impl(new URL("http://www.test.com"))

  private val fakeWebSocketEvents: SourceStream[SocketEvent] = EventStream()

  private def createWSPushService(userId:              UserId = UserId("userId"),
                                  accessTokenProvider: AccessTokenProvider = accessTokenProvider,
                                  requestCreator:      RequestCreator = _ => httpRequest,
                                  webSocketFactory:    WebSocketFactory = webSocketFactory,
                                  backoff:             Backoff = ExponentialBackoff.zeroBackoff(10)) = {
    new WSPushServiceImpl(userId, accessTokenProvider, requestCreator, webSocketFactory, backoff)
  }

  feature("WSPushService") {

    scenario("On activation should get access token and become connected. On deactivation become disconnected immediately.") {
      (accessTokenProvider.currentToken _).expects().once().returning(accessTokenSuccess)
      (webSocketFactory.openWebSocket(_: HttpRequest2)(_: EventContext))
        .expects(httpRequest, *).once().returning(fakeWebSocketEvents)

      val service = createWSPushService()
      service.activate()

      Thread.sleep(1000)
      fakeWebSocketEvents ! SocketEvent.Opened(webSocket)

      noException shouldBe thrownBy { await(service.connected.filter(identity).head) }
      service.deactivate()

      service.connected.currentValue shouldBe Some(false)
    }

    scenario("When can not get an access token should retry to connect.") {
      val accessTokenResults = List(accessTokenError, accessTokenSuccess).toIterator
      (accessTokenProvider.currentToken _).expects().twice().onCall(() => accessTokenResults.next())
      (webSocketFactory.openWebSocket(_: HttpRequest2)(_: EventContext))
        .expects(httpRequest, *).once().returning(fakeWebSocketEvents)

      val service = createWSPushService()
      service.activate()

      Thread.sleep(1000)
      fakeWebSocketEvents ! SocketEvent.Opened(webSocket)

      noException shouldBe thrownBy { await(service.connected.filter(identity).head) }
    }

    scenario("When web socket closed should retry to connect.") {
      (accessTokenProvider.currentToken _).expects().twice().returning(accessTokenSuccess)
      (webSocketFactory.openWebSocket(_: HttpRequest2)(_: EventContext))
        .expects(httpRequest, *).twice().returning(fakeWebSocketEvents)

      val service = createWSPushService()
      service.activate()

      Thread.sleep(500)
      fakeWebSocketEvents ! SocketEvent.Opened(webSocket)

      Thread.sleep(500)
      fakeWebSocketEvents ! SocketEvent.Closed(webSocket, Some(new InterruptedException))

      Thread.sleep(500)
      fakeWebSocketEvents ! SocketEvent.Opened(webSocket)

      noException shouldBe thrownBy { await(service.connected.filter(identity).head) }
    }

    scenario("When web socket is going to be closed by other side should close web socket with normal closure code.") {
      (accessTokenProvider.currentToken _).expects().once().returning(accessTokenSuccess)
      (webSocketFactory.openWebSocket(_: HttpRequest2)(_: EventContext))
        .expects(httpRequest, *).once().returning(fakeWebSocketEvents)

      val service = createWSPushService()
      service.activate()

      Thread.sleep(500)
      fakeWebSocketEvents ! SocketEvent.Opened(webSocket)

      (webSocket.close _).expects(WebSocket.CloseCodes.NormalClosure, *).once().returning(true)

      Thread.sleep(500)
      fakeWebSocketEvents ! SocketEvent.Closing(webSocket, 0, null)
    }


  }

}
