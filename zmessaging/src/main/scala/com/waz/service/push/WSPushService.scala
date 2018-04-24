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

import java.util.concurrent.atomic.AtomicInteger

import android.net.Uri
import com.waz.ZLog._
import com.waz.api.ErrorResponse
import com.waz.model.UserId
import com.waz.model.otr.ClientId
import com.waz.service.ZMessaging.accountTag
import com.waz.service.push.WSPushServiceImpl.RequestCreator
import com.waz.service.{AccountContext, BackendConfig, NetworkModeService}
import com.waz.sync.client.PushNotificationEncoded
import com.waz.sync.client.PushNotificationsClient.NotificationsResponseEncoded
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events._
import com.waz.utils.{Backoff, ExponentialBackoff}
import com.waz.znet.AuthenticationManager.AccessToken
import com.waz.znet.OkHttpWebSocket.SocketEvent
import com.waz.znet._
import okhttp3.OkHttpClient

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Left

trait WSPushService {
  def activate(): Unit
  def deactivate(): Unit
  def notifications(): EventStream[Seq[PushNotificationEncoded]]
  def connected(): Signal[Boolean]
  def awaitActive(): Future[Unit]
}

object WSPushServiceImpl {

  type RequestCreator = AccessToken => okhttp3.Request

  def apply(userId: UserId,
            clientId: ClientId,
            backend: BackendConfig,
            accessTokenProvider: AccessTokenProvider,
            networkModeService: NetworkModeService,
            ev: AccountContext): WSPushServiceImpl = {

    val requestCreator = (token: AccessToken) => {
      val requestBuilder = new okhttp3.Request.Builder()
      val url = Uri.parse(backend.websocketUrl).buildUpon().appendQueryParameter("client", clientId.str).build().toString
      token.headers.toList.foreach { header => requestBuilder.addHeader(header._1, header._2) }

      requestBuilder
        .url(url)
        .addHeader("Accept-Encoding", "identity") // XXX: this is a hack for Backend In The Box problem: 'Accept-Encoding: gzip' header causes 500
        .addHeader(AsyncClient.UserAgentHeader, AsyncClient.userAgent())
        .get()
        .build()
    }

    new WSPushServiceImpl(
      userId,
      accessTokenProvider,
      requestCreator,
      ExponentialBackoff.standardBackoff,
      networkModeService
    )(ev)
  }

  protected sealed trait WebSocketProcessEvent
  protected case object SocketOpened extends WebSocketProcessEvent
  protected case class SocketClosed(error: Option[Throwable] = None) extends WebSocketProcessEvent
  protected case class AccessTokenError(errorResponse: ErrorResponse) extends WebSocketProcessEvent
  protected case class Message(content: ResponseContent) extends WebSocketProcessEvent
}

class WSPushServiceImpl(userId:              UserId,
                        accessTokenProvider: AccessTokenProvider,
                        requestCreator:      RequestCreator,
                        backoff:             Backoff = ExponentialBackoff.standardBackoff,
                        networkModeService:  NetworkModeService)
                       (implicit ev: AccountContext) extends WSPushService {

  import WSPushServiceImpl._

  private implicit val logTag: LogTag = accountTag[WSPushServiceImpl](userId)
  private implicit val dispatcher: SerialDispatchQueue = new SerialDispatchQueue(name = "WSPushServiceImpl")

  override val notifications: SourceStream[Seq[PushNotificationEncoded]] = EventStream()
  override val connected: SourceSignal[Boolean] = Signal(false)

  override def awaitActive(): Future[Unit] = connected.filter(identity).map(_ => ()).head

  private val okHttpClient = new OkHttpClient()

  private val activated: SourceSignal[Boolean] = Signal(false)
  override def activate(): Unit = activated ! true
  override def deactivate(): Unit = activated ! false

  private var currentWebSocketSubscription: Subscription = _

  activated {
    case false  =>
      debug("WebSocket will be deactivated")
      finishWebSocketProcess()
    case true   =>
      debug("WebSocket will be activated")
      restartWebSocketProcess()
  }

  private val retryCount = new AtomicInteger(0)

  private def finishWebSocketProcess(): Unit = if (currentWebSocketSubscription != null) {
    currentWebSocketSubscription.destroy()
    currentWebSocketSubscription = null
    connected ! false
  }

  private def restartWebSocketProcess(initialDelay: FiniteDuration = 0.seconds): Unit = {
    finishWebSocketProcess()
    currentWebSocketSubscription = webSocketProcessEngine(initialDelay)
  }

  private def webSocketProcessEngine(initialDelay: FiniteDuration): Subscription = {
    val events: Signal[WebSocketProcessEvent] = for {
      _ <- Signal.future(CancellableFuture.delay(initialDelay))
      _ = info(s"Opening WebSocket... ${if (retryCount.get() == 0) "" else s"Retry count: ${retryCount.get()}"}")
      accessTokenResult <- Signal.future(accessTokenProvider.currentToken())
      event <- accessTokenResult match {
        case Right(token) =>
          Signal.wrap(OkHttpWebSocket.socketEvents(okHttpClient, requestCreator(token)).map {
            case SocketEvent.Message(_, content) => Message(content)
            case SocketEvent.Closed(_, error) => SocketClosed(error)
            case SocketEvent.Opened(_, _) => SocketOpened
          })
        case Left(errorResponse) =>
          Signal.const(AccessTokenError(errorResponse))
      }
    } yield event

    events {
      case SocketOpened =>
        info("WebSocket opened")
        connected ! true
        retryCount.set(0)
      case SocketClosed(Some(error)) =>
        info(s"WebSocket closed with error: $error")
        restartWebSocketProcess(initialDelay = backoff.delay(retryCount.incrementAndGet()))
      case SocketClosed(_) =>
        info(s"WebSocket closed")
        restartWebSocketProcess(initialDelay = backoff.delay(retryCount.incrementAndGet()))
      case AccessTokenError(errorResponse) =>
        info(s"Error while access token receiving: $errorResponse")
        restartWebSocketProcess(initialDelay = backoff.delay(retryCount.incrementAndGet()))
      case Message(NotificationsResponseEncoded(notifs @ _*)) =>
        info("Push notifications received")
        notifications ! notifs
      case Message(_) =>
        error("Unknown message received")
    }
  }

}

//trait WireWebSocket {
//  def connected: Signal[Boolean]
//  def lastReceivedTime: Signal[Instant] // time when something was last received on websocket
//  def onError: EventStream[Throwable]
//  def onMessage: EventStream[ResponseContent]
//  def close(): Unit
//}
