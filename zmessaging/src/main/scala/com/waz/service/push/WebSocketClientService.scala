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

import android.content.Context
import android.net.Uri
import com.koushikdutta.async.http.WebSocket
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.model.AccountId
import com.waz.model.otr.ClientId
import com.waz.service._
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.utils.returning
import com.waz.utils.wrappers.URI
import com.waz.znet.WebSocketClient.{Disconnect, defaultBackoff}
import com.waz.znet._
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal

trait WebSocketClientService {
  def network:              NetworkModeService
  def useWebSocketFallback: Signal[Boolean]
  def wsActive:             Signal[Boolean]
  def client:               Signal[Option[WireWebSocket]]
  def connected:            Signal[Boolean]
  def connectionError:      Signal[Boolean]
  def connectionStats:      Signal[WebSocketClientService.ConnectionStats]
  def verifyConnection():   Future[Unit]
  def awaitActive():        Future[Unit]
}

class WebSocketClientServiceImpl(context:     Context,
                                 accountId:   AccountId,
                                 lifecycle:   ZmsLifeCycle,
                                 httpClient:  HttpClient,
                                 netClient:   ZNetClient,
                                 auth:        AuthenticationManager,
                                 override val network: NetworkModeService,
                                 backend:     BackendConfig,
                                 clientId:    ClientId,
                                 timeouts:    Timeouts,
                                 pushToken:   PushTokenService) extends WebSocketClientService {
  import WebSocketClientService._
  implicit val logTag: LogTag = s"${logTagFor[WebSocketClientService]}#${accountId.str.take(8)}"

  private implicit val ec = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "WebSocketClientService")

  //needed to ensure we have just one periodic ping running
  private var pingSchedule = CancellableFuture.cancelled[Unit]()

  private var init = httpClient.websocket(accountId, webSocketUri(clientId), auth)

  init.onFailure {
    case e: CancelException => CancellableFuture.cancelled()
    case NonFatal(ex) => retryLostConnection(ex)
  }

  private def retryLostConnection(ex: Throwable): CancellableFuture[Any] = if (closed || !init.isCompleted) { //if closed, this should be cancelled, else let it finish
    verbose(s"Will not retry connection: ${if (closed) "Connection closed" else "Connection still underway"}")
    init
  }
  else {
    error(s"Retrying lost connection after failure", ex)
    closeCurrentSocket()
    val delay = defaultBackoff.delay(retryCount)
    retryCount += 1
    debug(s"Retrying in $delay, retryCount = $retryCount")
    returning(CancellableFuture.delay(delay) flatMap { _ => connect() })(init = _)
  }.recover  {
    case e: CancelException => CancellableFuture.cancelled()
    case NonFatal(ex) => retryLostConnection(ex)
  }

  @volatile
  private var prevClient = Option.empty[WireWebSocket]

  override val useWebSocketFallback = pushToken.pushActive.map(!_)

  // true if web socket should be active,
  override val wsActive = network.networkMode.flatMap {
    case NetworkMode.OFFLINE => Signal const false
    case _ => lifecycle.loggedIn.flatMap {
      case false => Signal const false
      case _ => useWebSocketFallback
    }.flatMap {
      case true => Signal.const(true)
      case false =>
        // throttles inactivity notifications to avoid disconnecting on short UI pauses (like activity change)
        verbose(s"lifecycle no longer active, should stop the client")
        Signal.future(CancellableFuture.delayed(timeouts.webSocket.inactivityTimeout)(false)).orElse(Signal const true)
    }
  }

  override val client = wsActive.zip(lifecycle.idle) map {
    case (true, idle) =>
      debug(s"Active, client: $clientId")

      if (prevClient.isEmpty)
        prevClient = Some(createWebSocketClient(clientId))

      if (idle) {
        // start android service to keep the app running while we need to be connected.
        com.waz.zms.WebSocketService(context)
      }
      prevClient
    case (false, _) =>
      debug(s"onInactive")
      prevClient foreach (_.close())
      prevClient = None
      None
  }

  override val connected = client flatMap {
    case Some(c) => c.connected
    case None => Signal const false
  }

  /**
    * Indicates if there is some network connection error on websocket.
    * Signals `true` if we have a client, and it has been unconnected for some time.
    */
  override val connectionError = client.zip(network.networkMode) flatMap {
    case (None, _) => Signal const false
    case (Some(c), nm) =>
      c.connected flatMap {
      case true => Signal const false
      case false =>
        // delay signaling for some time
        Signal.future(CancellableFuture.delayed(getErrorTimeout(nm))(true)) orElse Signal.const(false)
    }
  }

  override val connectionStats = client collect { case Some(c) => new ConnectionStats(network, c) }

  override def verifyConnection() = client.head flatMap {
    case None => Future.successful(())
    case Some(c) => c.verifyConnection().future
  }

  network.networkMode {
    case NetworkMode.OFFLINE => //no point in checking connection
    case n =>
    // ping web socket on network changes, this should help us discover potential network outage
    verbose(s"network mode changed, will ping or reconnect, mode: $n")
    verifyConnection()
  }

  override def awaitActive(): Future[Unit] = wsActive.collect { case false => () }.head

  /**
    * Increase the timeout for poorer networks before showing a connection error
    */
  private def getErrorTimeout(networkMode: NetworkMode) = timeouts.webSocket.connectionTimeout * (networkMode match {
    case NetworkMode._2G => 3
    case NetworkMode._3G => 2
    case _ => 1
  })

  private def webSocketUri(clientId: ClientId) =
    URI.parse(backend.websocketUrl).buildUpon.appendQueryParameter("client", clientId.str).build

  //Ping, and attempt to reconnect if it fails according to the backoff
  private def verifyConnection(): CancellableFuture[Unit] = pingPong().recoverWith {
    case NonFatal(ex) if !closed =>
      warn("Ping to server failed, attempting to re-establish connection")
      onConnectionLost ! Disconnect.NoPong
      retryLostConnection(ex).flatMap ( _ => verifyConnection() )
  }

  //Continually ping the BE at a given frequency to ensure the websocket remains connected.
  override def scheduleRecurringPing(pingPeriod: FiniteDuration) = {
    verbose(s"scheduling new recurring ping every $pingPeriod")

    def recurringPing(pingPeriod: FiniteDuration): CancellableFuture[Unit] = {
      CancellableFuture.delay(pingPeriod).flatMap { _ =>
        if (closed) CancellableFuture.successful(()) //client is intentionally closed, do nothing to avoid re-establishing connection
        else {
          verbose("Performing scheduled ping")
          verifyConnection().flatMap(_ => recurringPing(pingPeriod))
        }
      }
    }

    pingSchedule.cancel() //cancel any currently out-standing pings. They might have a much greater period
    pingSchedule = recurringPing(pingPeriod)
    pingSchedule
  }
}

object WebSocketClientService {

  // collects websocket connection statistics for tracking and optimal ping timeout calculation
  class ConnectionStats(network: NetworkModeService,
                        client:  WireWebSocket) {
    import com.waz.ZLog.ImplicitTag._
    import com.waz.utils._

    def lastReceiveTime = client.lastReceivedTime.currentValue

    // last inactivity duration computed from lastReceiveTime
    val inactiveDuration = client.lastReceivedTime.scan(Option.empty[(Instant, FiniteDuration)]) {
      case (Some((prev, _)), time) => Some((time, prev.until(time).asScala))
      case (_, time) => Some((time, Duration.Zero))
    }

    // maximum time between anything is received on web socket without connection being lost
    // this gives us lower bound for ping intervals, no need to ping more often if we already know that
    // connection is able to stay alive for some interval
    val maxInactiveDuration = inactiveDuration.scan(Duration.Zero) {
      case (prev, None) => prev
      case (prev, Some((_, duration))) => prev max duration
    }

    // disconnection detected by missing Pong mean that we actually had stale connection and didn't notice it earlier
    // that's where we loose notifications, we need to somehow limit that cases
    val lostOnPingDuration = lostConnectionDuration(client.onConnectionLost.filter(_ == Disconnect.NoPong))

    // connection was lost but we were notified about it, so we'll automatically reconnect, that's not bad
    val aliveDuration = lostConnectionDuration(client.onConnectionLost.filter(_ != Disconnect.NoPong))

    private def lostConnectionDuration(onConnectionLost: EventStream[Disconnect]) =
      onConnectionLost map { _ =>
        lastReceiveTime.fold(Duration.Zero) { _.until(Instant.now).asScala }
      } filter { duration =>
        // will only report connection lost if:
        // - we have network (meaning that most likely disconnection is not a result of loosing internet connection)
        // - connection was inactive for some time, we only care about idle connections, want to detect when those get closed by some router/isp
        network.networkMode.currentValue.exists(_ != NetworkMode.OFFLINE) && duration >= PingIntervalService.MIN_PING_INTERVAL
      }
  }
}
