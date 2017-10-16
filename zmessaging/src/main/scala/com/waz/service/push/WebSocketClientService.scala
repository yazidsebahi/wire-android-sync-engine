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
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.model.AccountId
import com.waz.model.otr.ClientId
import com.waz.service._
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.utils.{returning, returningF}
import com.waz.utils.wrappers.URI
import com.waz.znet.WebSocketClient.{Disconnect, defaultBackoff}
import com.waz.znet._
import com.waz.zms.WebSocketService
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal

trait WebSocketClientService {
  def useWebSocketFallback:                              Signal[Boolean]
  def client:                                            Signal[Option[WireWebSocket]]
  def connected:                                         Signal[Boolean]
  def connectionError:                                   Signal[Boolean]
  def connectionStats:                                   Signal[WebSocketClientService.ConnectionStats]
  def awaitActive():                                     Future[Unit]
  def scheduleRecurringPing(pingPeriod: FiniteDuration): Future[Unit]
}

class WebSocketClientServiceImpl(context:    Context,
                                 accountId:  AccountId,
                                 lifeCycle:  ZmsLifeCycle,
                                 httpClient: HttpClient,
                                 auth:       AuthenticationManager,
                                 network:    NetworkModeService,
                                 backend:    BackendConfig,
                                 clientId:   ClientId,
                                 pushToken:  PushTokenService) extends WebSocketClientService {
  import WebSocketClientService._
  implicit val logTag: LogTag = s"${logTagFor[WebSocketClientService]}#${accountId.str.take(8)}"

  private implicit val ec = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "WebSocketClientService")

  //needed to ensure we have just one periodic ping running
  private var pingSchedule = CancellableFuture.cancelled[Unit]()

  private var retryCount = 0

  override val useWebSocketFallback = pushToken.pushActive.map(!_)

  // true if web socket should be active,
  private val wsActive = network.networkMode.flatMap {
    case NetworkMode.OFFLINE => Signal const false
    case _ => lifeCycle.accLoggedIn(accountId).flatMap {
      case false => Signal const false
      case _ => useWebSocketFallback
    }.flatMap {
      case true => Signal.const(true)
      case false =>
        // throttles inactivity notifications to avoid disconnecting on short UI pauses (like activity change)
        verbose(s"lifecycle no longer active, should stop the client")
        Signal.future(CancellableFuture.delayed(inactivityTimeout)(false)).orElse(Signal const true)
    }
  }

  // start android service to keep the app running while we need to be connected.
  lifeCycle.idle.onChanged.filter(_ == true)(_ => Option(context) match {
    case Some(c) => WebSocketService(c)
    case _ => verbose("No context, websocket may not be kept open in the background")
  })

  override val client = (for {
    wsActive <- wsActive
    ws <- Signal.future {
      if (wsActive) {
        debug(s"Active, client: $clientId")
        openWebsocket()
      } else {
        debug(s"onInactive")
        init.cancel()
        CancellableFuture.successful(None)
      }
    }
  } yield ws).orElse(Signal.const(None))

  private var currentSocket = Option.empty[WireWebSocket]
  private var init = CancellableFuture.cancelled[WireWebSocket]()

  private def openWebsocket() = {
    def open(): CancellableFuture[WireWebSocket] = returningF(httpClient.websocket(accountId, webSocketUri(clientId, backend), auth)) { f =>
      f.recoverWith {
        case _: CancelException =>
          warn("Opening websocket was cancelled")
          closeWebSocket()
          CancellableFuture.cancelled()
        case NonFatal(ex) =>
          val delay = defaultBackoff.delay(retryCount)
          warn(s"Failed to open websocket, retrying after $delay, retryCount = $retryCount", ex)
          retryCount += 1
          CancellableFuture.delay(delay).flatMap(_ => closeWebSocket()).flatMap(_ => open())
      }
      retryCount = 0
      init = f
    }

    verbose(s"Opening new socket. Current socket: $currentSocket, already opening?: ${!init.isCompleted}")
    if (!init.isCompleted) init else {
      closeWebSocket()
      open()
    }
  }.map { ws =>
    currentSocket = Some(ws)
    currentSocket
  }

  private def closeWebSocket(): CancellableFuture[Unit] = {
    verbose(s"Closing current websocket: $currentSocket")
    init.cancel()
    returning(currentSocket.fold(CancellableFuture.successful({}))(ws => CancellableFuture.lift(ws.close()))) { _ =>
      currentSocket = None
    }
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

  private def verifyConnection(): CancellableFuture[Unit] = CancellableFuture.lift(client.head).flatMap {
    case None => CancellableFuture.successful(())
    case Some(c) => c.pingPong().recoverWith {
      case NonFatal(_) =>
        warn("Ping to server failed, attempting to re-establish connection")
        openWebsocket().flatMap ( _ => verifyConnection() )
    }
  }

  network.networkMode.onChanged {
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
  private def getErrorTimeout(networkMode: NetworkMode) = connectionTimeout * (networkMode match {
    case NetworkMode._2G => 3
    case NetworkMode._3G => 2
    case _ => 1
  })

  //Continually ping the BE at a given frequency to ensure the websocket remains connected.
  override def scheduleRecurringPing(pingPeriod: FiniteDuration): Future[Unit] = {
    verbose(s"scheduling new recurring ping every $pingPeriod")
    def recurringPing(pingPeriod: FiniteDuration): CancellableFuture[Unit] = {
      CancellableFuture.delay(pingPeriod).flatMap { _ =>
        verbose("Performing scheduled ping")
        verifyConnection().flatMap(_ => recurringPing(pingPeriod))
      }
    }

    pingSchedule.cancel() //cancel any currently out-standing pings. They might have a much greater period
    pingSchedule = recurringPing(pingPeriod)
    pingSchedule.future
  }
}

object WebSocketClientService {

  def webSocketUri(clientId: ClientId, backend: BackendConfig) =
    URI.parse(backend.websocketUrl).buildUpon.appendQueryParameter("client", clientId.str).build

  var inactivityTimeout = 3.seconds
  var connectionTimeout = 8.seconds

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
