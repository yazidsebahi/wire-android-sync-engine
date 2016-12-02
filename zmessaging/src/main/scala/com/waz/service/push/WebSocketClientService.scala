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
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.model.otr.ClientId
import com.waz.service.LifecycleState._
import com.waz.service._
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.znet.{WebSocketClient, ZNetClient}

import scala.concurrent.Future
import scala.concurrent.duration._

class WebSocketClientService(context: Context, lifecycle: ZmsLifecycle, netClient: ZNetClient, val network: NetworkModeService, backend: BackendConfig, clientId: ClientId, timeouts: Timeouts, gcmService: IGcmService, prefs: PreferenceService) {
  import WebSocketClientService._
  private implicit val ec = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "WebSocketClientService")

  @volatile
  private var prevClient = Option.empty[WebSocketClient]

  // true if websocket should be active,
  val wsActive = lifecycle.lifecycleState.flatMap {
    case Stopped => Signal const false
    case Idle => gcmService.gcmActive.map(!_)
    case Active | UiActive => Signal const true
  }.flatMap {
    case true => Signal.const(true)
    case false =>
      // throttles inactivity notifications to avoid disconnecting on short UI pauses (like activity change)
      verbose(s"lifecycle no longer active, should stop the client")
      Signal.future(CancellableFuture.delayed(timeouts.webSocket.inactivityTimeout)(false)).orElse(Signal const true)
  }

  val client = wsActive.zip(lifecycle.lifecycleState) map {
    case (true, state) =>
      debug(s"Active, client: $clientId")

      if (prevClient.isEmpty)
        prevClient = Some(createWebSocketClient(clientId))

      if (state == Idle) {
        // start android service to keep the app running while we need to be connected.
        com.waz.zms.WebSocketService(context)
      }
      prevClient.foreach { _.scheduleRecurringPing(if (state == Active || state == UiActive) PING_INTERVAL_FOREGROUND else prefs.webSocketPingInterval) }
      prevClient
    case (false, _) =>
      debug(s"onInactive")
      prevClient foreach (_.close())
      prevClient = None
      None
  }

  val connected = client flatMap {
    case Some(c) => c.connected
    case None => Signal const false
  }

  /**
    * Indicates if there is some network connection error on websocket.
    * Signals `true` if we have a client, and it has been unconnected for some time.
    */
  val connectionError = client.zip(network.networkMode) flatMap {
    case (None, _) => Signal const false
    case (Some(c), nm) =>
      c.connected flatMap {
      case true => Signal const false
      case false =>
        // delay signaling for some time
        Signal.future(CancellableFuture.delayed(getErrorTimeout(nm))(true)) orElse Signal.const(false)
    }
  }

  def verifyConnection() =
    client.head flatMap {
      case None => Future.successful(())
      case Some(c) => c.verifyConnection().future
    }

  def awaitActive(): Future[Unit] = wsActive.collect { case false => () }.head

  /**
    * Increase the timeout for poorer networks before showing a connection error
    */
  private def getErrorTimeout(networkMode: NetworkMode) = timeouts.webSocket.connectionTimeout * (networkMode match {
    case NetworkMode._2G => 3
    case NetworkMode._3G => 2
    case _ => 1
  })

  private def webSocketUri(clientId: ClientId) =
    Uri.parse(backend.pushUrl).buildUpon().appendQueryParameter("client", clientId.str).build()

  private[waz] def createWebSocketClient(clientId: ClientId) = WebSocketClient(context, netClient, webSocketUri(clientId))
}

object WebSocketClientService {
  val PING_INTERVAL_FOREGROUND         = 30.seconds
  val DEFAULT_PING_INTERVAL_BACKGROUND = 15.minutes
  val MIN_PING_INTERVAL                = 15.seconds

}
