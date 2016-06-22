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
package com.waz.service

import android.net.Uri
import com.waz.ZLog._
import com.waz.model.otr.ClientId
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.znet.{WebSocketClient, ZNetClient}

class WebSocketClientService(lifecycle: ZmsLifecycle, netClient: ZNetClient, network: NetworkModeService, backend: BackendConfig, clientId: ClientId, timeouts: Timeouts) {
  private implicit val tag: LogTag = logTagFor[WebSocketClientService]
  private implicit val ec = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "WebSocketClientService")
  import LifecycleState._

  @volatile
  private var prevClient = Option.empty[WebSocketClient]

  // true if websocket should be active,
  // throttles inactivity notifications to avoid disconnecting on short UI pauses (like activity change)
  private[service] val wsActive = lifecycle.lifecycleState.map(s => s == UiActive || s == Active) flatMap {
    case true   => Signal const true
    case false  => Signal.future(CancellableFuture.delayed(timeouts.webSocket.inactivityTimeout)(false)).orElse(Signal const true)
  }

  val client = wsActive map {
    case true =>
      debug(s"Active, client: $clientId")
      if (prevClient.isEmpty)
        prevClient = Some(createWebSocketClient(clientId))
      prevClient
    case false =>
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
  val connectionError = client flatMap {
    case None => Signal const false
    case Some(c) =>
      c.connected flatMap {
        case true => Signal const false
        case false =>
          // delay signaling for some time
          Signal.future(CancellableFuture.delayed(timeouts.webSocket.connectionTimeout)(true)) orElse Signal.const(false)
      }
  }

  network.networkMode { n =>
    // ping web socket on network changes, this should help us discover potential network outage
    client.head.foreach {
      case Some(c) =>
        verbose(s"network mode changed, will ping or reconnect, mode: $n")
        c.retryInDisconnected()
        c.socket.foreach(_.ping("ping"))
    }
  }

  private def webSocketUri(clientId: ClientId) =
    Uri.parse(backend.pushUrl).buildUpon().appendQueryParameter("client", clientId.str).build()

  private[waz] def createWebSocketClient(clientId: ClientId) = WebSocketClient(netClient, webSocketUri(clientId))
}
