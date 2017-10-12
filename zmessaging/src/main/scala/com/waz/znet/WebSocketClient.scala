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
package com.waz.znet

import com.koushikdutta.async.callback.{CompletedCallback, DataCallback}
import com.koushikdutta.async.http.WebSocket
import com.koushikdutta.async.http.WebSocket.{PongCallback, StringCallback}
import com.koushikdutta.async.{ByteBufferList, DataEmitter}
import com.waz.ZLog._
import com.waz.model.AccountId
import com.waz.service.ZMessaging.clock
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventStream, Signal}
import com.waz.utils.{Backoff, ExponentialBackoff}
import com.waz.znet.ContentEncoder.{BinaryRequestContent, EmptyRequestContent}
import com.waz.znet.WebSocketClient.Disconnect
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try


trait WireWebSocket {

  def connected:        Signal[Boolean]
  def lastReceivedTime: Signal[Instant]

  def onMessage:        EventStream[ResponseContent]
  def onError:          EventStream[Exception]
  def onConnectionLost: EventStream[Disconnect]

  def send[A: ContentEncoder](msg: A): Future[Unit]
  def pingPong(): CancellableFuture[Unit]

  def close(): Future[Unit]
}


/**
  * Handles WebSocket connection, will pass all received messages to callback.
  * Can also maintain a constant ping to the websocket server to ensure that the connection is alive.
  * If the connection drops, it will automatically try to reconnect.
  */
class WebSocketClient(accountId:   AccountId,
                      webSocket:   WebSocket,
                      pongTimeout: FiniteDuration = 15.seconds) extends WireWebSocket {
  import WebSocketClient._

  implicit val logTag: LogTag = s"${logTagFor[WebSocketClient]}#${accountId.str.take(8)}"
  implicit val dispatcher = new SerialDispatchQueue(name = "WebSocketClient")

  override val connected = Signal(false)
  override val onError   = EventStream[Exception]()
  override val onMessage = EventStream[ResponseContent]()
  override val lastReceivedTime = Signal[Instant]() // time when something was last received on websocket
  override val onConnectionLost = EventStream[Disconnect]()

  private val onPing    = EventStream[Unit]()
  private val onPong    = EventStream[Unit]()

  //Used to ensure just one ping request (waiting for pong) is active at a time
  private var pongFuture = CancellableFuture.cancelled[Unit]()

  require(webSocket != null, "connected web socket should be not null")
  debug(s"onConnected $webSocket")

  connected ! webSocket.isOpen
  lastReceivedTime ! clock.instant()

  webSocket.setStringCallback(new StringCallback {
    override def onStringAvailable(s: String): Unit = {
      lastReceivedTime ! Instant.now
      onMessage ! Try(JsonObjectResponse(new JSONObject(s))).getOrElse(StringResponse(s))
    }
  })
  webSocket.setDataCallback(new DataCallback {
    override def onDataAvailable(emitter: DataEmitter, bb: ByteBufferList): Unit = {
      lastReceivedTime ! Instant.now
      onMessage ! Try(JsonObjectResponse(new JSONObject(new String(bb.getAllByteArray, "utf8")))).getOrElse(BinaryResponse(bb.getAllByteArray, ""))
    }
  })
  webSocket.setClosedCallback(new CompletedCallback {
    override def onCompleted(ex: Exception): Unit = dispatcher {
      if (ex != null) error("WebSocket connection has been closed with error", ex)
      else info("WebSocket connection has been closed")
      connected ! false
      onConnectionLost ! Disconnect.WsClosed
    }
  })
  webSocket.setPongCallback(new PongCallback {
    override def onPongReceived(s: String): Unit = {
      info(s"pong")
      lastReceivedTime ! Instant.now
      onPong ! (())
    }
  })
  webSocket.setEndCallback(new CompletedCallback {
    override def onCompleted(ex: Exception): Unit = {
      error("WebSocket frame parsing failed", ex)
      onError ! ex
      onConnectionLost ! Disconnect.WsEnded
    }
  })

  override def send[A: ContentEncoder](msg: A) = dispatcher {
    implicitly[ContentEncoder[A]].apply(msg) match {
      case EmptyRequestContent =>
        error(s"Sending EmptyRequest with webSocket for msg: '$msg'")
        webSocket.send("")
      case BinaryRequestContent(data, _) =>
        webSocket.send(data)
      case req =>
        throw new UnsupportedOperationException(s"Unsupported request content: $req")
    }
  }.future

  override def close() = dispatcher {
    info(s"closing socket: $webSocket")
    pongFuture.cancel()
    webSocket.close()
  }.future

  //Pings the BE. Will return a future of whether a pong was received within the pongTimeout. If the future
  //succeeds, we can assume the ping was successful.
  def pingPong(): CancellableFuture[Unit] = dispatcher {
    case Some(c) =>
      import com.waz.utils.events.EventContext.Implicits.global

      if (pongFuture.isCompleted) { // ping only if not waiting for pong already
        pongFuture = onPong.next.withTimeout(pongTimeout)
        info(s"ping")
        webSocket.ping(s"ping")
        onPing ! ({})
      }
      pongFuture
  }.flatten
}

object WebSocketClient {

  //var for tests
  var defaultBackoff: Backoff = new ExponentialBackoff(250.millis, 5.minutes)

  trait Disconnect
  object Disconnect {
    case object NoPong extends Disconnect
    case object WsClosed extends Disconnect
    case object WsEnded extends Disconnect
  }
}
