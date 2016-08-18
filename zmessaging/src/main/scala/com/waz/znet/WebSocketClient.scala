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

import android.content.Context
import android.net.Uri
import com.koushikdutta.async.callback.{CompletedCallback, DataCallback}
import com.koushikdutta.async.http.AsyncHttpClient.WebSocketConnectCallback
import com.koushikdutta.async.http.WebSocket.StringCallback
import com.koushikdutta.async.http.{AsyncHttpGet, WebSocket}
import com.koushikdutta.async.{ByteBufferList, DataEmitter}
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.events.{EventStream, Signal}
import com.waz.utils.{ExponentialBackoff, WakeLock}
import com.waz.znet.ContentEncoder.{BinaryRequestContent, EmptyRequestContent}
import org.json.JSONObject

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Left, Try}

/**
 * Handles WebSocket connection, will pass all received messages to callback, and retry connection if needed.
 */
class WebSocketClient(context: Context, client: AsyncClient, uri: => Uri, auth: AccessTokenProvider, backoff: ExponentialBackoff = new ExponentialBackoff(250.millis, 5.minutes) ) {
  import WebSocketClient._

  implicit val dispatcher = new SerialDispatchQueue(Threading.ThreadPool)

  private val wakeLock = new WakeLock(context)

  val connected = Signal(false)
  val onError   = EventStream[Exception]()
  val onMessage = EventStream[ResponseContent]()

  var future: CancellableFuture[WebSocket] = connect()
  var socket: Option[WebSocket] = None
  var closed = false
  var retryCount = 0

  def send[A: ContentEncoder](msg: A): CancellableFuture[Unit] = future flatMap { s =>
    implicitly[ContentEncoder[A]].apply(msg) match {
      case EmptyRequestContent =>
        error(s"Sending EmptyRequest with webSocket for msg: '$msg'")
        s.send("")
        CancellableFuture.successful({})
      case BinaryRequestContent(data, _) =>
        s.send(data)
        CancellableFuture.successful({})
      case req =>
        throw new UnsupportedOperationException(s"Unsupported request content: $req")
    }
  }

  def close() = dispatcher {
    info("closing")
    closed = true
    future.cancel()
    closeCurrentSocket()
    connected ! false
  }

  def retryIfDisconnected() = connected.head flatMap {
    case false if !closed =>
      verbose(s"retrying")
      retryCount = 0
      future.cancel()
      future = connect()
      future.future.map(Some(_))
    case _ =>
      verbose("already connected")
      Future.successful(socket)
  }

  private def closeCurrentSocket() = socket.foreach { s =>
    s.setEndCallback(null)
    s.setClosedCallback(null)
    s.close()
    socket = None
  }

  protected def connect(): CancellableFuture[WebSocket] = CancellableFuture.lift(auth.currentToken()) flatMap {
    case Right(token) if closed => CancellableFuture.failed(new Exception("WebSocket client closed"))
    case Right(token) =>
      val p = Promise[WebSocket]()
      debug(s"Sending webSocket request: $uri")
      val req = token.prepare(new AsyncHttpGet(uri))
      req.setHeader("Accept-Encoding", "identity") // XXX: this is a hack for Backend In The Box problem: 'Accept-Encoding: gzip' header causes 500
      req.setHeader("User-Agent", client.userAgent)

      CancellableFuture.lift(client.client) flatMap { client =>
        val f = client.websocket(req, null, new WebSocketConnectCallback {
          override def onCompleted(ex: Exception, socket: WebSocket): Unit = {
            debug(s"WebSocket request finished, ex: $ex, socket: $socket")
            p.tryComplete(if (ex == null) Try(onConnected(socket)) else Failure(ex))
          }
        })
        new CancellableFuture(p) {
          override def cancel()(implicit tag: LogTag): Boolean = {
            f.cancel(true)
            super.cancel()(tag)
          }
        }
      }
    case Left(status) =>
      CancellableFuture.failed(new Exception(s"Authentication returned error status: $status"))

  } recoverWith {
    case e: CancelException => CancellableFuture.failed(e)
    case NonFatal(ex) if !closed =>
      info(s"Got error while connecting, will retry, ex: $ex")
      val delay = backoff.delay(retryCount)
      retryCount += 1
      debug(s"Retrying in $delay, retryCount = $retryCount")
      CancellableFuture.delay(delay) flatMap { _ => connect() }
  }

  private def onConnected(webSocket: WebSocket): WebSocket = {
    debug(s"onConnected $webSocket")
    require(webSocket != null, "connected web socket should be not null")

    closeCurrentSocket()
    socket = Option(webSocket)
    retryCount = 0

    connected ! true

    webSocket.setStringCallback(new StringCallback {
      override def onStringAvailable(s: String): Unit = wakeLock { onMessage ! Try(JsonObjectResponse(new JSONObject(s))).getOrElse(StringResponse(s)) }
    })
    webSocket.setDataCallback(new DataCallback {
      override def onDataAvailable(emitter: DataEmitter, bb: ByteBufferList): Unit = wakeLock {
        onMessage ! Try(JsonObjectResponse(new JSONObject(new String(bb.getAllByteArray, "utf8")))).getOrElse(BinaryResponse(bb.getAllByteArray, ""))
      }
    })
    webSocket.setClosedCallback(new CompletedCallback {
      override def onCompleted(ex: Exception): Unit = dispatcher {
        if (ex != null) error("WebSocket connection has been closed with error", ex)
        else info("WebSocket connection has been closed")

        if (!closed) {
          closeCurrentSocket()
          connected ! false
          future = CancellableFuture.delay(backoff.delay(retryCount)) flatMap { _ => connect() }
          retryCount += 1
        }
      }
    })
    webSocket.setEndCallback(new CompletedCallback {
      override def onCompleted(ex: Exception): Unit = {
        error("WebSocket frame parsing failed", ex)
        onError ! ex
      }
    })

    schedulePing(webSocket)

    webSocket
  }

  private def schedulePing(webSocket: WebSocket): CancellableFuture[Unit] =
    CancellableFuture.delay(PING_INTERVAL) flatMap { _ =>
      if (closed) CancellableFuture.successful({})
      else {
        webSocket.ping("ping")
        schedulePing(webSocket)
      }
    }
}

object WebSocketClient {
  val PING_INTERVAL = 8.minutes

  def apply(context: Context, client: ZNetClient, pushUri: => Uri) =
    new WebSocketClient(context, client.client, pushUri, client.auth)
}
