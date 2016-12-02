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
import com.koushikdutta.async.http.WebSocket.{PongCallback, StringCallback}
import com.koushikdutta.async.http.{AsyncHttpGet, WebSocket}
import com.koushikdutta.async.{ByteBufferList, DataEmitter}
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.events.{EventStream, Signal}
import com.waz.utils.{ExponentialBackoff, WakeLock, returning}
import com.waz.znet.ContentEncoder.{BinaryRequestContent, EmptyRequestContent}
import org.json.JSONObject

import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Left, Try}

/**
  * Handles WebSocket connection, will pass all received messages to callback.
  * Can also maintain a constant ping to the websocket server to ensure that the connection is alive.
  * If the connection drops, it will automatically try to reconnect.
  */
class WebSocketClient(context: Context,
                      client: AsyncClient,
                      uri: => Uri,
                      auth: AccessTokenProvider,
                      backoff: ExponentialBackoff = WebSocketClient.defaultBackoff,
                      pongTimeout: FiniteDuration = 15.seconds) {

  implicit val dispatcher = new SerialDispatchQueue(Threading.ThreadPool)

  private val wakeLock = new WakeLock(context)

  val connected = Signal(false)
  val onError   = EventStream[Exception]()
  val onMessage = EventStream[ResponseContent]()

  private var init  : CancellableFuture[WebSocket] = connect()
  private var socket: Option[WebSocket]            = None

  private var closed     = false
  private var retryCount = 0

  private var pongPromise = Option.empty[Promise[Unit]]
  //Used to ensure just one ping request (waiting for pong) is active at a time
  private var pongFuture = CancellableFuture.cancelled[Unit]()

  //needed to ensure we have just one periodic ping running
  private var pingSchedule = CancellableFuture.cancelled[Unit]()

  def send[A: ContentEncoder](msg: A): CancellableFuture[Unit] = init flatMap { s =>
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
    init.cancel()
    pingSchedule.cancel()
    pongFuture.cancel()
    closeCurrentSocket()
    connected ! false
  }

  def isClosed = closed

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
        returning(new CancellableFuture(p).withTimeout(30.seconds)) { _.onFailure { case _ => f.cancel(true) } }
      }
    case Left(status) =>
      CancellableFuture.failed(new Exception(s"Authentication returned error status: $status"))

  } recoverWith {
    case e: CancelException => CancellableFuture.cancelled()
    case NonFatal(ex) => retryLostConnection(ex)
  }

  private def onConnected(webSocket: WebSocket): WebSocket = {
    debug(s"onConnected $webSocket")
    require(webSocket != null, "connected web socket should be not null")

    closeCurrentSocket()
    socket = Option(webSocket)
    retryCount = 0

    connected ! true

    webSocket.setStringCallback(new StringCallback {
      override def onStringAvailable(s: String): Unit = wakeLock {
        onMessage ! Try(JsonObjectResponse(new JSONObject(s))).getOrElse(StringResponse(s))
      }
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
        retryLostConnection(ex)
      }
    })
    webSocket.setPongCallback(new PongCallback {
      override def onPongReceived(s: String): Unit = {
        info(s"pong")
        pongPromise.foreach(_.trySuccess(()))
        pongPromise = None
      }
    })
    webSocket.setEndCallback(new CompletedCallback {
      override def onCompleted(ex: Exception): Unit = {
        error("WebSocket frame parsing failed", ex)
        onError ! ex
      }
    })
    webSocket
  }

  private def retryLostConnection(ex: Throwable) = if (!closed) {
    warn(s"Retrying lost connection after failure", ex)
    closeCurrentSocket()
    connected ! false
    val delay = backoff.delay(retryCount)
    retryCount += 1
    debug(s"Retrying in $delay, retryCount = $retryCount")
    returning(CancellableFuture.delay(delay) flatMap { _ => connect() })(init = _)
  } else init //if closed, this should be cancelled

  //Continually ping the BE at a given frequency to ensure the websocket remains connected.
  def scheduleRecurringPing(pingPeriod: FiniteDuration): CancellableFuture[Unit] = {
    verbose(s"scheduling new recurring ping every $pingPeriod")
    //TODO Dean - can this eventually overflow the call stack?
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

  //Ping, and attempt to reconnect if it fails according to the backoff
  def verifyConnection(): CancellableFuture[Unit] = ping().recoverWith {
    case NonFatal(ex) =>
      warn("Ping to server failed, attempting to re-establish connection")
      retryLostConnection(ex).flatMap ( _ => verifyConnection() )
  }

  //Pings the BE. Will return a future of whether a pong was received within the pongTimeout. If the future
  //succeeds, we can assume the ping was successful.
  def ping(): CancellableFuture[Unit] = init flatMap { ws =>
    pongPromise.fold {
      val p = Promise[Unit]()
      pongPromise = Some(p)
      verbose(s"ping")
      ws.ping("hello")
      returning(new CancellableFuture(p).withTimeout(pongTimeout)) (pongFuture = _)
    } { p =>
      pongFuture
    }.recoverWith { case _ =>
      pongPromise = None
      pongFuture
    }
  }
}

object WebSocketClient {

  val defaultBackoff = new ExponentialBackoff(250.millis, 5.minutes)

  def apply(context: Context, client: ZNetClient, pushUri: => Uri) =
    new WebSocketClient(context, client.client, pushUri, client.auth)
}
