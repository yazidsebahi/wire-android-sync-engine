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

import java.net.InetSocketAddress

import android.net.Uri
import com.waz.testutils.Slow
import com.waz.znet.AuthenticationManager.Token
import com.waz.znet.Response.Status
import com.waz.RobolectricUtils
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


class WebSocketManagerSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils {

  object lock
  var socket: Option[WebSocket] = None
  var server: WebSocketServer = _
  var manager: WebSocketClient = _

  var authResult: Either[Status, Token] = Right(Token("test_token", "Bearer"))
  val auth = new AccessTokenProvider {
    override def currentToken() = Future.successful(authResult)
  }

  def createServer(port: Int = 9982) = {
    val s = new WebSocketServer(new InetSocketAddress(port)) {

      override def onOpen(conn: WebSocket, handshake: ClientHandshake): Unit = {
        socket = Some(conn)
        lock.synchronized(lock.notifyAll())
      }

      override def onClose(conn: WebSocket, code: Int, reason: String, remote: Boolean): Unit = {
        if (socket.contains(conn)) {
          socket = None
          lock.synchronized(lock.notifyAll())
        }
      }

      override def onMessage(conn: WebSocket, message: String): Unit = {
        println(s"Server received a message: $message")
      }

      override def onError(conn: WebSocket, ex: Exception): Unit = {
        println(s"Server got an error $ex")
        ex.printStackTrace(Console.err)
      }
    }
    s
  }

  before {
    server = createServer()
    server.start()
  }

  after {
    server.stop()
    if (manager != null)
      Await.ready(manager.close(), 10.seconds)
  }

  feature("Connection") {

    scenario("Connect to server and close") {
      manager = new WebSocketClient(new AsyncClient(), Uri.parse("http://localhost:9982"), auth)
      awaitUi(socket.isDefined)
      manager.close()
      awaitUi(socket.isEmpty)
    }

    scenario("Retry connection to unavailable server", Slow) {
      server.stop()

      manager = new WebSocketClient(new AsyncClient(), Uri.parse("http://localhost:9982"), auth)
      Thread.sleep(1000)
      socket.isDefined shouldEqual false

      server = createServer()
      server.start()
      awaitUi(socket.isDefined)(10.seconds)

      manager.close()
      awaitUi(socket.isEmpty)(10.seconds)
    }

    scenario("Retry on closed connection", Slow) {
      manager = new WebSocketClient(new AsyncClient(), Uri.parse("http://localhost:9982"), auth)
      awaitUi(socket.isDefined)(10.seconds)
      val ws = socket.get
      socket = None
      ws.close()
      awaitUi(socket.isDefined)(10.seconds)
      manager.close()
      awaitUi(socket.isEmpty)(10.seconds)
    }

    scenario("Retry when server stops working", Slow) {
      manager = new WebSocketClient(new AsyncClient(), Uri.parse("http://localhost:9982"), auth)
      awaitUi(socket.isDefined)(10.seconds)

      server.stop()
      socket = None

      Thread.sleep(1000)

      server = createServer()
      server.start()
      awaitUi(socket.isDefined)(10.seconds)

      manager.close()
      awaitUi(socket.isEmpty)(10.seconds)
    }
  }
}
