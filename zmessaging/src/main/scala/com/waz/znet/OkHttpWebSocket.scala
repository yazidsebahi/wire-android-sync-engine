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

import com.waz.ZLog.{error, info, warn}
import com.waz.utils.events.{EventContext, EventStream}
import okhttp3._
import okio.ByteString
import org.json.JSONObject

import scala.util.Try

object OkHttpWebSocket {

  import okhttp3.{Request => OkRequest, Response => OkResponse}

  sealed trait SocketEvent
  object SocketEvent {
    case class Opened(socket: WebSocket, response: OkResponse) extends SocketEvent
    case class Message(socket: WebSocket, content: ResponseContent) extends SocketEvent
    case class Closed(socket: WebSocket, error: Option[Throwable] = None) extends SocketEvent
  }

  def socketEvents(client: OkHttpClient, request: OkRequest)(implicit evContext: EventContext): EventStream[SocketEvent] = {
    new EventStream[SocketEvent] {
      import SocketEvent._
      import com.waz.ZLog.ImplicitTag.implicitLogTag

      @volatile private var socket: WebSocket = _

      override protected def onWire(): Unit = {
        socket = client.newWebSocket(request, new WebSocketListener {

          override def onOpen(webSocket: WebSocket, response: OkResponse): Unit = {
            info("WebSocket connection has been opened")
            publish(Opened(webSocket, response))
          }

          override def onMessage(webSocket: WebSocket, text: String): Unit = {
            info("WebSocket received a text message.")
            val content = Try(JsonObjectResponse(new JSONObject(text))).getOrElse(StringResponse(text))
            publish(Message(webSocket, content))
          }

          override def onMessage(webSocket: WebSocket, bytes: ByteString): Unit = {
            info("WebSocket received a bytes message.")
            val content = Try(JsonObjectResponse(new JSONObject(bytes.utf8()))).getOrElse(BinaryResponse(bytes.toByteArray, ""))
            publish(Message(webSocket, content))
          }

          override def onClosing(webSocket: WebSocket, code: Int, reason: String): Unit = {
            info("WebSocket connection will be closed")
            webSocket.close(1000, null) //according to RFC 6455
          }

          override def onClosed(webSocket: WebSocket, code: Int, reason: String): Unit = {
            info("WebSocket connection has been closed")
            publish(Closed(webSocket))
          }

          override def onFailure(webSocket: WebSocket, ex: Throwable, response: okhttp3.Response): Unit = {
            warn("WebSocket connection has been failed", ex)
            publish(Closed(webSocket, Some(ex)))
          }
        })
      }

      override protected def onUnwire(): Unit = socket.cancel()
    }
  }

}
