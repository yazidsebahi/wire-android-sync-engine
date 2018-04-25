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

import java.net.URL
import java.util.concurrent.TimeoutException

import com.waz.utils.events.{EventContext, EventStream, Subscription}
import com.waz.znet.WebSocketFactory.SocketEvent
import io.fabric8.mockwebserver.DefaultMockServer
import org.scalatest.{BeforeAndAfterEach, Inside, MustMatchers, WordSpec}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, _}
import scala.util.Try

object OkHttpWebSocketSpec {

  object BlockingSyntax {

    def toBlocking[T,R](stream: EventStream[T])
                       (doWithBlocking: BlockingEventStream[T] => R)
                       (implicit ec: EventContext = EventContext.Global): R = {
      val blocking = new BlockingEventStream[T](stream)
      blocking.subscribe
      val result = doWithBlocking(blocking)
      blocking.unsubscribe()
      result
    }

    class BlockingEventStream[T](private val eventStream: EventStream[T]) {
      private var subscription: Subscription = _
      private val events = ArrayBuffer.empty[T]

      private val waitingStepInMillis = 100
      private val defaultTimeout: Duration = 3.seconds

      def subscribe(implicit ev: EventContext): Unit = {
        subscription = eventStream { e =>
          println(s"BlockingEventStream. Received event: $e")
          events.append(e)
        }
      }

      def unsubscribe(): Unit = {
        if (subscription != null) subscription.destroy()
      }

      private def waitForEvents(eventsCount: Int, timeout: Duration = defaultTimeout): List[T] = {
        var alreadyWaiting = 0
        while (events.size < eventsCount) {
          Thread.sleep(waitingStepInMillis)
          alreadyWaiting += waitingStepInMillis
          if (alreadyWaiting >= timeout.toMillis) throw new TimeoutException()
        }
        events.toList
      }

      def takeEvents(count: Int, timeout: Duration = defaultTimeout): List[T] = {
        waitForEvents(count, timeout)
        events.take(count).toList
      }

      def waitForEvents(duration: Duration): List[T] = {
        val eventsCount = events.size
        Thread.sleep(duration.toMillis)
        if (events.size == eventsCount) List.empty[T]
        else events.drop(eventsCount).toList
      }

      def getEvent(index: Int, timeout: Duration = defaultTimeout): T = {
        waitForEvents(index + 1, timeout)
        events(index)
      }

    }

  }

}

class OkHttpWebSocketSpec extends WordSpec with MustMatchers with Inside with BeforeAndAfterEach {

  import EventContext.Implicits.global
  import OkHttpWebSocketSpec.BlockingSyntax.toBlocking

  private val testPath = "/test"
  private val defaultWaiting = 100
  private def testWebSocketRequest(url: String): HttpRequest2 = HttpRequest2Impl(new URL(url))

  private var mockServer: DefaultMockServer = _

  override protected def beforeEach(): Unit = {
    mockServer = new DefaultMockServer()
    mockServer.start()
  }

  override protected def afterEach(): Unit = {
    mockServer.shutdown()
  }

  "OkHttp events stream" should {

    "provide all okHttp events properly when socket closed without error." in {
      val textMessage = "Text message"
      val bytesMessage = Array[Byte](1, 2, 3, 4)

      mockServer.expect().get().withPath(testPath)
        .andUpgradeToWebSocket()
        .open()
        .waitFor(defaultWaiting).andEmit(textMessage)
        .waitFor(defaultWaiting).andEmit(bytesMessage)
        .done().once()


      toBlocking(OkHttpWebSocketFactory.openWebSocket(testWebSocketRequest(mockServer.url(testPath)))) { stream =>
        val firstEvent :: secondEvent :: thirdEvent :: fourthEvent :: Nil = stream.takeEvents(4)

        firstEvent mustBe an[SocketEvent.Opened]
        secondEvent mustBe an[SocketEvent.Message]
        thirdEvent mustBe an[SocketEvent.Message]
        fourthEvent mustBe an[SocketEvent.Closing]

        withClue("No events should be emitted after socket has been closed") {
          stream.waitForEvents(2.seconds) mustBe List.empty[SocketEvent]
        }
      }
    }

    "provide all okHttp events properly when socket closed with error." in {
      mockServer.expect().get().withPath(testPath)
        .andUpgradeToWebSocket()
        .open()
        .waitFor(10000).andEmit("")
        .done().once()

      toBlocking(OkHttpWebSocketFactory.openWebSocket(testWebSocketRequest(mockServer.url(testPath)))) { stream =>
        val firstEvent = stream.getEvent(0)
        Try { mockServer.shutdown() } //we do not care about this error
      val secondEvent = stream.getEvent(1)

        firstEvent mustBe an[SocketEvent.Opened]
        secondEvent mustBe an[SocketEvent.Closed]

        inside(secondEvent) { case SocketEvent.Closed(_, error) =>
          error mustBe an[Some[_]]
        }

        withClue("No events should be emitted after socket has been closed") {
          stream.waitForEvents(2.seconds) mustBe List.empty[SocketEvent]
        }
      }
    }
  }




}

