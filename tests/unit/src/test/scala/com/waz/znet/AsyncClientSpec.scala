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

import java.io.{File, PipedInputStream, PipedOutputStream}

import android.os.Build._
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import com.koushikdutta.async.callback.CompletedCallback.NullCompletedCallback
import com.koushikdutta.async.callback.DataCallback.NullDataCallback
import com.koushikdutta.async.callback.{CompletedCallback, DataCallback}
import com.koushikdutta.async.future.{Future, SimpleFuture}
import com.koushikdutta.async.http.callback.HttpConnectCallback
import com.koushikdutta.async.http.{AsyncHttpClient, AsyncHttpRequest, AsyncHttpResponse, Headers}
import com.koushikdutta.async.{AsyncServer, AsyncSocket, ByteBufferList}
import com.waz.ZLog.LogTag
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator
import com.waz.testutils.Matchers._
import com.waz.testutils.Slow
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.wrappers.URI
import com.waz.utils.{IoUtils, Json}
import com.waz.znet.ContentEncoder.{EmptyRequestContent, GzippedRequestContent, JsonContentEncoder, MultipartRequestContent, StreamRequestContent}
import com.waz.znet.Response.{DefaultResponseBodyDecoder, HttpStatus, SuccessHttpStatus}
import org.json.JSONObject
import org.scalatest._

import scala.concurrent.Future.successful
import scala.concurrent.duration._
import scala.concurrent.{Await, TimeoutException}
import scala.util.Random

class AsyncClientSpec extends FeatureSpecLike with Matchers with BeforeAndAfter with RobolectricTests {
  implicit val tag: LogTag = "AsyncClientSpec"
  
  val wireMockPort = 9000 + Random.nextInt(3000)
  lazy val wireMockServer = new WireMockServer(WireMockConfiguration.wireMockConfig()
    .port(wireMockPort)
    .httpsPort(4433)
    .withRootDirectory(getClass.getResource("/ZNetClientSpec").getPath)
  )

  implicit lazy val dispatcher = new SerialDispatchQueue

  before {
    wireMockServer.start()
    configureFor("localhost", wireMockPort)
  }

  after {
    wireMockServer.stop()
  }

  def client = new AsyncClient(wrapper = TestClientWrapper)

  @volatile var progress = ProgressIndicator.ProgressData(0L, 0L, State.UNKNOWN)
  @volatile var progressTickCount: Int = 0

  def doGet(path: String, auth: Boolean = false) = Await.result(client(URI.parse(s"http://localhost:$wireMockPort$path"), "GET", EmptyRequestContent, timeout = AsyncClient.DefaultTimeout, decoder = Some(DefaultResponseBodyDecoder), downloadProgressCallback = Some({
    case currentProgress =>
      progressTickCount += 1
      progress = currentProgress
  })), 500.millis)

  def doPost[A: ContentEncoder](path: String, data: A, auth: Boolean = false, timeout: FiniteDuration = AsyncClient.DefaultTimeout, waitTime: FiniteDuration = 500.millis) =
    Await.result(client(URI.parse(s"http://localhost:$wireMockPort$path"), "POST", implicitly[ContentEncoder[A]].apply(data), timeout = timeout), waitTime)

  feature("GET request") {

    scenario("Empty http GET request - 200") {
      doGet("/get/empty200") match {
        case Response(HttpStatus(200, _), EmptyResponse, _) => // expected
        case resp => fail(s"got: $resp when expected Response(HttpStatus(200))")
      }
    }

    scenario("Empty http GET request - 400") {
      doGet("/get/empty400") match {
        case Response(HttpStatus(400, _), EmptyResponse, _) => // expected
        case resp => fail(s"got: $resp when expected Response(HttpStatus(400))")
      }
    }

    scenario("Perform json GET request") {
      val json = """{"result": "ok"}"""
      doGet("/get/json200") match {
        case Response(HttpStatus(200, _), JsonObjectResponse(js), headers) if js.toString == new JSONObject(json).toString && headers("Content-Type").contains("application/json") => info(s"got json response with headers: $headers") //fine
        case r => fail(s"got: $r instead of Response(HttpStatus(200), JsonBody($json))")
      }
    }

    scenario("Perform image GET request") {
      progressTickCount = 0
      doGet("/get/penguin") match {
        case Response(HttpStatus(200, _), _, _) => info(s"got 200") // expected
        case r => fail(s"got: $r instead of 200")
      }
      progressTickCount should be > 0
      progress.current should be > 0L
      progress.current shouldEqual progress.total
      progress.state shouldEqual State.COMPLETED
    }

    scenario("Get gzipped json") {
      import scala.collection.JavaConverters._

      def toMap(js: JSONObject) =
        js.keys().asInstanceOf[java.util.Iterator[String]].asScala.map(key => key -> js.getString(key)).toMap

      val obj = Json((1 to 25).map(i => s"key_$i" -> Seq.tabulate(i)(identity).mkString(",")).toMap)

      val gzipped = IoUtils.gzip(obj.toString.getBytes("utf8"))

      stubFor(get(urlEqualTo("/get/gzipped"))
        .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withHeader("Content-Encoding", "gzip")
        .withBody(gzipped)
        ))

      doGet("/get/gzipped") match {
        case Response(HttpStatus(200, _), JsonObjectResponse(js), headers) => toMap(js) shouldEqual toMap(obj)
        case r => fail(s"got unexpected response: $r")
      }
    }
  }

  feature("POST request") {

    scenario("Empty http POST request - 200") {
      doPost("/post/empty200", {}) match {
        case Response(HttpStatus(200, _), EmptyResponse, _) => // expected
        case resp => fail(s"got: $resp when expected Response(HttpStatus(200))")
      }
    }

    scenario("Post json, receive empty 200") {
      doPost("/post/json_empty200", new JSONObject("""{ "key": "value"}""")) match {
        case Response(HttpStatus(200, _), EmptyResponse, _) => // expected
        case resp => fail(s"got: $resp when expected Response(HttpStatus(200))")
      }
    }

    scenario("Post json, receive json 200") {
      val json = """{"result": "ok"}"""
      doPost("/post/json_json200", new JSONObject("""{ "key": "value"}""")) match {
        case Response(HttpStatus(200, _), JsonObjectResponse(js), _) if js.toString == new JSONObject(json).toString => //fine
        case r => fail(s"got: $r instead of Response(HttpStatus(200), JsonBody($json))")
      }
    }
  }

  feature("Cancelling") {

    scenario("Cancel once completed") {
      val future = client(URI.parse(s"http://localhost:$wireMockPort/get/empty200"), "GET", EmptyRequestContent, timeout = AsyncClient.DefaultTimeout)
      Thread.sleep(100)
      future.cancel()

      Await.result(future, 100.millis) match {
        case Response(HttpStatus(200, _), EmptyResponse, _) => //expected
        case resp => fail(s"got: $resp while expecting Response(HttpStatus(200))")
      }
    }

    scenario("Cancel right away", Slow) {
      val c = client
      (0 to 100) map { _ =>
        val future = c(URI.parse(s"http://localhost:$wireMockPort/get/json200"), "GET", EmptyRequestContent, timeout = AsyncClient.DefaultTimeout)
        future.cancel()
        future
      } foreach { future =>
        intercept[CancelException] {
          Await.result(future, 100.millis)
        }
      }
    }

    scenario("Cancel randomly", Slow) {
      val c = client
      (0 to 255) foreach { _ =>
        val future = c(URI.parse(s"http://localhost:$wireMockPort/get/json200"), "GET", EmptyRequestContent, timeout = AsyncClient.DefaultTimeout)
        Thread.sleep(Random.nextInt(50))
        future.cancel()
        try {
          Await.result(future, 100.millis) match {
            case Response(HttpStatus(200, _), JsonObjectResponse(js), _) => //expected
            case resp => fail(s"got unexpected response: $resp")
          }
        } catch {
          case _: CancelException => // fine
        }
      }
    }

    scenario("Cancel randomly on post", Slow) {
      val c = client
      val data = JsonContentEncoder(new JSONObject("""{ "key": "value"}"""))
      (0 to 255) foreach { _ =>
        val future = c(URI.parse(s"http://localhost:$wireMockPort/post/json_json200"), "POST", data, timeout = AsyncClient.DefaultTimeout)
        Thread.sleep(Random.nextInt(50))
        future.cancel()
        try {
          Await.result(future, 100.millis) match {
            case Response(HttpStatus(200, _), JsonObjectResponse(js), _) => //expected
            case resp => fail(s"got unexpected response: $resp")
          }
        } catch {
          case _: CancelException => // fine
        }
      }
    }

    scenario("Cancel long get") {
      stubFor(get(urlEqualTo("/get/delayed"))
        .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withBody("""{"result":"ok"}""")
        .withFixedDelay(500)
        ))

      val future = client(URI.parse(s"http://localhost:$wireMockPort/get/delayed"), "GET", EmptyRequestContent, timeout = AsyncClient.DefaultTimeout)
      Thread.sleep(100)

      future.cancel()

      intercept[CancelException] {
        Await.result(future, 100.millis)
      }
    }


    scenario("Cancel while streaming POST") {
      val os = new PipedOutputStream()
      val is = new PipedInputStream(os)
      val future = client(URI.parse(s"http://localhost:$wireMockPort/post/json_json200"), "POST", new StreamRequestContent(is, "application/json", -1), timeout = AsyncClient.DefaultTimeout)
      os.write("""{"key":"""".getBytes("utf8"))
      future.cancel()
      os.write("""value"}""".getBytes("utf8"))

      intercept[CancelException] {
        Await.result(future, 100.millis)
      }
    }
  }

  feature("Gzip POST encoding") {

    scenario("Send gzipped json request") {
      stubFor(post(urlEqualTo("/post/gzipped"))
        .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withBody("""{"result":"ok"}""")
        ))

      val obj = Json((1 to 100).map(i => s"key_$i" -> Seq.tabulate(i)(identity).mkString(",")).toMap)

      client(URI.parse(s"http://localhost:$wireMockPort/post/gzipped"), "POST", GzippedRequestContent(obj.toString.getBytes("utf8"), "application/json"), timeout = AsyncClient.DefaultTimeout) should eventually(beMatching {
        case Response(SuccessHttpStatus(), JsonObjectResponse(json), _) if json.getString("result") == "ok" => // fine
      })

      verify(postRequestedFor(urlEqualTo("/post/gzipped")).withHeader("Content-Encoding", equalTo("gzip")))
    }
  }

  feature("User-Agent") {

    scenario("Send user agent with requests") {
      val agent = s"Wire/* (zms *; Android ${VERSION.RELEASE}; $MANUFACTURER $MODEL)"

      verify(getRequestedFor(urlEqualTo("/get/empty200")).withHeader("User-Agent", equalTo(agent)))
      verify(postRequestedFor(urlEqualTo("/post/empty200")).withHeader("User-Agent", equalTo(agent)))
      verify(getRequestedFor(urlEqualTo("/get/delayed")).withHeader("User-Agent", equalTo(agent)))
    }
  }

  feature("Network inactivity timeout") {
    import scala.language.reflectiveCalls

    lazy val mockedResponse = new AsyncHttpResponse {
      @volatile var dataCallback: DataCallback = new NullDataCallback
      @volatile var endCallback: CompletedCallback = new NullCompletedCallback

      override def getRequest: AsyncHttpRequest = null
      override def message(): String = "meep"
      override def code(): Int = 200
      override def protocol(): String = "http"
      override def headers(): Headers = new Headers()
      override def detachSocket(): AsyncSocket = null
      override def getEndCallback: CompletedCallback = endCallback
      override def isPaused: Boolean = false
      override def setDataCallback(callback: DataCallback): Unit = dataCallback = callback
      override def getDataCallback: DataCallback = dataCallback
      override def getServer: AsyncServer = null
      override def isChunked: Boolean = false
      override def charset(): String = "UTF-8"
      override def setEndCallback(callback: CompletedCallback): Unit = endCallback = callback
      override def pause(): Unit = ()
      override def close(): Unit = ()
      override def resume(): Unit = ()
    }

    lazy val mockedClient = new AsyncClient {
      @volatile var onConnect: Option[HttpConnectCallback] = None

      override val client = successful(new AsyncHttpClient(new AsyncServer) {
        override def execute(req: AsyncHttpRequest, callback: HttpConnectCallback): Future[AsyncHttpResponse] = {
          onConnect = Option(callback)
          new SimpleFuture[AsyncHttpResponse](mockedResponse)
        }
      })
    }

    scenario("connect doesn't complete") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      a[TimeoutException] should be thrownBy Await.result(resp, 2.seconds)
    }

    scenario("connect completes, no response is sent") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      Thread.sleep(100L)
      mockedClient.onConnect.foreach { _.onConnectCompleted(null, mockedResponse) }
      a[TimeoutException] should be thrownBy Await.result(resp, 2.seconds)
    }

    scenario("connect completes, response is sent, but no completion") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      Thread.sleep(100L)
      mockedClient.onConnect.foreach { _.onConnectCompleted(null, mockedResponse) }
      Thread.sleep(100L)
      mockedResponse.dataCallback.onDataAvailable(null, new ByteBufferList)
      a[TimeoutException] should be thrownBy Await.result(resp, 2.seconds)
    }

    scenario("connect completes, response is sent, in time completion") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      Thread.sleep(100L)
      mockedClient.onConnect.foreach { _.onConnectCompleted(null, mockedResponse) }
      Thread.sleep(100L)
      mockedResponse.dataCallback.onDataAvailable(null, new ByteBufferList)
      Thread.sleep(100L)
      mockedResponse.endCallback.onCompleted(null)
      Await.result(resp, 2.seconds).status.status shouldEqual 200
    }

    scenario("connect completes, response is sent slowly, in time completion") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      Thread.sleep(100L)
      mockedClient.onConnect.foreach { _.onConnectCompleted(null, mockedResponse) }
      for (i <- 1 to 3) {
        Thread.sleep(500L)
        mockedResponse.dataCallback.onDataAvailable(null, new ByteBufferList)
      }
      Thread.sleep(100L)
      mockedResponse.endCallback.onCompleted(null)
      Await.result(resp, 2.seconds).status.status shouldEqual 200
    }

    scenario("connect completes, response is sent slowly, then takes too long") {
      val resp = mockedClient(URI.parse("http://meep.me"), timeout = 1.second)
      Thread.sleep(100L)
      mockedClient.onConnect.foreach { _.onConnectCompleted(null, mockedResponse) }
      for (i <- 1 to 2) {
        Thread.sleep(500L)
        mockedResponse.dataCallback.onDataAvailable(null, new ByteBufferList)
      }
      Thread.sleep(1100L)
      mockedResponse.dataCallback.onDataAvailable(null, new ByteBufferList)
      Thread.sleep(100L)
      mockedResponse.endCallback.onCompleted(null)
      a[TimeoutException] should be thrownBy Await.result(resp, 2.seconds)
    }

    scenario("multipart POST, connect (incl. sending of request) is very slow") {
      wireMockServer.addRequestProcessingDelay(3000)
      doPost("/post/multi_empty200", MultipartRequestContent(Seq("meep" -> new File(getClass.getResource("/emojis.txt").getPath))), false, 2.seconds, 10.seconds) match {
        case Response(HttpStatus(200, _), _, _) => // expected
        case resp => fail(s"got: $resp when expected Response(HttpStatus(200))")
      }
    }

    scenario("JSON POST, connect (incl. sending of request) is very slow") {
      wireMockServer.addRequestProcessingDelay(3000)
      a[TimeoutException] should be thrownBy doPost("/post/json_json200", new JSONObject("""{ "key": "value"}"""), false, 2.seconds, 10.seconds)
    }
  }
}
