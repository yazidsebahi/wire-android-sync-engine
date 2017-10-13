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

import java.net.{ConnectException, UnknownHostException}
import java.security.cert.X509Certificate
import java.util.concurrent.TimeoutException
import javax.net.ssl.{SSLContext, SSLEngine, SSLException, X509TrustManager}

import com.google.android.gms.security.ProviderInstaller
import com.koushikdutta.async.AsyncServer
import com.koushikdutta.async.http.AsyncHttpClient.WebSocketConnectCallback
import com.koushikdutta.async.http._
import com.koushikdutta.async.http.callback.HttpConnectCallback
import com.waz.HockeyApp.NoReporting
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.model.AccountId
import com.waz.model.otr.ClientId
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.{returning, _}
import com.waz.utils.wrappers.{Context, URI}
import com.waz.znet.ContentEncoder.MultipartRequestContent
import com.waz.znet.Response.{DefaultResponseBodyDecoder, ResponseBodyDecoder}
import org.apache.http.conn.ssl.{AbstractVerifier, StrictHostnameVerifier}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Left, Try}
import scala.util.control.NonFatal

trait HttpClient {

  def execute(request: Request[_]): CancellableFuture[Response]

  def apply(request: Request[_]): CancellableFuture[Response] = execute(request)

  def close(): Unit

  def websocket(accountId: AccountId, pushUri: URI, auth: AuthenticationManager): CancellableFuture[WireWebSocket]
}

class HttpClientImpl(context:         Context,
                     decoder:         ResponseBodyDecoder = DefaultResponseBodyDecoder,
                     userAgent:       String              = HttpClient.userAgent()) extends HttpClient {
  import HttpClient._
  import HttpClientImpl._

  protected implicit val dispatcher = new SerialDispatchQueue(name = "HttpClient")

  private val requestWorker  = new HttpRequestImplWorker
  private val responseWorker = new ResponseImplWorker
  private val client         = Future(returning(new AsyncHttpClient(new AsyncServer))(c => init(c, context)))

  override def execute(request: Request[_]): CancellableFuture[Response] = {
    val body = request.getBody
    debug(s"Starting request[${request.httpMethod}](${request.absoluteUri}) with body: '${if (body.toString.contains("password")) "<body>" else body}', headers: '${request.headers}'")

    val requestTimeout = if (request.httpMethod != Request.PostMethod) request.timeout else body match {
      case _: MultipartRequestContent => MultipartPostTimeout
      case _ => request.timeout
    }

    CancellableFuture.lift(client) flatMap { c =>
      val p = Promise[Response]()
      @volatile var cancelled = false
      @volatile var processFuture = None: Option[CancellableFuture[_]]
      @volatile var lastNetworkActivity: Long = System.currentTimeMillis
      @volatile var timeoutForPhase = requestTimeout
      val interval = 5.seconds min request.timeout

      val ua = request.headers.getOrElse(HttpClient.UserAgentHeader, userAgent)
      val requestBuilt = requestWorker.processRequest(
        request.withTimeout(0.millis).withHeaders(Map(HttpClient.UserAgentHeader -> ua))
      ) // switching off the AsyncHttpClient's timeout - we will use our own
      debug(s"request headers: ${requestBuilt.headers}")

      val httpFuture = c.execute(requestBuilt, new HttpConnectCallback {
        override def onConnectCompleted(ex: Exception, response: AsyncHttpResponse): Unit = {
          debug(s"Connect completed for uri: '${request.absoluteUri}', ex: '$ex', cancelled: $cancelled")
          timeoutForPhase = request.timeout

          if (ex != null) {
            p.tryFailure(if (cancelled) CancellableFuture.DefaultCancelException else ex)
          } else {
            val networkActivityCallback = () => lastNetworkActivity = System.currentTimeMillis
            debug(s"got connection response for request: ${request.absoluteUri}")
            val future = responseWorker.processResponse(request.absoluteUri, response, request.decoder.getOrElse(decoder), request.downloadCallback, networkActivityCallback)
            p.tryCompleteWith(future)

            // XXX: order is important here, we first set processFuture and then check cancelled to avoid race condition in cancel callback
            processFuture = Some(future)
            if (cancelled) {
              debug("cancelled == true, cancelling...")
              future.cancel()
            }
          }
        }
      })

      returning(new CancellableFuture(p) {
        override def cancel()(implicit tag: LogTag): Boolean = {
          debug(s"cancelling request for ${request.absoluteUri}")(tag)
          cancelled = true
          httpFuture.cancel()
          processFuture.foreach(_.cancel()(tag))
          super.cancel()(tag)
        }
      }.recover(exceptionStatus)) { cancellable =>
        def cancelOnInactivity: CancellableFuture[Unit] = {
          val timeSinceLastNetworkActivity = System.currentTimeMillis - lastNetworkActivity
          val t = timeoutForPhase
          if (timeSinceLastNetworkActivity > t.toMillis) CancellableFuture.successful {
            debug(s"cancelling due to inactivity: $timeSinceLastNetworkActivity")
            cancellable.fail(new TimeoutException("[AsyncClient] timedOut") with NoReporting)
          } else CancellableFuture.delay(interval min (t - timeSinceLastNetworkActivity.millis)) flatMap { _ => cancelOnInactivity }
        }

        val cancelOnTimeout = CancellableFuture.delay(interval) flatMap { _ => cancelOnInactivity }
        cancellable.onComplete { _ => cancelOnTimeout.cancel() }
      }
    }
  }

  def close(): Unit = client foreach { _.getServer.stop() }

  override def websocket(accountId: AccountId, pushUri: URI, auth: AuthenticationManager) = {

    CancellableFuture.lift(auth.currentToken()).flatMap {
      case Right(token) =>
        val p = Promise[WireWebSocket]()
        val uri = URI.unwrap(pushUri)
        debug(s"Sending webSocket request: ${uri.toString}")
        val req = token.prepare(new AsyncHttpGet(uri))
        req.setHeader("Accept-Encoding", "identity") // XXX: this is a hack for Backend In The Box problem: 'Accept-Encoding: gzip' header causes 500
        req.setHeader("User-Agent", userAgent)

        CancellableFuture.lift(client).flatMap { c =>
          val f = c.websocket(req, null, new WebSocketConnectCallback {
            override def onCompleted(ex: Exception, socket: WebSocket): Unit = {
              debug(s"WebSocket request finished, ex: $ex, socket: $socket")
              p.tryComplete(if (ex == null) Try(new WebSocketClient(accountId, socket)) else Failure(ex))
            }
          })
          returning(new CancellableFuture(p).withTimeout(30.seconds)) { _.onFailure { case _ => f.cancel() } }
        }
      case Left(status) =>
        CancellableFuture.failed(new Exception(s"Authentication returned error status: $status"))
    }
  }
}

object HttpClient {

  val domains @ Seq(zinfra, wire) = Seq("zinfra.io", "wire.com")
  val protocol = "TLSv1.2"
  val cipherSuite = "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384"

  val MultipartPostTimeout = 15.minutes
  val DefaultTimeout = 30.seconds
  val EmptyHeaders = Map[String, String]()

  val UserAgentHeader = "User-Agent"
  val ContentTypeHeader = "Content-Type"

  def userAgent(appVersion: String = "*", zmsVersion: String = "*") = {
    import android.os.Build._
    s"Wire/$appVersion (zms $zmsVersion; Android ${VERSION.RELEASE}; $MANUFACTURER $MODEL)"
  }

}

object HttpClientImpl {

  import HttpClient._
  import Threading.Implicits.Background

  private def exceptionStatus: PartialFunction[Throwable, Response] = {
    case e: ConnectException => Response(Response.ConnectionError(e.getMessage))
    case e: UnknownHostException => Response(Response.ConnectionError(e.getMessage))
    case e: ConnectionClosedException => Response(Response.ConnectionError(e.getMessage))
    case e: ConnectionFailedException => Response(Response.ConnectionError(e.getMessage))
    case e: RedirectLimitExceededException => Response(Response.ConnectionError(e.getMessage))
    case e: TimeoutException => Response(Response.ConnectionError(e.getMessage))
    case e: CancelException => Response(Response.Cancelled)
    case NonFatal(e) => Response(Response.InternalError(e.getMessage, Some(e)))
  }

  private def init(client: AsyncHttpClient, context: Context): Unit = {
    val installGmsCoreOpenSslProvider = Future (try {
      ProviderInstaller.installIfNeeded(context)
    } catch {
      case t: Throwable => debug("Looking up GMS Core OpenSSL provider failed fatally.") // this should only happen in the tests
    })

    installGmsCoreOpenSslProvider map { _ =>
      // using specific hostname verifier to ensure compatibility with `isCertForDomain` (below)
      client.getSSLSocketMiddleware.setHostnameVerifier(new StrictHostnameVerifier)
      client.getSSLSocketMiddleware.setTrustManagers(Array(new X509TrustManager {
        override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {
          debug(s"checking certificate for authType $authType, name: ${chain(0).getSubjectDN.getName}")
          chain.headOption.fold(throw new SSLException("expected at least one certificate!")) { cert =>
            val tm = if (isCertForDomain(zinfra, cert) || isCertForDomain(wire, cert)) {
              verbose("using backend trust manager")
              ServerTrust.backendTrustManager
            } else {
              verbose("using system trust manager")
              ServerTrust.systemTrustManager
            }
            try {
              tm.checkServerTrusted(chain, authType)
            } catch {
              case e: Throwable =>
                error("certificate check failed", e)
                throw e
            }
          }
        }

        override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = throw new SSLException("unexpected call to checkClientTrusted")

        override def getAcceptedIssuers: Array[X509Certificate] = throw new SSLException("unexpected call to getAcceptedIssuers")

        /**
          * Checks if certificate matches given domain.
          * This is used to check if currently verified server is known to wire, and we should do certificate pinning for it.
          *
          * Warning: it's very important that this implementation matches used HostnameVerifier.
          * If HostnameVerifier accepts this cert with some wire sub-domain then this function must return true,
          * otherwise pinning will be skipped and we risk MITM attack.
          */
        private def isCertForDomain(domain: String, cert: X509Certificate): Boolean = {
          def iter(arr: Array[String]) = Option(arr).fold2(Iterator.empty, _.iterator)
          (iter(AbstractVerifier.getCNs(cert)) ++ iter(AbstractVerifier.getDNSSubjectAlts(cert))).exists(_.endsWith(s".$domain"))
        }
      }))

      client.getSSLSocketMiddleware.setSSLContext(returning(SSLContext.getInstance("TLSv1.2")) { _.init(null, null, null) })

      client.getSSLSocketMiddleware.addEngineConfigurator(new AsyncSSLEngineConfigurator {

        override def createEngine(sslContext: SSLContext, peerHost: String, peerPort: Int) = null

        override def configureEngine(engine: SSLEngine, data: AsyncHttpClientMiddleware.GetSocketData, host: String, port: Int): Unit = {
          debug(s"configureEngine($host, $port)")

          if (domains.exists(host.endsWith)) {
            verbose("restricting to TLSv1.2")
            engine.setSSLParameters(returning(engine.getSSLParameters) { params =>
              if (engine.getSupportedProtocols.contains(protocol)) params.setProtocols(Array(protocol))
              else warn(s"$protocol not supported by this device, falling back to defaults.")

              if (engine.getSupportedCipherSuites.contains(cipherSuite)) params.setCipherSuites(Array(cipherSuite))
              else warn(s"cipher suite $cipherSuite not supported by this device, falling back to defaults.")
            })
          }
        }
      })

    }
  }
}
