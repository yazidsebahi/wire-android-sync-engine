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

import java.security.cert.X509Certificate
import javax.net.ssl._

import com.koushikdutta.async.http.{AsyncHttpClient, AsyncHttpClientMiddleware, AsyncSSLEngineConfigurator}
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.ZLog._

import scala.concurrent.Future

/**
 * Replaces ssl context, trust managers and verifiers for Robolectric testing.
 * Default ssl implementation in AndroidAsync doesn't work correctly when run in RoboTests, so we need to hack it a bit.
  */
object TestClientWrapper extends ClientWrapper {
  def apply(client: AsyncHttpClient): Future[AsyncHttpClient] = Future {
    client.getSSLSocketMiddleware.setSSLContext(returning(SSLContext.getInstance("TLSv1.2")) { _.init(null, null, null) })

    client.getSSLSocketMiddleware.setTrustManagers(Array(new X509TrustManager {
      override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {}

      override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = throw new SSLException("unexpected call to checkClientTrusted")

      override def getAcceptedIssuers: Array[X509Certificate] = throw new SSLException("unexpected call to getAcceptedIssuers")
    }))

    client.getSSLSocketMiddleware.addEngineConfigurator(new AsyncSSLEngineConfigurator {
      override def configureEngine(engine: SSLEngine, data: AsyncHttpClientMiddleware.GetSocketData, host: String, port: Int): Unit = {
        debug(s"configureEngine: $host:$port")(logTagFor(TestClientWrapper))
        val peerHost = engine.getClass.getSuperclass.getDeclaredField("peerHost")
        peerHost.setAccessible(true)
        peerHost.set(engine, host)

        val peerPort = engine.getClass.getSuperclass.getDeclaredField("peerPort")
        peerPort.setAccessible(true)
        peerPort.setInt(engine, port)
      }
    })

    client
  } (Threading.Background)
}
