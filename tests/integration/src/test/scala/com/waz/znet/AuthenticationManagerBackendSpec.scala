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

import java.io.File

import com.waz.ShadowLogging
import com.waz.content.{ZStorage, KeyValueStorage}
import com.waz.model.{ZUserId, EmailAddress}
import com.waz.provision.ProvisionedSuite
import com.waz.service._
import com.waz.testutils.Matchers._
import com.waz.znet.AuthenticationManager.Token
import org.robolectric.Robolectric
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class AuthenticationManagerBackendSpec extends FeatureSpec with Matchers with BeforeAndAfter with ProvisionedSuite with ShadowLogging with RobolectricTests { test =>

  override val provisionFile = "/one_user.json"
  override protected lazy val logfileBaseDir: File = new File("target/logcat/integration")

  lazy val globalModule: GlobalModule = new GlobalModule(Robolectric.application, BackendConfig.EdgeBackend) {
    override lazy val clientWrapper: ClientWrapper = TestClientWrapper
  }

  lazy val storage = new ZStorage(ZUserId(), Robolectric.application)
  lazy val keyValue = new KeyValueService(new KeyValueStorage(Robolectric.application, storage), new ReportingService)

  lazy val client = new LoginClient(new AsyncClient(wrapper = TestClientWrapper), BackendConfig.EdgeBackend)

  lazy val email = provisionedEmail("auto1")
  lazy val passwd = "auto1_pass"

  lazy val credentials = new BasicCredentials(EmailAddress(email), Some(passwd))

  var accessToken = Option.empty[Token]

  feature("Cookie") {
    scenario("Load cookie") {
      val auth = new AuthenticationManager(client, credentials, initialAccessToken = None, keyValue.accessTokenPref)
      Await.result(auth.currentToken(), 5.seconds) match {
        case Right(token) =>
          accessToken = Some(token)
          credentials.cookie match {
            case Some(c) => info(s"Got cookie: $c")
            case _ => fail("didn't get the cookie")
          }
        case resp => fail(s"Got unexpected response: $resp")
      }
    }

    scenario("Access token is persisted") {
      keyValue.accessTokenPref() should eventually(be(accessToken))
    }

    scenario("Access token is reused") {
      val auth = new AuthenticationManager(client, credentials, initialAccessToken = None, keyValue.accessTokenPref)
      Await.result(auth.currentToken(), 5.seconds) match {
        case Right(token) if accessToken contains token => info(s"Reused access token: $token")
        case resp => fail("didn't reuse access token")
      }
    }

    scenario("Login with previously loaded cookie") {
      val cr = new BasicCredentials(EmailAddress(email), None, credentials.cookie)
      val auth = new AuthenticationManager(client, cr, initialAccessToken = None, Preference.empty)

      Await.result(auth.currentToken(), 5.seconds) match {
        case Right(token) =>
          credentials.cookie match {
            case Some(c) => info(s"Got cookie: $c")
            case _ => fail("didn't get the cookie")
          }
        case resp => fail(s"Got unexpected response: $resp")
      }
    }
  }
}
