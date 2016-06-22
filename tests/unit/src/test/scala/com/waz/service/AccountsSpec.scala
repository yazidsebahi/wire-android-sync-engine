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
package com.waz.service

import com.waz.api.impl.{Credentials, EmailCredentials, ErrorResponse}
import com.waz.model._
import com.waz.testutils.{DefaultPatienceConfig, MockAccounts, MockGlobalModule}
import com.waz.threading.CancellableFuture
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import com.waz.znet.LoginClient
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class AccountsSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with ScalaFutures with DefaultPatienceConfig {

  var loginRequest: Option[Credentials] = _
  var loginResponse: Either[ErrorResponse, (Token, Cookie)] = _

  lazy val global = new MockGlobalModule {
    override lazy val loginClient: LoginClient = new LoginClient(client, BackendConfig.EdgeBackend) {
      override def login(userId: AccountId, credentials: Credentials): CancellableFuture[Either[ErrorResponse, (Token, Cookie)]] = {
        loginRequest = Some(credentials)
        CancellableFuture.successful(loginResponse)
      }
    }
  }
  def accounts = new MockAccounts(global)

  before {
    loginRequest = None
    loginResponse = Left(ErrorResponse(0, "", ""))
  }

  feature("login") {

    scenario("login with unverified user") {
      loginResponse = Left(ErrorResponse(403, "message", "pending-activation"))

      val creds = EmailCredentials(EmailAddress("email"), Some("pass"))
      Await.result(accounts.login(creds), 2.seconds) match {
        case Right(data) =>
          data.email shouldEqual Some(EmailAddress("email"))
          data.credentials shouldEqual creds
          data.activated shouldEqual false
          data.userId shouldBe empty
        case res =>
          fail(s"login returned: $res")
      }
    }
  }
}
