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

import com.waz.api.impl.{EmailCredentials, ErrorResponse}
import com.waz.content.Preferences.Preference
import com.waz.model.EmailAddress
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.returning
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import com.waz.znet.Response.{HttpStatus, Status}

import scala.concurrent.Future

class AuthenticationManagerSpec extends AndroidFreeSpec {

  val loginClient = mock[LoginClient]
  val credentials = mock[CredentialsHandler]


  feature("Successful logins") {
    scenario("Return authentication token if valid") {

      val token = Token("token", "token", System.currentTimeMillis() + AuthenticationManager.ExpireThreshold + 1000)

      (credentials.credentials _).expects().returning(EmailCredentials(EmailAddress("blah@blah.com"), Some("password")))
      (credentials.accessToken _).expects().returning(Preference.inMemory(Option(token)))
      val manager = getManager

      result(manager.currentToken()) shouldEqual Right(token)
    }

    scenario("Request new token if old token is invalid") {

      val cookie = Cookie("cookie")

      val oldToken = Token("token", "token", System.currentTimeMillis())
      val newToken = Token("newToken", "token", System.currentTimeMillis() + 15 * 60 * 1000L)

      (credentials.credentials _).expects().returning(EmailCredentials(EmailAddress("blah@blah.com"), Some("password")))
      (credentials.accessToken _).expects().returning(Preference.inMemory(Option(oldToken)))
      (credentials.cookie _).expects().returning(Preference.inMemory(Option(cookie)))

      (loginClient.access _).expects(cookie, Some(oldToken)).returning(CancellableFuture.successful(Right(newToken, None)))

      val manager = getManager

      result(manager.currentToken()) shouldEqual Right(newToken)
    }

    scenario("Cookie in access response should be saved") {

      val oldCookie = Cookie("oldCookie")
      val newCookie = Cookie("newCookie")

      val oldToken = Token("token", "token", System.currentTimeMillis())
      val newToken = Token("newToken", "token", System.currentTimeMillis() + 15 * 60 * 1000L)

      val cookiePref = Preference.inMemory(Option(oldCookie))

      (credentials.credentials _).expects().returning(EmailCredentials(EmailAddress("blah@blah.com"), Some("password")))
      (credentials.accessToken _).expects().returning(Preference.inMemory(Option(oldToken)))
      (credentials.cookie _).expects().twice().returning(cookiePref)

      (loginClient.access _).expects(oldCookie, Some(oldToken)).returning(CancellableFuture.successful(Right(newToken, Some(newCookie))))

      val manager = getManager

      result(manager.currentToken()) shouldEqual Right(newToken)
      result(cookiePref.apply()) shouldEqual Some(newCookie)
    }

    scenario("Multiple calls to access should only trigger at most one request") {
      val cookie = Cookie("cookie")

      val oldToken = Token("token", "token", System.currentTimeMillis())
      val newToken = Token("newToken", "token", System.currentTimeMillis() + 15 * 60 * 1000L)

      val accessTokenPref = Preference.inMemory(Option(oldToken))
      val cookiePref      = Preference.inMemory(Option(cookie))

      (credentials.credentials _).expects().anyNumberOfTimes().returning(EmailCredentials(EmailAddress("blah@blah.com"), None))

      (credentials.accessToken _).expects().anyNumberOfTimes().returning(accessTokenPref)
      (credentials.cookie _).expects().anyNumberOfTimes().returning(cookiePref)

      (loginClient.access _).expects(cookie, Some(oldToken)).returning(CancellableFuture.successful(Right(newToken, None)))

      val manager = getManager

      import Threading.Implicits.Background
      val futures = Future.sequence((1 to 10).map(_ => manager.currentToken()))

      result(futures).foreach(_ shouldEqual Right(newToken))
    }
  }

  feature("Insufficient login information") {
    scenario("Logout after providing invalid credentials when new token is needed") {

      val cookie = Cookie("cookie")

      val oldToken = Token("token", "token", System.currentTimeMillis())

      (credentials.credentials _).expects().twice().returning(EmailCredentials(EmailAddress("blah@blah.com"), None))
      (credentials.accessToken _).expects().twice().returning(Preference.inMemory(Option(oldToken)))
      (credentials.cookie _).expects().twice().returning(Preference.inMemory(Option(cookie)))
      (credentials.onInvalidCredentials _).expects()

      (loginClient.access _).expects(cookie, Some(oldToken)).returning(CancellableFuture.successful(Left((Option(""), ErrorResponse(Status.Forbidden, "", "")))))

      val manager = getManager

      result(manager.currentToken()) shouldEqual Left(HttpStatus(401, "Password missing in dispatchLoginRequest"))
    }
  }

  feature("Failures") {
    scenario("Retry if login not successful for unknown reasons") {

      val cookie = Cookie("cookie")

      val oldToken = Token("token", "token", System.currentTimeMillis())
      val newToken = Token("newToken", "token", System.currentTimeMillis() + 15 * 60 * 1000L)

      (credentials.credentials _).expects().once().returning(EmailCredentials(EmailAddress("blah@blah.com"), None))
      (credentials.accessToken _).expects().once().returning(Preference.inMemory(Option(oldToken)))
      (credentials.cookie _).expects().once().returning(Preference.inMemory(Option(cookie)))

      var attempts = 0

      (loginClient.access _).expects(cookie, Some(oldToken)).anyNumberOfTimes().onCall { (cookie, token) =>
        returning(CancellableFuture.successful(attempts match {
          case 0|1|2 => Left((Option(""), ErrorResponse(500, "Some server error", "Some server error")))
          case 3     => Right((newToken, None))
          case _     => fail("Unexpected number of access attempts")
        }))(_ => attempts += 1)
      }

      val manager = getManager

      result(manager.currentToken()) shouldEqual Right(newToken)
    }
  }

  def getManager = new AuthenticationManager(loginClient, credentials)
}
