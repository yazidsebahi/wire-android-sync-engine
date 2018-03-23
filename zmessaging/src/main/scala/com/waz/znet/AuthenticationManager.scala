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

import com.koushikdutta.async.http.AsyncHttpRequest
import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.content.AccountStorage
import com.waz.model.{AccountData, UserId}
import com.waz.service.ZMessaging.{accountTag, clock}
import com.waz.service.tracking.TrackingService
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.JsonEncoder.encodeInstant
import com.waz.utils.events.EventStream
import com.waz.utils.{JsonDecoder, JsonEncoder, _}
import com.waz.znet.AuthenticationManager.AccessToken
import com.waz.znet.LoginClient.LoginResult
import com.waz.znet.Response._
import com.waz.znet.ZNetClient.ErrorOr
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._

trait AccessTokenProvider {
  def currentToken(): ErrorOr[AccessToken]
  def checkLoggedIn(token: Option[AccessToken]): ErrorOr[AccessToken]
}

/**
 * Manages authentication token, and dispatches login requests when needed.
 * Will retry login request if unsuccessful.
 */
class AuthenticationManager(id: UserId, accStorage: AccountStorage, client: LoginClient, tracking: TrackingService) extends AccessTokenProvider {

  lazy implicit val logTag: LogTag = accountTag[AuthenticationManager](id)

  import com.waz.znet.AuthenticationManager._

  implicit val dispatcher = new SerialDispatchQueue(name = "AuthenticationManager")

  val onInvalidCredentials = EventStream[Unit]()

  private def token  = withAccount(_.accessToken)
  private def cookie = withAccount(_.cookie)

  private def withAccount[A](f: (AccountData) => A): Future[A] = accStorage.get(id).map {
    case Some(acc) => f(acc)
    case _         => throw new IllegalStateException(s"Could not find matching account for: $id")
  }

  //Only performs safe update - never wipes either the cookie or the token.
  private def updateCredentials(token: Option[AccessToken] = None, cookie: Option[Cookie] = None) =
    accStorage.update(id, acc => acc.copy(accessToken = if (token.isDefined) token else acc.accessToken, cookie = cookie.getOrElse(acc.cookie)))


  private def wipeCredentials() =
    accStorage.remove(id)

  /**
   * Last login request result. Used to make sure we never send several concurrent login requests.
   */
  private var loginFuture: ErrorOr[AccessToken] = token.map(_.fold2(Left(ErrorResponse.internalError("No token currently set")), Right(_))) //TODO will this cause lots of failures?

  def invalidateToken() = token.map(_.foreach(t => updateCredentials(Some(t.copy(expiresAt = Instant.EPOCH)))))

  def isExpired(token: AccessToken) = (token.expiresAt - ExpireThreshold) isBefore clock.instant()

  /**
   * Returns current token if not expired or performs login request
   */
  override def currentToken() = {
    loginFuture = loginFuture.recover {
      case ex =>
        warn(s"login failed", ex)
        Left(ErrorResponse.internalError(s"Exception while trying to login: ${ex.getCause}"))
    } flatMap { _ =>
      token.flatMap {
        case Some(token) if !isExpired(token) =>
          verbose(s"Non expired token: $token")
          CancellableFuture.successful(Right(token))
        case token => checkLoggedIn(token)
      }
    }
    loginFuture
  }

  /**
    * Forces an access request to check if the current cookie is valid. Can be used to see if a password reset was successful
    */
  override def checkLoggedIn(token: Option[AccessToken]) =
    cookie.flatMap { cookie =>
      debug(s"Non existent or potentially expired token: $token, will attempt to refresh with cookie: $cookie")
      dispatchRequest(client.access(cookie, token).future) {
        case Left(resp @ ErrorResponse(Status.Forbidden | Status.Unauthorized, message, label)) =>
          verbose(s"access request failed (label: $label, message: $message), will try login request. currToken: $token, cookie: $cookie, access resp: $resp")
          tracking.exception(new RuntimeException(s"Access request failed: msg: $message, label: $label, cookie expired at: ${cookie.expiry} (is valid: ${cookie.isValid}), currToken expired at: ${token.map(_.expiresAt)} (is valid: ${token.exists(_.isValid)})"), null)
          wipeCredentials().map(_ => Left(resp))
      }
    }

  private def dispatchRequest(request: => Future[LoginResult], retryCount: Int = 0)(handler: ResponseHandler): ErrorOr[AccessToken] =
    request.flatMap(handler.orElse {
      case Right((token, cookie)) =>
        debug(s"receivedAccessToken: '$token'")
        updateCredentials(Some(token), cookie).map(_ => Right(token))

      case Left(err @ ErrorResponse(Cancelled.status, msg, label)) =>
        debug(s"request has been cancelled")
        Future.successful(Left(err))

      case Left(err) if retryCount < MaxRetryCount =>
        info(s"Received error from request: $err, will retry")
        dispatchRequest(request, retryCount + 1)(handler)

      case Left((_, err)) =>
        val msg = s"Login request failed after $retryCount retries, last status: $err"
        error(msg)
        Future.successful(Left(err))
    })
}

object AuthenticationManager {
  val MaxRetryCount = 3
  val ExpireThreshold = 15.seconds // refresh access token on background if it is close to expire

  type ResponseHandler = PartialFunction[LoginResult, ErrorOr[AccessToken]]

  case class Cookie(str: String) {

    private val parts = str.split('.').toSet
    val headers = Map(LoginClient.Cookie -> s"zuid=$str")
    val expiry = find("d=").map(v => Instant.ofEpochSecond(v.toLong))
    def isValid = expiry.exists(_.isAfter(Instant.now))

    private def find(pref: String) = parts.find(_.contains(pref)).map(_.drop(2))

    override def toString = s"${str.take(10)}, exp: $expiry, isValid: $isValid"
  }

  case class AccessToken(accessToken: String, tokenType: String, expiresAt: Instant) {
    val headers = Map(AccessToken.AuthorizationHeader -> s"$tokenType $accessToken")
    def prepare(req: AsyncHttpRequest) = req.addHeader(AccessToken.AuthorizationHeader, s"$tokenType $accessToken")

    def isValid = expiresAt isAfter clock.instant()

    override def toString = s"${accessToken.take(10)}, exp: $expiresAt, isValid: $isValid"
  }

  object AccessToken extends ((String, String, Instant) => AccessToken ){
    val AuthorizationHeader = "Authorization"

    implicit lazy val Encoder: JsonEncoder[AccessToken] = new JsonEncoder[AccessToken] {
      override def apply(v: AccessToken): JSONObject = JsonEncoder { o =>
        o.put("token", v.accessToken)
        o.put("type", v.tokenType)
        o.put("expires", encodeInstant(v.expiresAt))
      }
    }

    implicit lazy val Decoder: JsonDecoder[AccessToken] = new JsonDecoder[AccessToken] {
      import JsonDecoder._
      override def apply(implicit js: JSONObject): AccessToken = AccessToken('token, 'type, 'expires)
    }
  }
}
