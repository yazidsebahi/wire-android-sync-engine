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
import com.waz.api.impl.{Credentials, EmailCredentials, ErrorResponse}
import com.waz.model.{ZUserId, EmailAddress}
import com.waz.service.Preference
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.AuthenticationManager.Token
import com.waz.znet.LoginClient.LoginResult
import com.waz.znet.Response._
import org.json.JSONObject

import scala.concurrent.Future

trait AccessTokenProvider {
  def currentToken(): Future[Either[Status, Token]]
}

trait CredentialsHandler {
  def userId: ZUserId
  def credentials: Credentials

  def cookie: Option[String]
  
  def updateCookie(cookie: Option[String]): Unit
  def onInvalidCredentials(id: ZUserId): Unit = {}
}

class BasicCredentials(email: EmailAddress, password: Option[String], var cookie: Option[String] = None) extends CredentialsHandler {
  override val userId: ZUserId = ZUserId()

  def credentials: Credentials = EmailCredentials(email, password)

  override def updateCookie(cookie: Option[String]): Unit = this.cookie = cookie
}

/**
 * Manages authentication token, and dispatches login requests when needed.
 * Will retry login request if unsuccessful.
 */
class AuthenticationManager(client: LoginClient, user: CredentialsHandler, initialAccessToken: Option[Token], tokenPref: Preference[Option[Token]]) extends AccessTokenProvider {
  def this(client: LoginClient, email: EmailAddress, passwd: String) = this(client, new BasicCredentials(email, Some(passwd)), None, Preference.empty) // currently only used in integration tests

  import com.waz.znet.AuthenticationManager._

  debug(s"init, \ncredentials: ${user.credentials}")

  implicit val dispatcher = new SerialDispatchQueue(name = "AuthenticationManager")

  private var closed = false

  initialAccessToken foreach { token => tokenPref := Some(token) }

  /**
   * Last login request result. Used to make sure we never send several concurrent login requests.
   */
  private var loginFuture: CancellableFuture[Either[Status, Token]] = CancellableFuture.lift(tokenPref() map { _.fold[Either[Status, Token]](Left(Cancelled))(Right(_)) })

  def currentToken() = tokenPref() flatMap {
    case Some(token) if !isExpired(token) =>
      if (shouldRefresh(token)) performLogin() // schedule login on background and don't care about the result, it's supposed to refresh the access token
      Future.successful(Right(token))
    case _ => performLogin()
  }

  def invalidateToken() = tokenPref() .map (_.foreach { token => tokenPref := Some(token.copy(expiresAt = 0)) })(dispatcher)

  def isExpired(token: Token) = token.expiresAt < System.currentTimeMillis()

  def close() = dispatcher {
    closed = true
    loginFuture.cancel()
  }

  private def shouldRefresh(token: Token) = token.expiresAt - bgRefreshThreshold < System.currentTimeMillis()

  /**
   * Performs login request once the last request is finished, but only if we still need it (ie. we don't have access token already)
   */
  private def performLogin(): Future[Either[Status, Token]] = {
    debug(s"performLogin, \ncredentials: ${user.credentials}")

    loginFuture = loginFuture.recover {
      case ex =>
        warn(s"login failed", ex)
        Left(Cancelled)
    } flatMap { _ =>
      CancellableFuture.lift(tokenPref()) flatMap {
        case Some(token: Token) if !isExpired(token) =>
          if (shouldRefresh(token)) dispatchAccessRequest()
          CancellableFuture.successful(Right(token))
        case _ =>
          debug(s"No access token, or expired, cookie: ${user.cookie}")
          user.cookie.fold(dispatchLoginRequest())(_ => dispatchAccessRequest())
      }
    }
    loginFuture.future
  }

  private def dispatchAccessRequest(): CancellableFuture[Either[Status, Token]] = {
    val userId = user.userId // capture id
    CancellableFuture.lift(tokenPref()) flatMap { token =>
      dispatchRequest(client.access(user.cookie, token)) {
        case Left(resp@ErrorResponse(Status.Forbidden | Status.Unauthorized, message, label)) =>
          debug(s"access request failed (label: $label, message: $message), will try login request. token: $token, cookie: ${user.cookie}, access resp: $resp")
          if (user.userId == userId) {
            user.updateCookie(None)
            tokenPref := None
            dispatchLoginRequest()
          } else CancellableFuture.successful(Left(InternalError("user has changed in the meantime")))
      }
    }
  }

  private def dispatchLoginRequest(): CancellableFuture[Either[Status, Token]] = {
    val userId = user.userId // capture id
    val credentials = user.credentials
    if (credentials.canLogin) {
      dispatchRequest(client.login(userId, credentials)) {
        case Left(resp @ ErrorResponse(Status.Forbidden, _, _)) =>
          debug(s"login request failed with: $resp")
          user.onInvalidCredentials(userId)
          CancellableFuture.successful(Left(HttpStatus(Status.Unauthorized, s"login request failed with: $resp")))
      }
    } else { // no cookie, no password/code, therefore unable to login, don't even try
      assert(user.cookie.isEmpty, "dispatchLoginRequest should only be called if cookie is empty")
      debug("Password or confirmation code missing in dispatchLoginRequest, returning Unauthorized")
      user.onInvalidCredentials(userId)
      CancellableFuture.successful(Left(HttpStatus(Status.Unauthorized, "Password missing in dispatchLoginRequest")))
    }
  }

  private def dispatchRequest(request: => CancellableFuture[LoginResult], retryCount: Int = 0)(handler: ResponseHandler): CancellableFuture[Either[Status, Token]] = {
    request flatMap handler.orElse {
      case Right((token, cookie)) =>
        debug(s"receivedAccessToken: '$token'")
        tokenPref := Some(token) // persist so that we can reuse the access token after app restarts
        cookie.foreach(c => user.updateCookie(Some(c)))
        CancellableFuture.successful(Right(token))

      case Left(_) if closed => CancellableFuture.successful(Left(ClientClosed))

      case Left(err @ ErrorResponse(Cancelled.status, msg, label)) =>
        debug(s"request has been cancelled")
        CancellableFuture.successful(Left(HttpStatus(err.code, s"$msg - $label")))

      case Left(err) if retryCount < MaxRetryCount =>
        info(s"Received error from request: $err, will retry")
        dispatchRequest(request, retryCount + 1)(handler)

      case Left(err) =>
        val msg = s"Login request failed after $retryCount retries, last status: $err"
        error(msg)
        CancellableFuture.successful(Left(HttpStatus(err.code, msg)))
    }
  }
}

object AuthenticationManager {
  private implicit val logTag: LogTag = logTagFor[AuthenticationManager]

  val MaxRetryCount = 3
  val bgRefreshThreshold = 15 * 1000 // refresh access token on background if it is close to expire

  type ResponseHandler = PartialFunction[LoginResult, CancellableFuture[Either[Status, Token]]]

  type Cookie = Option[String]

  case class Token(accessToken: String, tokenType: String, expiresAt: Long = 0) {
    val headers = Map(Token.AuthorizationHeader -> s"$tokenType $accessToken")
    def prepare(req: AsyncHttpRequest) = req.addHeader(Token.AuthorizationHeader, s"$tokenType $accessToken")
  }

  object Token extends ((String, String, Long) => Token ){
    val AuthorizationHeader = "Authorization"

    implicit lazy val Encoder: JsonEncoder[Token] = new JsonEncoder[Token] {
      override def apply(v: Token): JSONObject = JsonEncoder { o =>
        o.put("token", v.accessToken)
        o.put("type", v.tokenType)
        o.put("expires", v.expiresAt)
      }
    }

    implicit lazy val Decoder: JsonDecoder[Token] = new JsonDecoder[Token] {
      import JsonDecoder._
      override def apply(implicit js: JSONObject): Token = Token('token, 'type, 'expires)
    }
  }
}
