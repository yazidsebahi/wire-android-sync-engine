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
package com.waz.client

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api._
import com.waz.api.impl.ErrorResponse
import com.waz.client.RegistrationClientImpl.ActivateResult
import com.waz.model._
import com.waz.service.BackendConfig
import com.waz.sync.client.UsersClient.UserResponseExtractor
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonEncoder
import com.waz.utils.Locales._
import com.waz.znet.AuthenticationManager.Cookie
import com.waz.znet.ContentEncoder.JsonContentEncoder
import com.waz.znet.Response.{Status, SuccessHttpStatus}
import com.waz.znet.ZNetClient.{ErrorOr, ErrorOrResponse}
import com.waz.znet._

import scala.concurrent.Future
import scala.concurrent.duration._

trait RegistrationClient {
  def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean = false): Future[ActivateResult]
  def requestEmailCode(email: EmailAddress): Future[ActivateResult] //for now only used for registration

  def verifyRegistrationMethod(method: Either[PhoneNumber, EmailAddress], code: ConfirmationCode, dryRun: Boolean): ErrorOr[Unit]

  def register(credentials: Credentials, name: String, accentId: Option[Int]): ErrorOrResponse[(UserInfo, Option[Cookie])]
  def registerTeamAccount(account: AccountDataOld): ErrorOrResponse[(UserInfo, Option[Cookie])]
}

class RegistrationClientImpl(client: AsyncClient, backend: BackendConfig) extends RegistrationClient {
  import Threading.Implicits.Background
  import com.waz.client.RegistrationClientImpl._

  def register(credentials: Credentials, name: String, accentId: Option[Int]): ErrorOrResponse[(UserInfo, Option[Cookie])] = {
    val json = JsonEncoder { o =>
      o.put("name", name)
      accentId foreach (o.put("accent_id", _))
      o.put("locale", bcp47.languageTagOf(currentLocale))
      credentials.addToRegistrationJson(o)
    }

    val request = Request.Post(RegisterPath, JsonContentEncoder(json), baseUri = Some(backend.baseUrl), timeout = timeout)
    client(request) map {
      case resp @ Response(SuccessHttpStatus(), UserResponseExtractor(user), headers) =>
        debug(s"registration succeeded: $resp")
        Right((user, LoginClient.getCookieFromHeaders(headers))) //cookie will be optional depending on login method
      case Response(_, ErrorResponse(code, msg, label), headers) =>
        info(s"register failed with error: ($code, $msg, $label), headers: $headers")
        Left(ErrorResponse(code, msg, label))
      case resp =>
        error(s"Unexpected response from register: $resp")
        Left(ErrorResponse(resp.status.status, resp.toString, "unknown"))
    }
  }

  //TODO merge this method with the above register method when safe to do so
  override def registerTeamAccount(account: AccountDataOld) = {
    import account._
    (name, pendingTeamName, pendingEmail, password, code) match {
      case (Some(n), Some(teamName), Some(e), Some(p), Some(c)) =>
        val json = JsonEncoder { o =>
          o.put("name", n)
          o.put("locale", bcp47.languageTagOf(currentLocale))
          o.put("label", account.id.str)  // this label can be later used for cookie revocation
          o.put("email", e.str)
          o.put("password", p)
          o.put("email_code", c.str)
          o.put("team", JsonEncoder { o2 =>
            o2.put("icon", "abc")
            o2.put("name", teamName)
          })
        }

        val request = Request.Post(RegisterPath, JsonContentEncoder(json), baseUri = Some(backend.baseUrl), timeout = timeout)
        client(request) map {
          case resp @ Response(SuccessHttpStatus(), UserResponseExtractor(user), headers) =>
            debug(s"registration succeeded: $resp")
            Right((user, LoginClient.getCookieFromHeaders(headers)))
          case Response(_, ErrorResponse(code, msg, label), headers) =>
            info(s"register failed with error: ($code, $msg, $label), headers: $headers")
            Left(ErrorResponse(code, msg, label))
          case resp =>
            error(s"Unexpected response from register: $resp")
            Left(ErrorResponse(resp.status.status, resp.toString, "unknown"))
        }
      case _ => CancellableFuture.successful(Left(ErrorResponse(
        ErrorResponse.InternalErrorCode,
        "Email, name, password, confirmation code and a team name are needed to complete account registration for a team",
        "insufficient team account information")))
    }
  }

  override def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean) =
    requestCode(Left(phone), login, call)

  override def requestEmailCode(email: EmailAddress) =
    requestCode(Right(email))

  //note, login and call only apply to PhoneNumber and are always false for email addresses
  private def requestCode(method: Either[PhoneNumber, EmailAddress], login: Boolean = false, call: Boolean = false) = {
    val params = JsonEncoder { o =>
      method.fold(p => o.put("phone", p.str), e => o.put("email",  e.str))
      if (!login) o.put("locale", bcp47.languageTagOf(currentLocale))
      if (call)   o.put("voice_call", call)
    }

    client(Request.Post(if (login) LoginSendPath else ActivateSendPath, JsonContentEncoder(params), baseUri = Some(backend.baseUrl), timeout = timeout)).map {
      case resp @ Response(SuccessHttpStatus(), _, _) =>
        debug(s"confirmation code requested: $resp")
        ActivateResult.Success
      case Response(_, ErrorResponse(Status.Forbidden, _, "password-exists"), headers) =>
        ActivateResult.PasswordExists
      case Response(_, ErrorResponse(code, msg, label), headers) =>
        warn(s"postActivateSend: login=$login failed with error: ($code, $msg, $label), headers: $headers")
        ActivateResult.Failure(ErrorResponse(code, msg, label))
      case other =>
        error(s"Unexpected response from postActivateSend: login=$login: $other")
        ActivateResult.Failure(ErrorResponse(other.status.status, other.toString, "unknown"))
    }.future
  }

  override def verifyRegistrationMethod(method: Either[PhoneNumber, EmailAddress], code: ConfirmationCode, dryRun: Boolean) = {
    val params = JsonEncoder { o =>
      method.fold(p => o.put("phone", p.str), e => o.put("email",  e.str))
      o.put("code",   code.str)
      o.put("dryrun", dryRun)
    }

    client(Request.Post(ActivatePath, JsonContentEncoder(params), baseUri = Some(backend.baseUrl), timeout = timeout)).map {
      case resp @ Response(SuccessHttpStatus(), _, _) =>
        debug(s"verifyRegistrationMethod: verified: $resp")
        Right(())
      case Response(_, ErrorResponse(code, msg, label), headers) =>
        warn(s"verifyRegistrationMethod: failed with error: ($code, $msg, $label), headers: $headers")
        Left(ErrorResponse(code, msg, label))
      case other =>
        error(s"verifyRegistrationMethod: Unexpected response: $other")
        Left(ErrorResponse(other.status.status, other.toString, "unknown"))
    }.future
  }
}

object RegistrationClientImpl {
  val RegisterPath = "/register"
  val ActivatePath = "/activate"
  val ActivateSendPath = "/activate/send"
  val LoginSendPath = "/login/send"

  val timeout = 15.seconds

  sealed trait ActivateResult
  object ActivateResult {
    case object Success extends ActivateResult
    case object PasswordExists extends ActivateResult
    case class Failure(err: ErrorResponse) extends ActivateResult
  }
}
