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
import com.waz.model.AccountData.Label
import com.waz.model._
import com.waz.service.BackendConfig
import com.waz.sync.client.UsersClient.UserResponseExtractor
import com.waz.threading.Threading
import com.waz.utils.JsonEncoder
import com.waz.utils.Locales._
import com.waz.znet.AuthenticationManager.Cookie
import com.waz.znet.ContentEncoder.JsonContentEncoder
import com.waz.znet.Response.{Status, SuccessHttpStatus}
import com.waz.znet.ZNetClient.ErrorOr
import com.waz.znet._

import scala.concurrent.Future
import scala.concurrent.duration._

trait RegistrationClient {
  def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean = false): Future[ActivateResult]
  def requestEmailCode(email: EmailAddress): Future[ActivateResult] //for now only used for registration

  def requestVerificationEmail(email: EmailAddress): ErrorOr[Unit]

  def verifyRegistrationMethod(method: Either[PhoneNumber, EmailAddress], code: ConfirmationCode, dryRun: Boolean): ErrorOr[Option[(Cookie, Label)]]

  def register(credentials: Credentials, name: String, teamName: Option[String]): ErrorOr[(UserInfo, Option[(Cookie, Label)])]
}

class RegistrationClientImpl(client: AsyncClient, backend: BackendConfig) extends RegistrationClient {
  import Threading.Implicits.Background
  import com.waz.client.RegistrationClientImpl._

  def register(credentials: Credentials, name: String, teamName: Option[String]) = {
    val label = Label()
    val params = JsonEncoder { o =>
      o.put("name", name)
      o.put("locale", bcp47.languageTagOf(currentLocale))
      credentials.addToRegistrationJson(o)
      teamName.foreach { t =>
        o.put("team", JsonEncoder { o2 =>
          o2.put("icon", "abc") //TODO proper icon
          o2.put("name", t)
        })
      }
      o.put("label", label.str)
    }

    val request = Request.Post(RegisterPath, JsonContentEncoder(params), baseUri = Some(backend.baseUrl), timeout = timeout)
    client(request).map {
      case resp @ Response(SuccessHttpStatus(), UserResponseExtractor(user), headers) =>
        debug(s"registration succeeded: $resp")
        Right((user, LoginClient.getCookieFromHeaders(headers).map(c => (c, label)))) //cookie (and label) will be optional depending on login method
      case Response(_, ErrorResponse(code, msg, label), headers) =>
        info(s"register failed with error: ($code, $msg, $label), headers: $headers")
        Left(ErrorResponse(code, msg, label))
      case resp =>
        error(s"Unexpected response from register: $resp")
        Left(ErrorResponse(resp.status.status, resp.toString, "unknown"))
    }.future
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
      case Response(_, ErrorResponse(Status.Forbidden, _, "password-exists"), _) =>
        ActivateResult.PasswordExists
      case Response(_, ErrorResponse(code, msg, label), headers) =>
        warn(s"requestCode: login=$login failed with error: ($code, $msg, $label), headers: $headers")
        ActivateResult.Failure(ErrorResponse(code, msg, label))
      case other =>
        error(s"Unexpected response from requestCode: login=$login: $other")
        ActivateResult.Failure(ErrorResponse(other.status.status, other.toString, "unknown"))
    }.future
  }

  override def requestVerificationEmail(email: EmailAddress) = {
    val params = JsonEncoder { o =>
      o.put("email", email.str)
    }
    val request = Request.Post(ActivateSendPath, JsonContentEncoder(params), baseUri = Some(backend.baseUrl))
    client(request).map {
      case Response(SuccessHttpStatus(), _, _) => Right(())
      case Response(_, ErrorResponse(code, msg, label), _) =>
        info(s"requestVerificationEmail failed with error: ($code, $msg, $label)")
        Left(ErrorResponse(code, msg, label))
      case resp =>
        error(s"Unexpected response from resendVerificationEmail: $resp")
        Left(ErrorResponse(400, resp.toString, "unknown"))
    }.future
  }

  override def verifyRegistrationMethod(method: Either[PhoneNumber, EmailAddress], code: ConfirmationCode, dryRun: Boolean) = {
    val label = Label()
    val params = JsonEncoder { o =>
      method.fold(p => o.put("phone", p.str), e => o.put("email",  e.str))
      o.put("code",   code.str)
      o.put("dryrun", dryRun)
      if (!dryRun) o.put("label", label.str)
    }

    client(Request.Post(ActivatePath, JsonContentEncoder(params), baseUri = Some(backend.baseUrl), timeout = timeout)).map {
      case resp @ Response(SuccessHttpStatus(), _, headers) =>
        debug(s"verifyRegistrationMethod: verified: $resp")
        Right(LoginClient.getCookieFromHeaders(headers).map(c => (c, label))) //cookie (and label) may be returned on dryRun = false
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
