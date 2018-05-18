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
package com.waz.sync.client

import com.waz.ZLog._
import com.waz.model.AccountData.Password
import com.waz.model.{EmailAddress, Handle, PhoneNumber}
import com.waz.threading.Threading
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.Response.{NotFoundStatus, SuccessHttpStatus}
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._

import scala.concurrent.Future

trait CredentialsUpdateClient {

  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit]
  def clearEmail(): ErrorOrResponse[Unit]

  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit]
  def clearPhone(): ErrorOrResponse[Unit]

  def updatePassword(newPassword: Password, currentPassword: Option[Password]): ErrorOrResponse[Unit]
  def updateHandle(handle: Handle): ErrorOrResponse[Unit]

  def hasPassword(): ErrorOrResponse[Boolean]

  def hasMarketingConsent: Future[Boolean]

  def setMarketingConsent(receiving: Boolean, majorVersion: String, minorVersion: String): ErrorOrResponse[Unit]
}

class CredentialsUpdateClientImpl(netClient: ZNetClient) extends CredentialsUpdateClient {
  import CredentialsUpdateClient._
  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[CredentialsUpdateClientImpl]

  override def updateEmail(email: EmailAddress) =
    netClient.updateWithErrorHandling("updateEmail", Request.Put(EmailPath, JsonEncoder { _.put("email", email.str) }))

  override def clearEmail() =
    netClient.updateWithErrorHandling("clearEmail", Request.Delete[Unit](EmailPath))

  override def updatePhone(phone: PhoneNumber) =
    netClient.updateWithErrorHandling("updatePhone", Request.Put(PhonePath, JsonEncoder { _.put("phone", phone.str) }))

  override def clearPhone() =
    netClient.updateWithErrorHandling("clearPhone", Request.Delete[Unit](PhonePath))

  override def updatePassword(newPassword: Password, currentPassword: Option[Password]) =
    netClient.updateWithErrorHandling("updatePassword", Request.Put(PasswordPath, JsonEncoder { o =>
      o.put("new_password", newPassword.str)
      currentPassword.map(_.str).foreach(o.put("old_password", _))
    }))

  override def updateHandle(handle: Handle) =
    netClient.updateWithErrorHandling("updateHandle", Request.Put(HandlePath, JsonEncoder { _.put("handle", handle.toString) }))

  override def hasPassword() =
    netClient.withErrorHandling("hasPassword", Request.Head(PasswordPath)) {
      case Response(SuccessHttpStatus(), _, _) => true
      case Response(NotFoundStatus(), _, _)    => false
    }

  override def hasMarketingConsent =
    netClient.withErrorHandling("isReceivingNewsAndOffers", Request.Get(ConsentPath)) {
      case Response(SuccessHttpStatus(), JsonObjectResponse(json), _) =>
        val results = JsonDecoder.array(json.getJSONArray("results"), {
          case (arr, i) => (arr.getJSONObject(i).getInt("type"), arr.getJSONObject(i).getInt("value"))
        }).toMap
        results.get(ConsentTypeMarketing).contains(1)
    }.future.map {
      case Right(true) => true
      case _           => false
    }

  override def setMarketingConsent(receiving: Boolean, majorVersion: String, minorVersion: String) =
    netClient.updateWithErrorHandling(s"setMarketingConsent: $receiving", Request.Put(ConsentPath, JsonEncoder { o =>
      o.put("type", ConsentTypeMarketing)
      o.put("value", if (receiving) 1 else 0)
      o.put("source", s"Android $majorVersion.$minorVersion")
    }))
}

object CredentialsUpdateClient {
  val PasswordPath = "/self/password"
  val EmailPath = "/self/email"
  val PhonePath = "/self/phone"
  val HandlePath = "/self/handle"

  val ConsentPath = "/self/consent"

  //https://github.com/wireapp/architecture/blob/master/topics/privacy/use_cases/clients/01-change-marketing-consent.md
  //https://github.com/wireapp/architecture/blob/master/topics/privacy/use_cases/clients/02-ask-marketing-consent-at-registration.md
  val ConsentTypeMarketing = 2

}
