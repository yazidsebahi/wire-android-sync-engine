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
import com.waz.utils.JsonEncoder
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._
import com.waz.znet.Response.{SuccessHttpStatus, NotFoundStatus}

trait CredentialsUpdateClient {

  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit]
  def clearEmail(): ErrorOrResponse[Unit]

  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit]
  def clearPhone(): ErrorOrResponse[Unit]

  def updatePassword(newPassword: Password, currentPassword: Option[Password]): ErrorOrResponse[Unit]
  def updateHandle(handle: Handle): ErrorOrResponse[Unit]

  def hasPassword(): ErrorOrResponse[Boolean]
}

class CredentialsUpdateClientImpl(netClient: ZNetClient) extends CredentialsUpdateClient {
  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[CredentialsUpdateClientImpl]

  override def updateEmail(email: EmailAddress) =
    netClient.updateWithErrorHandling("updateEmail", Request.Put(CredentialsUpdateClientImpl.emailPath, JsonEncoder { _.put("email", email.str) }))

  override def clearEmail() =
    netClient.updateWithErrorHandling("clearEmail", Request.Delete[Unit](CredentialsUpdateClientImpl.emailPath))

  override def updatePhone(phone: PhoneNumber) =
    netClient.updateWithErrorHandling("updatePhone", Request.Put(CredentialsUpdateClientImpl.phonePath, JsonEncoder { _.put("phone", phone.str) }))

  override def clearPhone() =
    netClient.updateWithErrorHandling("clearPhone", Request.Delete[Unit](CredentialsUpdateClientImpl.phonePath))

  override def updatePassword(newPassword: Password, currentPassword: Option[Password]) =
    netClient.updateWithErrorHandling("updatePassword", Request.Put(CredentialsUpdateClientImpl.passwordPath, JsonEncoder { o =>
      o.put("new_password", newPassword.str)
      currentPassword.map(_.str).foreach(o.put("old_password", _))
    }))

  override def updateHandle(handle: Handle): ErrorOrResponse[Unit] =
    netClient.updateWithErrorHandling("updateHandle", Request.Put(CredentialsUpdateClientImpl.handlePath, JsonEncoder { _.put("handle", handle.toString) }))

  override def hasPassword(): ErrorOrResponse[Boolean] =
    netClient.withErrorHandling("hasPassword", Request.Head(CredentialsUpdateClientImpl.passwordPath)) {
      case Response(SuccessHttpStatus(), _, _) => true
      case Response(NotFoundStatus(), _, _)    => false
    }
}

object CredentialsUpdateClientImpl {
  val passwordPath = "/self/password"
  val emailPath = "/self/email"
  val phonePath = "/self/phone"
  val handlePath = "/self/handle"
}
