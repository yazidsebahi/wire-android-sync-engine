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
package com.waz.api

import com.waz.model.{ConfirmationCode, EmailAddress, Handle, PhoneNumber}
import org.json.JSONObject

sealed trait Credentials {
  def addToRegistrationJson(o: JSONObject): Unit
  def addToLoginJson(o: JSONObject): Unit
}

case class EmailCredentials(email: EmailAddress, password: String) extends Credentials {
  override def addToRegistrationJson(o: JSONObject): Unit = {
    o.put("email", email.str)
    o.put("password", password)
  }

  override def addToLoginJson(o: JSONObject): Unit = addToRegistrationJson(o)

  override def toString: String = s"EmailCredentials($email, $password)" //TODO obfuscate
}

case class PhoneCredentials(phone: PhoneNumber, code: ConfirmationCode) extends Credentials { //TODO obfuscate code?

  override def addToRegistrationJson(o: JSONObject): Unit = addToJson(o, "phone_code")
  override def addToLoginJson(o: JSONObject): Unit = addToJson(o, "code")

  private def addToJson(o: JSONObject, codeName: String): Unit = {
    o.put("phone", phone.str)
    o.put(codeName, code.str)
  }
}

case class HandleCredentials(handle: Handle, password: String) extends Credentials {
  override def addToRegistrationJson(o: JSONObject): Unit = {
    o.put("email", handle.string)
    o.put("password", password)
  }

  override def addToLoginJson(o: JSONObject): Unit = addToRegistrationJson(o)

  override def toString: String = s"UsernameBasedCredentials($handle, $password)" //TODO obfuscate
}

