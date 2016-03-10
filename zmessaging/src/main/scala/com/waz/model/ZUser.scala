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
package com.waz.model

import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import android.util.Base64
import com.waz.ZLog._
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils.scrypt.SCrypt
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.AuthenticationManager.Cookie

/**
 * Represents a local zclient user account.
 *
 * @param emailVerified - true if user was successfully signed in on backend by email at least once
 * @param password - will not be stored in db
 */
case class ZUser(id: ZUserId, email: Option[EmailAddress], hash: String, phone: Option[PhoneNumber], emailVerified: Boolean = false,
    phoneVerified: Boolean = false, cookie: Cookie = None, password: Option[String] = None) {

  override def toString: String = s"ZUser($id, $email, $phone, $emailVerified, ${cookie.map(_ take 10)})"
}

case class PhoneNumber(str: String) extends AnyVal {
  override def toString: String = str
}
object PhoneNumber extends (String => PhoneNumber) {
  implicit def IsOrdered: Ordering[PhoneNumber] = currentLocaleOrdering.on(_.str)
  implicit val Encoder: JsonEncoder[PhoneNumber] = JsonEncoder.build(p => js => js.put("phone", p.str))
  implicit val Decoder: JsonDecoder[PhoneNumber] = JsonDecoder.lift(implicit js => PhoneNumber(JsonDecoder.decodeString('phone)))
}

case class ConfirmationCode(str: String) extends AnyVal {
  override def toString: String = str
}

object ZUser {
  private implicit val logTag: LogTag = logTagFor(ZUser)

  def apply(id: ZUserId, email: String, hash: String): ZUser = ZUser(id, email = Some(EmailAddress(email)), hash, phone = None)  // used only for testing

  def apply(email: EmailAddress, password: String): ZUser = {
    val id = ZUserId()
    ZUser(id, Some(email), computeHash(id, password), password = Some(password), phone = None)
  }

  def apply(phone: PhoneNumber, cookie: Cookie): ZUser = ZUser(ZUserId(), email = None, hash = "", phone = Some(phone), cookie = cookie)

  def computeHash(id: ZUserId, password: String) =
    logTime(s"compute scrypt password hash") {
      if (password.isEmpty) "" else {
        val salt = id.str.replace("-", "").getBytes("utf8").take(16)
        val hash = SCrypt.scrypt(password.getBytes("utf8"), salt, 1024, 8, 1, 32)
        Base64.encodeToString(hash, Base64.NO_WRAP)
      }
    }

  implicit object ZUserDao extends Dao[ZUser, ZUserId] {
    val Id = id[ZUserId]('_id, "PRIMARY KEY").apply(_.id)
    val Email = opt(emailAddress('email))(_.email)
    val Hash = text('hash)(_.hash)
    val EmailVerified = bool('verified)(_.emailVerified)
    val PhoneVerified = bool('phone_verified)(_.phoneVerified)
    val Cookie = opt(text('cookie))(_.cookie)
    val Phone = opt(phoneNumber('phone))(_.phone)

    override val idCol = Id
    override val table = Table("ZUsers", Id, Email, Hash, EmailVerified, PhoneVerified, Cookie, Phone)

    override def apply(implicit cursor: Cursor): ZUser = ZUser(Id, Email, Hash, Phone, EmailVerified, PhoneVerified, Cookie)

    def findByEmail(email: EmailAddress)(implicit db: SQLiteDatabase): Option[ZUser] =
      single(db.query(table.name, null, s"${Email.name} = ?", Array(email.str), null, null, null))

    def findByPhone(phone: PhoneNumber)(implicit db: SQLiteDatabase): Option[ZUser] =
      single(db.query(table.name, null, s"${Phone.name} = ?", Array(phone.str), null, null, null))

    def deleteForEmail(email: EmailAddress)(implicit db: SQLiteDatabase) = delete(Email, Some(email))
  }
}
