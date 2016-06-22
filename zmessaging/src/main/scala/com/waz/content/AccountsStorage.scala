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
package com.waz.content

import android.content.Context
import com.waz.ZLog._
import com.waz.api.impl.{Credentials, EmailCredentials, PhoneCredentials}
import com.waz.model.AccountData.AccountDataDao
import com.waz.model._
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.{CachedStorage, TrimmingLruCache}

import scala.concurrent.Future

class AccountsStorage(context: Context, storage: Database) extends CachedStorage[AccountId, AccountData](new TrimmingLruCache(context, Fixed(8)), storage)(AccountDataDao, "AccountStorage") {
  import AccountsStorage._
  import com.waz.threading.Threading.Implicits.Background

  def findByUserId(user: UserId) = find(_.userId == user, AccountDataDao.findByUser(user)(_), identity).map(_.headOption)

  def findByEmail(email: EmailAddress) = find(_.email.contains(email), AccountDataDao.findByEmail(email)(_), identity).map(_.headOption)

  def findByPhone(phone: PhoneNumber) = find(_.phone.contains(phone), AccountDataDao.findByPhone(phone)(_), identity).map(_.headOption)

  def find(credentials: Credentials): Future[Option[AccountData]] = credentials match {
    case EmailCredentials(email, _, _) => findByEmail(email)
    case PhoneCredentials(phone, _, _) => findByPhone(phone)
    case _ => Future successful None
  }

  def updateCookie(user: AccountData, cookie: Option[String]) = storage { implicit db =>
    verbose(s"updateCookie($user, cookie.isDefined = ${cookie.isDefined}")
    AccountDataDao.update(user)(_.copy(cookie = cookie, activated = user.activated || cookie.isDefined))
  }

  def updateEmail(id: AccountId, email: EmailAddress, verified: Boolean) = storage { implicit db =>
    debug(s"updateEmail($id, $email, $verified)")
    AccountDataDao.updateById(id)(_.copy(email = Some(email), activated = verified))
  } ("ZUsers.updateEmail")

  def updatePhone(id: AccountId, phone: PhoneNumber) = storage { implicit db =>
    debug(s"updatePhone($id, $phone)")
    AccountDataDao.updateById(id)(_.copy(phone = Some(phone)))
  } ("ZUsers.updatePhone")

  def resetCredentials(user: AccountData) = storage.withTransaction { implicit db =>
    AccountDataDao.update(user)(_.copy(hash = "", cookie = None))
  }
}

object AccountsStorage {
  private implicit val Tag: LogTag = "AccountStorage"
}
