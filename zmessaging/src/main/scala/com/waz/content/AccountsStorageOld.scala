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
import com.waz.api.impl.{Credentials, EmailCredentials, PhoneCredentials}
import com.waz.model.AccountDataOld.AccountDataDao
import com.waz.model._
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.{CachedStorage, CachedStorageImpl, TrimmingLruCache}

import scala.concurrent.Future

trait AccountStorage extends CachedStorage[UserId, AccountData]

class AccountStorageImpl(context: Context, storage: Database) extends CachedStorageImpl[UserId, AccountDataOld](new TrimmingLruCache(context, Fixed(8)), storage)(AccountDataDao) with AccountStorage

trait AccountsStorageOld extends CachedStorage[AccountId, AccountDataOld] {
  def findByEmail(email: EmailAddress): Future[Option[AccountDataOld]]
  def findByPhone(phone: PhoneNumber): Future[Option[AccountDataOld]]
  def findByPendingTeamName(name: String): Future[Option[AccountDataOld]]
  def find(credentials: Credentials): Future[Option[AccountDataOld]]
  def findLoggedIn(): Future[Seq[AccountDataOld]]
}

class AccountsStorageOldImpl(context: Context, storage: Database) extends CachedStorageImpl[AccountId, AccountDataOld](new TrimmingLruCache(context, Fixed(8)), storage)(AccountDataDao) with AccountsStorageOld {
  import com.waz.threading.Threading.Implicits.Background

  def findByEmail(email: EmailAddress) = find(ac => ac.email.contains(email) || ac.pendingEmail.contains(email), AccountDataDao.findByEmail(email)(_), identity).map(_.headOption)

  def findByPhone(phone: PhoneNumber) = find(ac => ac.phone.contains(phone) || ac.pendingPhone.contains(phone), AccountDataDao.findByPhone(phone)(_), identity).map(_.headOption)

  def find(credentials: Credentials): Future[Option[AccountDataOld]] = credentials match {
    case EmailCredentials(email, _) => findByEmail(email)
    case PhoneCredentials(phone, _) => findByPhone(phone)
    case _ => Future successful None
  }

  def findLoggedIn() = find(_.cookie.isDefined, AccountDataDao.findLoggedIn()(_), identity)

  def findByPendingTeamName(name: String) = find(ac => ac.pendingTeamName.contains(name), AccountDataDao.findByPendingTeamName(name)(_), identity).map(_.headOption)
}
