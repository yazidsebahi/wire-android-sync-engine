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
import com.waz.ZLog.ImplicitTag._
import com.waz.model.AccountData.AccountDataDao
import com.waz.model.AccountDataOld.AccountDataOldDao
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.{EventStream, RefreshingSignal, Signal}
import com.waz.utils.{CachedStorage, CachedStorageImpl, TrimmingLruCache}

import scala.concurrent.Future

trait AccountStorage extends CachedStorage[UserId, AccountData] {
  def getLoggedInAccounts: Future[Set[UserId]]
  def loggedInAccounts: Signal[Set[UserId]]
}
class AccountStorageImpl(context: Context, storage: Database) extends CachedStorageImpl[UserId, AccountData](new TrimmingLruCache(context, Fixed(8)), storage)(AccountDataDao) with AccountStorage {
  import Threading.Implicits.Background

  override def loggedInAccounts = {
    val changes = EventStream.union(
      onChanged.map(_.map(_.id)),
      onDeleted
    ).map(_.toSet)
    //TODO - make an aggregating signal?
    RefreshingSignal[Set[UserId], Set[UserId]](getLoggedInAccounts, changes)
  }

  //Note - only ids are safe to return here, as the password is not kept in memory and `list()` doesn't check the cache.
  override def getLoggedInAccounts = list().map(_.map(_.id).toSet)
}

trait AccountsStorageOld extends CachedStorage[AccountId, AccountDataOld]
class AccountsStorageOldImpl(context: Context, storage: Database) extends CachedStorageImpl[AccountId, AccountDataOld](new TrimmingLruCache(context, Fixed(8)), storage)(AccountDataOldDao) with AccountsStorageOld
