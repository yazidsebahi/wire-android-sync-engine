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
package com.waz.zms

import android.content.{Context, Intent}
import com.waz.ZLog._
import com.waz.model.AccountId
import com.waz.service.{Accounts, ZMessaging}
import com.waz.sync.AccountExecutor
import com.waz.threading.SerialDispatchQueue

import scala.collection.mutable
import scala.concurrent.Future

class SyncService extends WakefulFutureService with ZMessagingService {

  implicit val dispatcher = new SerialDispatchQueue(name = "SyncService")
  private implicit val logTag: LogTag = logTagFor[SyncService]

  val executors = new mutable.HashMap[AccountId, SyncService.Executor]

  def accounts = ZMessaging.currentAccounts

  override protected def onIntent(intent: Intent, id: Int): Future[Any] = {
    debug(s"onIntent $intent")
    if (intent != null && intent.hasExtra(SyncService.ZUserIdExtra)) {
      val userId = AccountId(intent.getStringExtra(SyncService.ZUserIdExtra))
      executors.getOrElseUpdate(userId, new SyncService.Executor(getApplicationContext, userId, accounts)).sync(id)
    } else {
      error("intent has no ZUserId extra")
      Future.successful(())
    }
  }
}

object SyncService {

  val ZUserIdExtra = "user_id"

  def intent(context: Context, user: AccountId) = {
    val intent = new Intent(context, classOf[SyncService])
    intent.putExtra(ZUserIdExtra, user.str)
    intent
  }

  class Executor(val context: Context, val userId: AccountId, val accounts: Accounts) extends AccountExecutor {
    import com.waz.threading.Threading.Implicits.Background

    def sync(id: Int) = execute { acc =>
      acc.userModule.head flatMap { _.syncRequests.scheduler.awaitRunning }
    } (s"SyncService.Executor $id")
  }
}
