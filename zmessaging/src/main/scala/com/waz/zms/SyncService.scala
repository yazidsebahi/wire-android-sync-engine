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
import com.waz.model.ZUserId
import com.waz.service.{InstanceService, ZMessaging}
import com.waz.sync.ZUserExecutor
import com.waz.threading.SerialDispatchQueue

import scala.collection.mutable
import scala.concurrent.Future

class SyncService extends WakefulFutureService with ZMessagingService {

  implicit val dispatcher = new SerialDispatchQueue(name = "SyncService")
  private implicit val logTag: LogTag = logTagFor[SyncService]

  val executors = new mutable.HashMap[ZUserId, SyncService.Executor]

  def instance = ZMessaging.currentInstance
  
  override protected def onIntent(intent: Intent, id: Int): Future[Any] = {
    debug(s"onIntent $intent")
    if (intent != null && intent.hasExtra(SyncService.ZUserIdExtra)) {
      val userId = ZUserId(intent.getStringExtra(SyncService.ZUserIdExtra))
      executors.getOrElseUpdate(userId, new SyncService.Executor(getApplicationContext, userId, instance)).sync(id)
    } else {
      error("intent has no ZUserId extra")
      Future.successful(())
    }
  }
}

object SyncService {

  val ZUserIdExtra = "user_id"

  def intent(context: Context, user: ZUserId) = {
    val intent = new Intent(context, classOf[SyncService])
    intent.putExtra(ZUserIdExtra, user.str)
    intent
  }

  class Executor(val context: Context, val userId: ZUserId, val instance: InstanceService) extends ZUserExecutor {
    def sync(id: Int) = execute(_.syncRequests.scheduler.awaitRunning)(s"SyncService.Executor $id")
  }
}
