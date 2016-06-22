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
package com.waz.sync

import android.content.Context
import com.waz.ZLog._
import com.waz.model.AccountId
import com.waz.service.{AccountService, Accounts, ZMessaging}
import com.waz.threading.Threading

import scala.concurrent.Future
import scala.util.control.NoStackTrace

trait ZmsExecutor {
  def execute[A](body: ZMessaging => Future[A])(implicit logTag: LogTag = getClass.getName): Future[A]
}

trait ZMessagingExecutor extends ZmsExecutor {
  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[ZMessagingExecutor]

  def context: Context
  def accounts: Accounts

  lazy val zms = accounts.getCurrentZms

  override def execute[A](body: ZMessaging => Future[A])(implicit logTag: LogTag = getClass.getName): Future[A] =
    zms.flatMap {
      case None =>
        error(s"zmessaging not available")
        Future.failed(ZMessagingExecutor.NoZMessagingException)
      case Some(zmessaging) => body(zmessaging)
    }
}

object ZMessagingExecutor {
  case object NoZMessagingException extends Exception("ZMessaging instance not available") with NoStackTrace
  case object NoAccountException extends Exception("AccountService instance not available") with NoStackTrace
}

trait AccountExecutor {
  import Threading.Implicits.Background

  val userId: AccountId
  def context: Context
  def accounts: Accounts

  lazy val account = accounts.getInstance(userId)

  def execute[A](body: AccountService => Future[A])(implicit logTag: LogTag): Future[A] = {
    account flatMap {
      case None =>
        error(s"zmessaging not available")
        Future.failed(ZMessagingExecutor.NoAccountException)
      case Some(acc) =>
        val future = Threading.Ui(acc.lifecycle.acquireSync(logTag)).future flatMap { _ => body(acc) }
        future.onComplete(_ => acc.lifecycle.releaseSync(logTag))(Threading.Ui)
        future
    }
  }
}

trait ActivePush extends ZMessagingExecutor {
  import Threading.Implicits.Background

  abstract override def execute[A](body: (ZMessaging) => Future[A])(implicit logTag: LogTag): Future[A] = {
    super.execute { zms =>
      val future = Threading.Ui(zms.lifecycle.acquirePush(logTag)).future flatMap { _ => body(zms) }
      future.onComplete(_ => zms.lifecycle.releasePush(logTag))(Threading.Ui)
      future
    }
  }
}
