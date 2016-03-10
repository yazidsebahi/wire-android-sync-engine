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
import com.waz.model.ZUserId
import com.waz.service.{InstanceService, ZMessaging}
import com.waz.threading.{CancellableFuture, Threading}

import scala.concurrent.Future
import scala.util.control.NoStackTrace

trait ZmsExecutor {
  def execute[A](body: ZMessaging => Future[A])(implicit logTag: LogTag = getClass.getName): Future[A]
}

trait ZMessagingExecutor extends ZmsExecutor {
  private implicit val logTag: LogTag = logTagFor[ZMessagingExecutor]
  import Threading.Implicits.Background

  def context: Context
  def instance: InstanceService

  lazy val zms = instance.getCurrent

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
}

trait ZUserExecutor extends ZMessagingExecutor {
  import Threading.Implicits.Background

  val userId: ZUserId

  lazy val user = instance.global.users.getById(userId)

  override lazy val zms = user.future.flatMap {
    case Some(u) => instance.getInstance(u) map (Some(_))
    case _ => CancellableFuture.successful(None)
  }

  override def execute[A](body: (ZMessaging) => Future[A])(implicit logTag: LogTag): Future[A] = {
    super.execute { zms =>
      val future = Threading.Ui(zms.lifecycle.acquireSync(logTag)).future flatMap { _ => body(zms) }
      future.onComplete(_ => zms.lifecycle.releaseSync(logTag))(Threading.Ui)
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
