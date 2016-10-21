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
package com.waz.utils

import android.content.Context
import android.os.PowerManager
import com.waz.ZLog
import com.waz.ZLog.{LogTag, verbose}
import com.waz.threading.CancellableFuture
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils.events.EventContext.Implicits.global
import com.waz.utils.events.Signal

import scala.concurrent.Future
import scala.concurrent.duration._

class WakeLock(context: Context, level: Int = PowerManager.PARTIAL_WAKE_LOCK)(implicit tag: LogTag) {

  protected val appContext = context.getApplicationContext
  protected lazy val powerManager = appContext.getSystemService(Context.POWER_SERVICE).asInstanceOf[PowerManager]
  protected lazy val wakeLock = powerManager.newWakeLock(level, tag)

  protected val count = Signal[Int]()

  count {
    case 0 if wakeLock.isHeld =>
      verbose("releasing wakelock")
      wakeLock.release()
    case 1 if !wakeLock.isHeld =>
      verbose("acquiring wakelock")
      wakeLock.acquire()
    case _ =>
  }

  def apply[A](body: => A): A = {
    count.mutateOrDefault(_ + 1, 1)
    try {
      body
    } finally {
      count.mutate(_ - 1)
    }
  }

  def async[A](body: Future[A]): Future[A] = {
    count.mutateOrDefault(_ + 1, 1)
    returning(body) { _.onComplete(_ => count.mutate(_ - 1)) }
  }
}

//To keep wakelock for a given period of time after executing it's code
class TimedWakeLock(context: Context, duration: FiniteDuration)(implicit tag: LogTag) extends WakeLock(context) {

  override def apply[A](body: => A): A = {
    count.mutateOrDefault(_ + 1, 1)
    try {
      body
    } finally {
      releaseAfterDuration()
    }
  }

  override def async[A](body: Future[A]): Future[A] = {
    count.mutateOrDefault(_ + 1, 1)
    returning(body) { _.onComplete(_ => releaseAfterDuration()) }
  }

  private def releaseAfterDuration(): Unit = CancellableFuture.delay(duration).onComplete(_ => count.mutate(_ - 1))
}
