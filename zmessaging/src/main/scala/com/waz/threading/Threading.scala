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
package com.waz.threading

import java.util.Timer

import android.os.Looper
import java.util.concurrent._

import com.waz.ZLog._
import com.waz.api.ZmsVersion

import scala.concurrent.ExecutionContext

object Threading {

  object Implicits {
    implicit val Background: DispatchQueue = Threading.ThreadPool
    implicit val Ui: DispatchQueue = Threading.Ui
    implicit val Image: DispatchQueue = Threading.ImageDispatcher
  }

  var AssertsEnabled = ZmsVersion.DEBUG

  val Cpus = math.max(2, Runtime.getRuntime.availableProcessors())

  def executionContext(service: ExecutorService) = new ExecutionContext {
    override def reportFailure(cause: Throwable): Unit = error(cause.getMessage, cause)("MainThreadPool")
    override def execute(runnable: Runnable): Unit = service.execute(runnable)
  }

  /**
   * Thread pool for non-blocking background tasks.
   */
  val ThreadPool: DispatchQueue = new LimitedDispatchQueue(Cpus, executionContext(Executors.newCachedThreadPool()), "CpuThreadPool")

  /**
   * Thread pool for blocking IO tasks.
   */
  val IOThreadPool: DispatchQueue = new LimitedDispatchQueue(Cpus, executionContext(Executors.newCachedThreadPool()), "IoThreadPool")

  val Background = ThreadPool

  val IO = IOThreadPool

  lazy val Ui = new UiDispatchQueue

  val Timer = new Timer(true)

  Timer.purge()

  /**
   * Image decoding/encoding dispatch queue. This operations are quite cpu intensive, we don't want them to use all cores (leaving one spare core for other tasks).
   */
  val ImageDispatcher = new LimitedDispatchQueue(Cpus - 1, ThreadPool, "ImageDispatcher")

  def assertUiThread(): Unit = {
    if (AssertsEnabled) assert(Thread.currentThread() eq Looper.getMainLooper.getThread, s"Should be run on Ui thread, but is using: ${Thread.currentThread().getName}")
  }
  def assertNotUiThread(): Unit = {
    if (AssertsEnabled) assert(Thread.currentThread() ne Looper.getMainLooper.getThread, s"Should be run on background thread, but is using: ${Thread.currentThread().getName}")
  }
}
