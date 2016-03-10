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
package com.waz.api.impl

import com.waz.api.ProgressIndicator.State
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.{RobolectricUtils, api}
import org.scalatest.{FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Promise
import scala.util.{Failure, Success}

class FutureLoadHandleSpec extends FeatureSpec with Matchers with RobolectricTests with RobolectricUtils {

  implicit lazy val dispatcher = Threading.Background

  var progress: api.ProgressIndicator = null

  feature("Future load handler progress indicators") {
    val p = Promise[Unit]()

    scenario("Initially, the progress indicator should be running.") {
      progress = handle(p).getProgressIndicator
      progress.isIndefinite should be(true)
      progress.getState shouldEqual State.RUNNING
    }

    scenario("When completing the future successfully, the progress indicator should be completed.") {
      p.complete(Success({}))
      withDelay {
        progress.getState shouldEqual State.COMPLETED
      }
    }

    scenario("WHen failing a future, the progress indicator should be failed.") {
      val p = Promise[Unit]()
      progress = handle(p).getProgressIndicator
      p.complete(Failure(new RuntimeException))
      withDelay {
        progress.getState shouldEqual State.FAILED
      }
    }

    scenario("WHen cancelling a future, the progress indicator should be cancelled (for succeeding futures).") {
      val p = Promise[Unit]()
      val lh = handle(p)
      progress = lh.getProgressIndicator
      lh.cancel()
      p.complete(Success({}))
      withDelay {
        progress.getState shouldEqual State.CANCELLED
      }
    }

    scenario("WHen cancelling a future, the progress indicator should be cancelled (for failing futures).") {
      val p = Promise[Unit]()
      val lh = handle(p)
      progress = lh.getProgressIndicator
      lh.cancel()
      p.complete(Failure(new RuntimeException))
      withDelay {
        progress.getState shouldEqual State.CANCELLED
      }
    }
  }
  private def handle[T](p: Promise[T]): FutureLoadHandle[T] = FutureLoadHandle(CancellableFuture.lift(p.future, {}))(_ => ())
}
