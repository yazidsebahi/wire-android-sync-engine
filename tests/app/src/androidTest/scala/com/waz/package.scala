package com

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.FiniteDuration

package object waz {
  case class Timeout(duration: FiniteDuration)

  implicit class FutureSyntax[A](val f: Future[A]) extends AnyVal {
    def await(implicit timeout: Timeout): A = Await.result(f, timeout.duration)
  }

  implicit class PromiseSyntax[A](val p: Promise[A]) extends AnyVal {
    def await(implicit timeout: Timeout): A = Await.result(p.future, timeout.duration)
  }

  implicit class DurationSyntax(val d: FiniteDuration) extends AnyVal {
    def idle(): Unit = Thread.sleep(d.toNanos / 1000000L, (d.toNanos % 1000000L).toInt)
  }
}
