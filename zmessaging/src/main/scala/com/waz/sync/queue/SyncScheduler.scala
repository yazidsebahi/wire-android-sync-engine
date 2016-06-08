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
package com.waz.sync.queue

import java.io.PrintWriter

import android.app.{AlarmManager, PendingIntent}
import android.content.Context
import android.support.v4.content.WakefulBroadcastReceiver
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.model.sync.SyncJob
import com.waz.model.{ConvId, SyncId, ZUserId}
import com.waz.service.{NetworkModeService, ZUserService}
import com.waz.sync.{SyncHandler, SyncRequestService, SyncResult}
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.returning
import com.waz.zms.SyncService

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.Try

class SyncScheduler(context: Context, userId: ZUserId, val content: SyncContentUpdater, val network: NetworkModeService, zuser: ZUserService, service: SyncRequestService, handler: => SyncHandler) {

  import EventContext.Implicits.global
  import content._

  private implicit val tag = logTagFor[SyncScheduler]
  private implicit val dispatcher = new SerialDispatchQueue(name = "SyncSchedulerQueue")

  private[sync] lazy val alarmSyncIntent = PendingIntent.getService(context, SyncScheduler.AlarmRequestCode, SyncService.intent(context, userId), PendingIntent.FLAG_UPDATE_CURRENT)
  private[sync] lazy val alarmManager = context.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
  private[sync] lazy val syncIntent = SyncService.intent(context, userId)

  private[sync] val queue = new SyncSerializer
  private[sync] val executor = new SyncExecutor(this, content, network, handler)
  private[sync] val executions = new mutable.HashMap[SyncId, Future[SyncResult]]()
  private[sync] val executionsCount = Signal(0)

  private val waitEntries = new mutable.HashMap[SyncId, WaitEntry]
  private val waiting = Signal(Map.empty[SyncId, Long])
  private val runningCount = Signal(executionsCount, waiting.map(_.size)) map { case (r, w) => r - w }

  private val alarmUpdate = Signal(0L)
  private val nextAlarm = waiting.throttle(1.second) flatMap { jobs =>
    alarmUpdate map { _ =>
      val time = System.currentTimeMillis()
      val inFuture = jobs.values.filter(_ > time)
      if (inFuture.isEmpty) None
      else Some(inFuture.min)
    }
  }

  // start sync service any time running executors count changes from 0 to positive number
  runningCount.map(_ > 0) { if (_) startSyncService() }

  syncStorage { storage =>
    storage.getJobs.toSeq.sortBy(_.timestamp) foreach execute
    storage.onAdded.on(dispatcher) { execute }
    storage.onUpdated
      .filter { case (prev, job) => prev.priority != job.priority || prev.startTime != job.startTime }
      .on(dispatcher) {
        case (prev, job) =>
          waiting.mutate { jobs => if (jobs.contains(job.id)) jobs.updated(job.id, getStartTime(job)) else jobs }
          waitEntries.get(job.id) foreach (_.onUpdated(job))
          alarmUpdate ! System.currentTimeMillis() // force update of alarm signal
      }
  }

  zuser.onVerifiedLogin.on(dispatcher) { user =>
    if (user.contains(userId)) waitEntries.foreach(_._2.onVerifiedLogin())
  }

  network.networkMode.on(dispatcher) {
    case NetworkMode.OFFLINE => // do nothing
    case _ => waitEntries.foreach(_._2.onOnline())
  }

  nextAlarm { updateRetryAlarm }

  def report(pw: PrintWriter) = Future {
    pw.println(s"SyncScheduler: executors: ${executions.size}, count: ${executionsCount.currentValue}, running: ${runningCount.currentValue}, waiting: ${waiting.currentValue}")
  }

  private def execute(job: SyncJob): Unit = {
    verbose(s"execute($job)")
    val future = executor(job)
    executions += job.id -> future
    executionsCount.mutate(_ + 1)
    future onComplete { res =>
      executions -= job.id
      executionsCount.mutate(_ - 1)
      verbose(s"job completed: $job, res: $res")
    }
  }

  def await(id: SyncId) = Future { executions.getOrElse(id, Future.successful(SyncResult.Success)) } flatMap identity

  def awaitRunning = CancellableFuture.delay(1.second).future flatMap { _ => runningCount.filter(_ == 0).head }

  private def countWaiting[A](id: SyncId, startTime: Long)(future: Future[A]) = {
    waiting.mutate(_ + (id -> startTime))
    future.onComplete(_ => waiting.mutate(_ - id))
    future
  }

  private[sync] def withConv[A](job: SyncJob, conv: ConvId)(f: ConvLock => Future[A]): Future[A] = {
    verbose(s"withConv($job, $conv)")
    countWaiting(job.id, getStartTime(job)) { queue.acquire(conv) } flatMap { lock =>
      Try(f(lock)).recover { case t => Future.failed[A](t) }.get.andThen { case _ => lock.release() }
    }
  }

  private[sync] def awaitPreconditions[A](job: SyncJob)(f: => Future[A]): Future[A] = {
    verbose(s"awaitPreconditions($job)")

    val entry = new WaitEntry(job)
    waitEntries.put(job.id, entry)
    entry.future onComplete { _ => waitEntries -= job.id }

    countWaiting(job.id, getStartTime(job))(entry.future) flatMap { _ =>
      returning(f)(_.onComplete(_ => queue.release()))
    }
  }

  private[sync] def startSyncService(): Unit = {
    debug("starting service")
    val res = WakefulBroadcastReceiver.startWakefulService(context, syncIntent)
    if (res == null) error("Couldn't start sync service. Make sure zeta sync service is included in the app manifest.")
  }

  private[sync] def updateRetryAlarm(time: Option[Long]) =
    time.fold(alarmManager.cancel(alarmSyncIntent)) { time => alarmManager.set(AlarmManager.RTC, time, alarmSyncIntent) }

  private[sync] def getStartTime(job: SyncJob): Long =
    if (job.offline && network.isOnlineMode) 0  // start right away if request last failed due to possible network errors
    else if (job.timeout > 0) math.min(job.startTime, job.timeout)
    else job.startTime


  class WaitEntry(private var job: SyncJob) { self =>
    private val promise = Promise[Unit]()

    private var delayFuture: CancellableFuture[Unit] = setup(job)

    private def setup(job: SyncJob) = {
      val delay = CancellableFuture.delay(math.max(0, getStartTime(job) - System.currentTimeMillis()).millis)
      for {
        _ <- delay.recover { case _: CancelException => () } .future
        _ <- Future.traverse(job.dependsOn)(await)
        _ <- queue.acquire(job.priority)
      } yield {
        if (job == self.job) promise.trySuccess(())
        else queue.release() // this wait entry was already updated, releasing acquired lock
      }
      delay
    }

    def isCompleted = promise.isCompleted
    def onVerifiedLogin() = delayFuture.cancel()
    def onOnline() = if (job.offline) delayFuture.cancel()
    def onUpdated(updated: SyncJob): Unit = {
      job = updated
      verbose(s"job updated: $job, should update delay and/or priority")
      delayFuture = setup(updated)
    }

    def future = promise.future
  }
}

object SyncScheduler {
  val AlarmRequestCode = 19523
}
