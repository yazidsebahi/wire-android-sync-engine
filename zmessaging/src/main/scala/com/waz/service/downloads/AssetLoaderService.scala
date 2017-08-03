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
package com.waz.service.downloads

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.ProgressData
import com.waz.cache.CacheEntry
import com.waz.model.{AssetData, AssetId}
import com.waz.service.ZMessaging.clock
import com.waz.service.downloads.AssetLoader.DownloadException
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils._
import com.waz.utils.events.{AggregatingSignal, EventContext, EventStream, Signal}
import org.threeten.bp.Instant

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * Keeps track of all load requests in priority queue, executes more important loads first.
  * Priority is based on request start times.
  *
  * Note, the AssetLoaderService is globally defined, and different instances of AssetLoader can be used to perform load
  * operations. This is to have a single global asset loading queue for all zms instances (and also for no instances), but
  * it still allows scoped instances of the actual loaders to be used (with their relevant credentials or lack thereof)
  */
class AssetLoaderService {
  import AssetLoaderService._
  private implicit val dispatcher = new SerialDispatchQueue(name = "AssetLoaderService")
  private implicit val ev = EventContext.Global

  private val requests  = new mutable.HashMap[AssetId, LoadEntry]()
  private val active    = new mutable.HashSet[AssetId]
  private val queue     = new mutable.PriorityQueue[QueueEntry]()(QueueOrdering)

  private val onAdded   = EventStream[LoadEntry]()

  private def getLoadEntry(id: AssetId): Signal[Option[LoadEntry]] =
    new AggregatingSignal[LoadEntry, Option[LoadEntry]](onAdded.filter(_.asset == id), Future(requests.get(id)), { (_, added) => Some(added) })

  def getLoadProgress(id: AssetId): Signal[ProgressData] = getLoadEntry(id) flatMap {
    case Some(entry) => entry.state
    case None => Signal(ProgressData.Unknown)
  }

  def cancel(id: AssetId): Future[Unit] = Future(removeTaskIfIdle(id))

  //When cancelling tasks, it only really makes sense to cancel idle loads, or else we'll be wasting work.
  //This also helps prevent race conditions caused by Cancellable futures exposing the work done by the LoadEntry
  private def removeTaskIfIdle(id: AssetId) = {
    if (!active.contains(id)) {
      requests.remove(id) foreach { _.cancel() }
      checkQueue()
      true
    } else false
  }

  def load(asset: AssetData, force: Boolean = false)(implicit loader: AssetLoader): CancellableFuture[Option[CacheEntry]] =
    loadRevealAttempts(asset, force).map { case (res, _) => res}

  //reveals attempted load count - useful for testing retry logic
  def loadRevealAttempts(asset: AssetData, force: Boolean = false)(implicit loader: AssetLoader): CancellableFuture[(Option[CacheEntry], Int)] = {

    val p = loadEntry(asset, force).promise
    returning(new CancellableFuture(p)) { l =>
      l.recoverWith {
        case ex: CancelException =>
          if (removeTaskIfIdle(asset.id)) CancellableFuture.failed(ex) else new CancellableFuture(p)
      }
    }.map {
      case (entry, attempts) => (Some(entry), attempts)
    }
  }

  private def loadEntry(asset: AssetData, force: Boolean = false)(implicit loader: AssetLoader) = {
    verbose(s"loadEntry(${asset.id}, $force)")

    verbose(s"load requests: ${requests.keys}")
    verbose(s"active:        $active")
    verbose(s"in queue:      $queue")

    def createOrUpdate(cur: Option[LoadEntry]) = {
      cur.map(e => e.copy(asset = asset, force = e.force || force))
        .getOrElse(returning(LoadEntry(asset, loader, force))(onAdded ! _))
    }

    returning(createOrUpdate(requests.get(asset.id))) { entry =>
      requests.update(asset.id, entry)
      if (!active(asset.id)) {
        verbose(s"adding entry to the queue: ${asset.id}")
        queue.enqueue(entry.queuePlaceHolder)
        checkQueue()
      }
    }
  }

  private def checkQueue(): Unit = {
    verbose(s"checkQueue(), active: $active")

    def shouldStartNext = queue.headOption.flatMap(e => requests.get(e.id)).fold(false)(_ => active.size < MaxConcurrentLoadRequests)

    while(shouldStartNext) {
      if (queue.nonEmpty) {
        val id = queue.dequeue().id
        requests.get(id).fold(debug(s"load entry has been removed for: $id")) { entry =>
          verbose(s"starting load for $entry")
          if (active(id)) error(s"entry was already active: $entry")
          active += id
          load(entry).onComplete { res =>
            requests -= id
            active -= id
            checkQueue()
          }
        }
      }
    }
  }

  //Returns a Future, since once actual loading has started, it doesn't make sense to cancel it and waste the work.
  private def load(entry: LoadEntry): Future[(CacheEntry, Int)] = {
    val id = entry.asset.id

    def onFail(ex: Throwable, attempts: Int = 1) = {
      ex match {
        case _: CancelException =>
          error(s"Loading cancelled for $id after $attempts attempts", ex)
          requests.get(id).foreach(_.state ! ProgressData(0, 0, State.CANCELLED))
        case NonFatal(_) =>
          error(s"Loading failed for $id after $attempts attempts", ex)
          requests.get(id).foreach(_.state ! ProgressData(0, 0, State.FAILED))
      }
      Future.failed(ex)
    }

    def recursive(retries: Int = 0): Future[(CacheEntry, Int)] = {
      debug(s"recursive: retries: $retries")
      val delay =
        if (retries == 0) CancellableFuture.successful({})
        else if (retries > AssetLoaderService.backoff.maxRetries) throw new Exception(MaxRetriesErrorMsg)
        else CancellableFuture.delay(AssetLoaderService.backoff.delay(retries))

      delay.future.flatMap { _ =>
        entry.load().future.map { res =>
          verbose(s"Loading succeeded for: $id")
          requests.get(id).foreach(_.state ! ProgressData(0, 0, State.COMPLETED))
          (res, retries + 1)
        }
      }
    }.recoverWith {
      case ex: DownloadException if ex.isRecoverable => recursive(retries + 1)
      case NonFatal(ex)                              => onFail(ex, attempts = retries + 1)
    }

    entry.state ! ProgressData(0, 0, State.RUNNING)
    entry.promise.tryCompleteWith(recursive()).future
  }
}

object AssetLoaderService {

  val MaxRetriesErrorMsg = "Max retries for loading asset exceeded"

  val MaxConcurrentLoadRequests = 4
  val DefaultExpiryTime = 7.days

  //var for tests
  var backoff: Backoff = new ExponentialBackoff(250.millis, 7.days)

  private[downloads] case class LoadEntry(asset: AssetData, loader: AssetLoader, force: Boolean) {
    val promise = Promise[(CacheEntry, Int)]()
    val state = Signal(ProgressData.Unknown)
    val time = clock.instant()

    def cancel(): Unit =
      promise.tryFailure(new CancelException("Cancelled by user"))

    def load() = loader.loadAsset(asset, state ! _, force)

    override def toString: LogTag = s"LoadEntry(${asset.id}) { force: $force, state: $state, time: $time }"

    def queuePlaceHolder: QueueEntry = QueueEntry(asset.id, time)
  }

  case class QueueEntry(id: AssetId, time: Instant)

  implicit object QueueOrdering extends Ordering[QueueEntry] {
    override def compare(x: QueueEntry, y: QueueEntry): Int =
      Ordering.ordered[Instant].compare(x.time, y.time)
  }
}
