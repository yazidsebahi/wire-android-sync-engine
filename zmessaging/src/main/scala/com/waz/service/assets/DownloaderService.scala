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
package com.waz.service.assets

import android.content.Context
import android.net.Uri
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.ProgressData
import com.waz.cache.{CacheEntry, CacheService, Expiration}
import com.waz.model.{RConvId, RImageDataId}
import com.waz.service.{NetworkModeService, PreferenceService}
import com.waz.sync.client.ImageAssetClient
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils._
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zms.R
import com.waz.znet.Request

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/**
 * Keeps track of all download requests in priority queue, executes more important downloads first.
 * Priority is based on request start and cancel times.
 * Cancelling request doesn't cancel actual download, just postpones it for background download later.
 * Most of the time, requested asset will be needed in a future, even if current request is cancelled.
 *
 * TODO: implement download pause/resume (with range headers),
 * with this we could pause less important requests when new request is added
 * we could then remove background downloads limit, which is currently used to make sure there is
 * some bandwidth reserved for important requests
 */
class DownloaderService(context: Context, cache: CacheService, prefs: PreferenceService, network: NetworkModeService) {
  import DownloaderService._
  private implicit val dispatcher = new SerialDispatchQueue(name = "DownloaderService")
  private implicit val ev = EventContext.Global

  private val downloads = new mutable.HashMap[DownloadKey, DownloadEntry]()
  private val active = new mutable.HashSet[DownloadKey]
  private val queue = new DownloadQueue

  private lazy val downloadPrefAlways = Try(context.getResources.getString(R.string.zms_image_download_default_value)).getOrElse("always")
  private lazy val downloadPrefKey = Try(context.getResources.getString(R.string.zms_image_download_preference_key)).getOrElse("zms_pref_image_download") // hardcoded value used in tests

  private lazy val downloadPref = prefs.uiPreferenceStringSignal(downloadPrefKey, downloadPrefAlways).signal
  private lazy val downloadEnabled = Signal.or(downloadPref.map(_ == downloadPrefAlways), network.networkMode.map(_ == NetworkMode.WIFI))

  downloadEnabled.disableAutowiring()

  def getDownloadState(key: DownloadKey): Signal[ProgressData] = Signal.future(Future(downloads.get(key))) flatMap {
    case Some(download) => download.state
    case None => Signal(ProgressData.Unknown)
  }

  def download(key: DownloadKey, force: Boolean = false)(implicit client: ImageAssetClient, expires: Expiration = DownloaderService.DefaultExpiryTime): CancellableFuture[Option[CacheEntry]] = {
    verbose(s"download($key, $force)")
    val p = Promise[Option[CacheEntry]]()
    p.tryCompleteWith(downloadEntry(key, client, force).flatMap(_.promise.future))

    new CancellableFuture(p) {
      override def cancel()(implicit tag: LogTag): Boolean = {
        onListenerCancelled(key)
        super.cancel()(tag)
      }
    }
  }

  private def downloadEntry(key: DownloadKey, client: ImageAssetClient, force: Boolean = false)(implicit expires: Expiration) = Future {
    verbose(s"downloadEntry($key, $force)")
    returning(downloads.getOrElseUpdate(key, new DownloadEntry(key, client))) { entry =>
      entry.listenersCount += 1
      if (!active(key)) {
        entry.force = entry.force || force
        entry.time = System.currentTimeMillis()
        entry.expiration = Expiration(entry.expiration.timeout max expires.timeout)
        queue.put(key, uiWaiting = true, entry.time)
        checkQueue()
      }
    }
  }

  private def onListenerCancelled(key: DownloadKey) = Future {
    downloads.get(key) foreach { entry =>
      entry.listenersCount -= 1
      if (entry.listenersCount == 0 && !active(key))
        queue.put(key, uiWaiting = false, entry.time)
    }
  }

  private def checkQueue(): Unit = {
    verbose(s"checkQueue(), active: $active")

    def shouldStartNext = queue.peek().flatMap(downloads.get).fold(false) { entry =>
      entry.listenersCount > 0 && active.size < MaxConcurrentDownloads || active.size < MaxBackgroundDownloads
    }

    while(shouldStartNext) {
      queue.poll() foreach { key =>
        downloads.get(key).fold(error(s"no download entry found for: $key")) { entry =>
          verbose(s"starting download for $entry")
          if (active(key)) error(s"entry was already active: $entry")
          active += key
          doDownload(entry) onComplete { res =>
            verbose(s"download complete for: $key, result: $res")
            entry.promise.tryComplete(res)
            downloads -= key
            active -= key
            checkQueue()
          }
        }
      }
    }
  }

  private def doDownload(download: DownloadEntry): CancellableFuture[Option[CacheEntry]] = {
    verbose(s"doDownload($download)")

    def actualDownload = {
      verbose(s"actualDownload $download")
      download.client.loadImageAsset(download.key.request(), download.state ! _)
    }

    def done(state: State) = {
      verbose(s"done($state), $download")
      download.state ! ProgressData(0, 0, state)
      downloads.remove(download.key)
      active -= download.key
      checkQueue()
    }

    if (download.force || downloadEnabled.currentValue.exists(identity)) {
      download.state ! ProgressData(0, 0, State.RUNNING)

      val result = actualDownload

      result.onComplete {
        case Success(Some(entry)) => done(State.COMPLETED)
        case Success(None) => done(State.FAILED)
        case Failure(ex: CancelException) =>
          download.state ! ProgressData(0, 0, State.CANCELLED)
          active -= download.key
          queue.put(download.key, uiWaiting = false, download.time)
          checkQueue()
        case Failure(ex) =>
          error(s"download failed for: $download", ex)
          done(State.FAILED)
      }

      result
    } else {
      info(s"Downloading is disabled by download preference.")
      CancellableFuture.successful(None)
    }
  }
}

object DownloaderService {
  private implicit val tag: LogTag = logTagFor[DownloaderService]

  val MaxConcurrentDownloads = 4
  // number of concurrent downloads for request not immediately required by UI
  val MaxBackgroundDownloads = 1
  val DefaultExpiryTime = 7.days

  private[assets] class DownloadEntry(val key: DownloadKey, val client: ImageAssetClient) {
    val promise = Promise[Option[CacheEntry]]()
    var force: Boolean = false
    var time = System.currentTimeMillis()
    var listenersCount = 0
    val state = Signal(ProgressData.Unknown)
    var expiration: Expiration = Duration.Zero

    override def toString: LogTag = s"DownloadEntry($key) { force: $force, listeners: $listenersCount, state: $state, time: $time }"
  }
}

sealed trait DownloadKey {
  def request(): Request[Unit]
}

object DownloadKey {
  case class OtrWireAsset(id: RImageDataId, conv: RConvId) extends DownloadKey {
    override def request(): Request[Unit] = Request.Get(ImageAssetClient.getOtrAssetPath(conv, id))
  }
  case class WireAsset(id: RImageDataId, conv: RConvId) extends DownloadKey {
    override def request(): Request[Unit] = Request.Get(ImageAssetClient.getAssetPath(conv, id))
  }
  case class External(uri: Uri) extends DownloadKey {
    override def request(): Request[Unit] = Request[Unit](absoluteUri = Some(uri), requiresAuthentication = false)
  }
  case class Proxied(path: String) extends DownloadKey {
    override def request(): Request[Unit] = Request.Get(path)
  }
}
