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
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.{Callback, ProgressData}
import com.waz.cache._
import com.waz.content.{Database, Preferences}
import com.waz.model.AssetData.RemoteData
import com.waz.model.{Mime, _}
import com.waz.service.NetworkModeService
import com.waz.service.downloads.{AssetDownloader, DownloadRequest, Downloader, DownloaderService}
import com.waz.service.downloads.DownloadRequest.{AssetRequest, WireAssetRequest}
import com.waz.testutils.Matchers._
import com.waz.testutils.MockGlobalModule
import com.waz.threading.CancellableFuture
import com.waz.threading.CancellableFuture.CancelException
import com.waz.utils.ExponentialBackoff
import com.waz.utils.events.EventContext
import com.waz.utils.returning
import com.waz.znet.Request.ProgressCallback
import com.waz.{RobolectricUtils, service}
import org.robolectric.shadows.ShadowLog
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}
import org.scalamock.scalatest.MockFactory

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Failure
import scala.language.reflectiveCalls

class DownloaderServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils with MockFactory {

  lazy val downloads = new mutable.HashMap[CacheKey, ProgressCallback => CancellableFuture[Option[CacheEntry]]]

  lazy val global = new MockGlobalModule()

  lazy val downloader = new service.downloads.DownloaderService(testContext, global.cache, global.prefs, global.network)

  implicit lazy val assetDownloader = new AssetDownloader(null, null) {
    override def load(req: AssetRequest, callback: Callback) = downloads(req.cacheKey).apply(callback)
  }

  after {
    ShadowLog.stream = null
  }

  def uri(id: RAssetId = RAssetId()): Uri =  Uri.parse(s"content://$id")
  def fakeDownload(id: RAssetId = RAssetId(), conv: RConvId = RConvId()) = new Download(WireAssetRequest(CacheKey(), AssetId(), RemoteData(Some(id), None, None, None), Some(conv), Mime.Unknown), 100)

  feature("Throttling") {

    scenario("Execute limited number of downloads concurrently") {
      val ds = Seq.fill(10)(fakeDownload())

      val futures = ds.map(d => downloader.download(d.req, withRetries = false))
      val max = service.downloads.DownloaderService.MaxConcurrentDownloads
      withDelay { ds.filter(_.started) should have size max } (10.seconds)

      ds.filter(_.started).head.setResult(None)
      withDelay { ds.filter(_.started) should have size max + 1 }

      ds.filter(d => d.started && !d.done).head.setResult(None)
      withDelay { ds.filter(_.started) should have size max + 2 }

      ds.foreach(d => if (!d.done) d.setResult(None))
      Await.result(CancellableFuture.sequence(futures), 5.seconds)
    }

    scenario("Limit concurrent requests even further if they are cancelled by UI") {
      import service.downloads.DownloaderService._

      val ds = Seq.fill(10)(fakeDownload())
      ds foreach { d => downloader.download(d.req).cancel()("test") }

      withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads}

      for (i <- MaxBackgroundDownloads until MaxConcurrentDownloads) {
        ds.filter(d => d.started && !d.done).head.setResult(None)
        withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads } // no new downloads are started
      }

      ds.filter(d => d.started && !d.done).head.setResult(None)
      withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads + 1 }

      ds.foreach(d => if (!d.done) d.setResult(None))
      awaitUi(1.second)
    }
  }

  feature("Batching") {

    scenario("Perform download only once for duplicate requests") {
      val d = fakeDownload()

      val futures = Seq.fill(1)(downloader.download(d.req, withRetries = false))

      withDelay(d.started shouldEqual true)
      d.setResult(None)
      Await.result(CancellableFuture.sequence(futures), 5.seconds) shouldEqual Seq.fill(1)(None)
    }

  }

  feature("Download ordering") {

    scenario("Cancelled request should be moved to end of queue") {
      val ds = Seq.fill(10)(fakeDownload())
      ds foreach { d => downloader.download(d.req).cancel()("test") }

      withDelay { ds.filter(_.started) should have size service.downloads.DownloaderService.MaxConcurrentDownloads}

      val d = fakeDownload()
      val f = downloader.download(d.req)

      ds.filter(_.started).head.setResult(None)
      withDelay { d.started shouldEqual true } // last added download will be executed next, since it wasn't cancelled
      d.setResult(None)

      ds.foreach(d => if (!d.done) d.setResult(None))
      withDelay(ds foreach (_.started shouldEqual true))
      awaitUi(100.millis)
    }
  }

  feature("Download progress") {

    scenario("Report progress for ongoing download") {
      val d = fakeDownload()
      downloader.download(d.req)

      var ps = Seq.empty[ProgressData]

      downloader.getDownloadState(d.req.cacheKey) { progress =>
        ps = ps :+ progress
      } (EventContext.Global)

      withDelay {
        d.started shouldEqual true
        ps should not be empty
        ps.last shouldEqual ProgressData(0, 100, State.RUNNING)
      }

      d.setProgress(10)
      withDelay { ps.last shouldEqual ProgressData(10, 100, State.RUNNING) }

      d.setProgress(20)
      withDelay { ps.last shouldEqual ProgressData(20, 100, State.RUNNING) }

      d.setResult(None)
      withDelay { ps.last shouldEqual ProgressData(0, 0, State.FAILED) }
    }

  }

  feature("Cancelling") {

    scenario("Cancel ongoing download") {
      val d = fakeDownload()
      val f = downloader.download(d.req)
      downloader.cancel(d.req.cacheKey)

      intercept[CancelException] {
        f.await()
      }

      d.cancelled shouldEqual true
    }

    scenario("Cancel multiple downloads in reverse order") {
      val ds = Seq.fill(10)(fakeDownload())
      val fs = ds map { d => downloader.download(d.req) }

      ds.reverse.foreach { d => downloader.cancel(d.req.cacheKey) }

      fs foreach { f =>
        intercept[CancelException] {
          f.await()
        }
      }
    }
  }

  val TIMEOUT = 5.seconds
  private def waitForResult[T](f: => Future[T]): T = Await.result(f, TIMEOUT)

  val database = stub[Database]
  val cacheStorage = stub[CacheStorage]
  val cacheService = CacheService(context, database, cacheStorage)
  val result = new CacheEntry(CacheEntryData(CacheKey("...")), cacheService)

  private def createDownloader(ctx: Context = testContext,
                               cache: CacheService = global.cache,
                               prefs: Preferences = global.prefs,
                               network: NetworkModeService = global.network
                              )
                              (downloadOnceBody: (Int) => CancellableFuture[Option[CacheEntry]]) =
    new service.downloads.DownloaderService(ctx, cache, prefs, network){
      var retry = 0
      override protected def downloadOnce[A <: DownloadRequest](req: A, force: Boolean = false)
                                                               (implicit loader: Downloader[A],
                                                                expires: Expiration = DownloaderService.DefaultExpiryTime
                                                               ): CancellableFuture[Option[CacheEntry]] =
        returning(downloadOnceBody(retry)){ _ => retry += 1 }
    }

  feature("Download retries") {
    scenario("download with no retries") {
      val downloader = createDownloader() { _ => CancellableFuture.successful(Some(result)) }
      val fakeD = fakeDownload()

      waitForResult{ downloader.download(fakeD.req, withRetries = false) } shouldEqual Some(result)
      downloader.retry shouldEqual 1
    }

    scenario("download with retries but at once") {
      val downloader = createDownloader() { _ => CancellableFuture.successful(Some(result)) }
      val fakeD = fakeDownload()

      waitForResult{ downloader.download(fakeD.req) } shouldEqual Some(result)
      downloader.retry shouldEqual 1
    }

    scenario("download after one retry") {
      val downloader = createDownloader() { retry =>
        if(retry == 0) CancellableFuture.successful(None)
        else CancellableFuture.successful(Some(result))
      }
      val fakeD = fakeDownload()

      waitForResult{ downloader.download(fakeD.req) } shouldEqual Some(result)
      downloader.retry shouldEqual 2
    }

    scenario("download after two retries") {
      val downloader = createDownloader() { retry =>
        if(retry < 2) CancellableFuture.successful(None)
        else CancellableFuture.successful(Some(result))
      }
      val fakeD = fakeDownload()

      waitForResult{ downloader.download(fakeD.req) } shouldEqual Some(result)
      downloader.retry shouldEqual 3
    }

    scenario("give up after max retries") {
      val downloader = createDownloader() { _ => CancellableFuture.successful(None) }
      val fakeD = fakeDownload()

      DownloaderService.setBackoff(new ExponentialBackoff(0.millis, 0.millis){
        override val maxRetries = 3
        override def delay(retry: Int, minDelay: FiniteDuration = Duration.Zero): FiniteDuration = 0.millis
      })

      waitForResult{ downloader.download(fakeD.req) } shouldEqual None
      downloader.retry shouldEqual 4
    }
  }

  class Download(val req: AssetRequest, val size: Int = 0) extends (ProgressCallback => CancellableFuture[Option[CacheEntry]]) {
    @volatile var started = false
    @volatile var cb: ProgressCallback = _

    private val promise = Promise[Option[CacheEntry]]

    def done = promise.isCompleted
    def cancelled = promise.isCompleted && (promise.future.value match {
      case Some(Failure(_: CancelException)) => true
      case _ => false
    })

    downloads(req.cacheKey) = this

    override def apply(cb: ProgressCallback): CancellableFuture[Option[CacheEntry]] = {
      assert(!started, "Download should be started only once")
      this.cb = cb
      started = true
      cb.apply(ProgressData(0, size, State.RUNNING))

      new CancellableFuture(promise)
    }

    def setProgress(p: Int): Unit = {
      cb.apply(ProgressData(p, size, State.RUNNING))
    }

    def setResult(result: Option[CacheEntry]): Unit = promise.success(result)
  }
}
