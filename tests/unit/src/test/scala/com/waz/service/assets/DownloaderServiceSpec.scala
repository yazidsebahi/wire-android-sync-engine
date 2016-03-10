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

import android.net.Uri
import com.waz.RobolectricUtils
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.ProgressData
import com.waz.cache.CacheEntry
import com.waz.model.{RConvId, RImageDataId}
import com.waz.service.assets.DownloadKey.{External, WireAsset}
import com.waz.sync.client.ImageAssetClient
import com.waz.testutils.MockGlobalModule
import com.waz.threading.CancellableFuture
import com.waz.znet.Request
import com.waz.znet.Request.ProgressCallback
import com.waz.znet.ZNetClient.EmptyClient
import org.robolectric.shadows.ShadowLog
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._

class DownloaderServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils {

  lazy val downloads = new mutable.HashMap[Request[Unit], ProgressCallback => Option[CacheEntry]]

  lazy val global = new MockGlobalModule()

  lazy val downloader = new DownloaderService(testContext, global.cache, global.prefs, global.network)

  implicit lazy val client = new ImageAssetClient(new EmptyClient, global.cache) {
    override def loadImageAsset(req: Request[Unit], cb: ProgressCallback): CancellableFuture[Option[CacheEntry]] = CancellableFuture { downloads(req).apply(cb) }
  }

  after {
    ShadowLog.stream = null
  }

  def uri(id: RImageDataId = RImageDataId()): Uri =  Uri.parse(s"content://$id")
  def fakeDownload(id: RImageDataId = RImageDataId(), conv: RConvId = RConvId()) = new Download(WireAsset(id, conv), 100)
  
  feature("Throttling") {

    scenario("Execute limited number of downloads concurrently") {
      val ds = Seq.fill(10)(fakeDownload())

      val futures = ds.map(d => downloader.download(d.key))
      val max = DownloaderService.MaxConcurrentDownloads
      withDelay { ds.filter(_.started) should have size max } (10.seconds)

      ds.filter(_.started).head.setResult(None)
      withDelay { ds.filter(_.started) should have size max + 1 }

      ds.filter(d => d.started && !d.done).head.setResult(None)
      withDelay { ds.filter(_.started) should have size max + 2 }

      ds.foreach(_.setResult(None))
      Await.result(CancellableFuture.sequence(futures), 5.seconds)
    }

    scenario("Limit concurrent requests even further if they are cancelled by UI") {
      import DownloaderService._

      val ds = Seq.fill(10)(fakeDownload())
      ds foreach { d => downloader.download(d.key).cancel()("test") }

      withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads}

      for (i <- MaxBackgroundDownloads until MaxConcurrentDownloads) {
        ds.filter(d => d.started && !d.done).head.setResult(None)
        withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads } // no new downloads are started
      }

      ds.filter(d => d.started && !d.done).head.setResult(None)
      withDelay { ds.filter(_.started) should have size MaxConcurrentDownloads + 1 }

      ds.foreach(_.setResult(None))
      awaitUi(1.second)
    }
  }

  feature("Batching") {

    scenario("Perform download only once for duplicate requests") {
      val d = fakeDownload()

      val futures = Seq.fill(5)(downloader.download(d.key))

      withDelay(d.started shouldEqual true)
      d.setResult(None)
      Await.result(CancellableFuture.sequence(futures), 5.seconds) shouldEqual Seq.fill(5)(None)
    }

  }

  feature("Download ordering") {

    scenario("Cancelled request should be moved to end of queue") {
      val ds = Seq.fill(10)(fakeDownload())
      ds foreach { d => downloader.download(d.key).cancel()("test") }

      withDelay { ds.filter(_.started) should have size DownloaderService.MaxConcurrentDownloads}

      val d = fakeDownload()
      val f = downloader.download(d.key)

      ds.filter(_.started).head.setResult(None)
      withDelay { d.started shouldEqual true } // last added download will be executed next, since it wasn't cancelled
      d.setResult(None)

      ds.foreach(_.setResult(None))
      withDelay(ds foreach (_.started shouldEqual true))
      awaitUi(100.millis)
    }
  }

  feature("Download progress") {

    scenario("Report progress for ongoing download") {
      // TODO
    }

  }


  class Download(val key: DownloadKey, val size: Int = 0) extends (ProgressCallback => Option[CacheEntry]) {
    @volatile var started = false
    @volatile var cb: ProgressCallback = _
    @volatile var done = false
    @volatile var result = Option.empty[CacheEntry]

    downloads(key.request()) = this

    override def apply(cb: ProgressCallback): Option[CacheEntry] = {
      assert(!started, "Download should be started only once")
      this.cb = cb
      started = true
      cb.apply(ProgressData(0, size, State.RUNNING))
      while (!done) {
        synchronized(wait())
      }
      result
    }

    def setProgress(p: Int): Unit = {
      cb.apply(ProgressData(p, size, State.RUNNING))
    }

    def setResult(result: Option[CacheEntry]): Unit = {
      this.result = result
      this.done = true
      synchronized(notifyAll())
    }
  }
}
