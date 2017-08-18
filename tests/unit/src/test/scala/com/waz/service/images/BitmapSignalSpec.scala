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
package com.waz.service.images

import com.waz.api.NetworkMode
import com.waz.model._
import com.waz.service.NetworkModeService
import com.waz.service.assets.AssetService.BitmapResult
import com.waz.service.assets.AssetService.BitmapResult.BitmapLoaded
import com.waz.service.downloads.AssetLoader.DownloadOnWifiOnlyException
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.CancellableFuture
import com.waz.ui.MemoryImageCache
import com.waz.ui.MemoryImageCache.BitmapRequest
import com.waz.ui.MemoryImageCache.BitmapRequest.Regular
import com.waz.utils.events.{EventContext, SourceSignal}
import com.waz.utils.wrappers.{Bitmap, FakeBitmap}

import scala.concurrent.Future

class BitmapSignalSpec extends AndroidFreeSpec { test =>

  def mockBitmap(asset: AssetData) = FakeBitmap(asset.sizeInBytes.toInt, asset.dimensions.width, asset.dimensions.height, asset.sizeInBytes > 0)

  def image(w: Int, h: Int, mime: Mime = Mime.Image.Png, preview: Option[AssetId] = None) =
    AssetData(mime = mime, metaData = Some(AssetMetaData.Image(Dim2(w, h))), previewId = preview)

  class SignalListener(loader: ImageLoader, asset: AssetData, req: BitmapRequest, network: NetworkModeService = mock[NetworkModeService]) {
    private var results = Seq.empty[BitmapResult]
    private val signal = new AssetBitmapSignal(asset, req, loader, network, { id => Future.successful(assetSource(id)) }, forceDownload = false)
    private val obs = signal { result => results = results :+ result }(EventContext.Global)

    private def medium = results.collectFirst { case BitmapLoaded(b, _) => b }

    def checkLoaded(result: Option[(Int, Int)] = None) = {
      awaitAllTasks

      withClue(results.mkString(", ")) {
        medium.map(b => (b.getWidth, b.getHeight)) shouldEqual result
      }
    }

    def assetSource(id: AssetId): Option[AssetData] = if(id == asset.id) Some(asset) else None
  }

  feature("Wire asset loading") {

    scenario("Request same size image without a preview") {
      val req = Regular(64)
      val result = Some((64,64))
      val asset = image(64, 64)

      val imageCache = mock[MemoryImageCache]
      val loader = mock[ImageLoader]
      (loader.memoryCache _).expects().anyNumberOfTimes().returning(imageCache)

      var bmp: Option[Bitmap] = None
      (imageCache.get _).expects(asset.id, req, asset.width).anyNumberOfTimes().onCall(_ => bmp)

      (loader.hasCachedBitmap _).expects(asset, req).anyNumberOfTimes().onCall { (asset, req) =>
        Future.successful(imageCache.get(asset.id, req, asset.width).isDefined)
      }
      (loader.loadBitmap _).expects(asset, req, *).once().onCall { _ =>
        val bitmap = mockBitmap(asset)
        bmp = Some(bitmap)
        CancellableFuture.successful(bitmap)
      }

      // result should be reported as full image
      val listener = new SignalListener(loader, asset, req)
      listener.checkLoaded(result)

      // this time the result should be loaded from cache (that's why loader.loadBitmap... once)
      listener.checkLoaded(result)
    }

    scenario("Load big size with small source image - no scaling") {
      val req = Regular(500)
      val result = Some((64,64))
      val asset = image(64, 64)

      val imageCache = mock[MemoryImageCache]
      val loader = mock[ImageLoader]
      (loader.memoryCache _).expects().anyNumberOfTimes().returning(imageCache)

      var bmp: Option[Bitmap] = None
      (imageCache.get _).expects(asset.id, req, asset.width).anyNumberOfTimes().onCall(_ => bmp)

      (loader.hasCachedBitmap _).expects(asset, req).anyNumberOfTimes().onCall { (asset, req) =>
        Future.successful(imageCache.get(asset.id, req, asset.width).isDefined)
      }
      (loader.loadBitmap _).expects(asset, req, *).once().onCall { _ =>
        val bitmap = mockBitmap(asset)
        bmp = Some(bitmap)
        CancellableFuture.successful(bitmap)
      }

      val listener = new SignalListener(loader, asset, req)
      listener.checkLoaded(result)

      // this time the result should be loaded from cache
      listener.checkLoaded(result)
    }

    scenario("Load image from preview when small image is requested") {
      val preview = image(64, 64)

      val req = Regular(65)
      val result = Some((64,64))
      val asset = image(512, 512, preview = Some(preview.id))

      val imageCache = mock[MemoryImageCache]
      val loader = mock[ImageLoader]
      (loader.memoryCache _).expects().anyNumberOfTimes().returning(imageCache)

      var bmp: Option[Bitmap] = None
      (imageCache.get _).expects(preview.id, req, preview.width).anyNumberOfTimes().onCall(_ => bmp)

      (loader.hasCachedBitmap _).expects(preview, req).anyNumberOfTimes().onCall { (asset, req) =>
        Future.successful(imageCache.get(asset.id, req, asset.width).isDefined)
      }
      (loader.loadBitmap _).expects(preview, req, *).once().onCall { _ =>
        val bitmap = mockBitmap(preview)
        bmp = Some(bitmap)
        CancellableFuture.successful(bitmap)
      }

      val listener = new SignalListener(loader, asset, req) {
        override def assetSource(id: AssetId): Option[AssetData] = id match {
          case asset.id => Some(asset)
          case preview.id => Some(preview)
          case _ => None
        }
      }

      // loading preview instead of asset
      listener.checkLoaded(result)
    }

    scenario("Load full image when requested is bigger than preview") {
      val preview = image(32, 32)

      val req = Regular(65)
      val result = Some((512,512))
      val asset = image(512, 512, preview = Some(preview.id))

      val imageCache = mock[MemoryImageCache]
      var bmp: Option[Bitmap] = None
      (imageCache.get _).expects(asset.id, req, asset.width).anyNumberOfTimes().onCall(_ => bmp)

      val loader = mock[ImageLoader]
      (loader.memoryCache _).expects().anyNumberOfTimes().returning(imageCache)
      (loader.hasCachedBitmap _).expects(asset, req).anyNumberOfTimes().onCall { (asset, req) =>
        Future.successful(imageCache.get(asset.id, req, asset.width).isDefined)
      }
      (loader.loadBitmap _).expects(asset, req, *).once().onCall { _ =>
        val bitmap = mockBitmap(asset)
        bmp = Some(bitmap)
        CancellableFuture.successful(bitmap)
      }

      val listener = new SignalListener(loader, asset, req) {
        override def assetSource(id: AssetId): Option[AssetData] = id match {
          case asset.id => Some(asset)
          case preview.id => Some(preview)
          case _ => None
        }
      }

      listener.checkLoaded(result)
    }
  }

  feature("Restart on wifi") {
    val req = Regular(64)
    val result = Some((64,64))
    val asset = image(64, 64)

    val imageCache = mock[MemoryImageCache]

    scenario("load restart after switching to wifi") {
      val network = mock[NetworkModeService]
      val networkSignal = new SourceSignal[NetworkMode](Some(NetworkMode._4G))
      (network.networkMode _).expects().anyNumberOfTimes.returning(networkSignal)

      val loader = mock[ImageLoader]
      (loader.memoryCache _).expects().anyNumberOfTimes.returning(imageCache)
      inSequence {
        // before switching to wifi
        (loader.hasCachedBitmap _).expects(asset, req).once.returning(Future.successful(false))
        (loader.loadBitmap _).expects(asset, req, *).once.onCall { _ => CancellableFuture.failed(DownloadOnWifiOnlyException) }

        // after
        (loader.hasCachedBitmap _).expects(asset, req).once.returning(Future.successful(false))
        (loader.loadBitmap _).expects(asset, req, *).once.onCall { _ => CancellableFuture.successful(mockBitmap(asset)) }
      }

      val listener = new SignalListener(loader, asset, req, network)
      listener.checkLoaded(None)

      networkSignal ! NetworkMode.WIFI // switching to wifi should trigger reloading

      listener.checkLoaded(result)
    }
  }
//
//  feature("Local asset loading") {
//    def gifStream = getClass.getResourceAsStream("/gifs/regular1.gif")
//    def gif = GifReader(gifStream).get
//
//    scenario("Load gif from local source") {
//      lazy val asset = AssetData(AssetId(), mime = Mime.Image.Gif, metaData = Some(Image(Dim2(0, 0), Medium)), source = Some(URI.parse("content://test")), convId = Some(RConvId()))
//      gifResult = { _ => Some(gif) }
//      rawDataResult = { _ => Some(LocalData(gifStream, -1))}
//      val listener = new SignalListener(asset, Regular(200))
//      withDelay {
//        withClue(listener.results.mkString(", ")) {
//          listener.results.size should be > 10 // animated gif will produce several frames
//        }
//      }
//    }
//  }
}
