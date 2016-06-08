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

import android.graphics.Bitmap
import android.support.v4.content.ContextCompat
import com.waz.RobolectricUtils
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.bitmap.gif.{Gif, GifReader}
import com.waz.cache.LocalData
import com.waz.model._
import com.waz.service.assets
import assets.AssetService.BitmapRequest.Regular
import assets.AssetService.BitmapResult.BitmapLoaded
import assets.AssetService.{BitmapRequest, BitmapResult}
import com.waz.threading.CancellableFuture
import com.waz.ui.MemoryImageCache
import org.robolectric.annotation.Config
import org.robolectric.shadows.ShadowLog
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._
import scala.util.Try

@Config(manifest = "/src/main/AndroidManifest.xml", reportSdk = 17)
class BitmapSignalSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  var cachedBitmapResult: ImageData => Option[Bitmap] = { _ => None }
  var bitmapResult: ImageData => Option[Bitmap] = { _ => None }
  var cachedGifResult: ImageData => Option[Gif] = { _ => None }
  var gifResult: ImageData => Option[Gif] = { _ => None }
  var rawDataResult: ImageData => Option[LocalData] = { _ => None }
  var cachedDataResult: ImageData => Option[LocalData] = { _ => None }
  var loadDelay: ImageData => FiniteDuration = { _ => Duration.Zero }
  
  def mockBitmap(im: ImageData) = Some(Bitmap.createBitmap(im.width, im.height, Bitmap.Config.ARGB_8888))
  def mockDelay(preview: FiniteDuration = Duration.Zero, medium: FiniteDuration = Duration.Zero)(im: ImageData) = im.tag match {
    case "medium" => medium
    case _ => preview
  }

  lazy val imageCache = new MemoryImageCache(testContext)

  lazy val loader = new ImageLoader(testContext, null, imageCache, null, null, null) {

    override def hasCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Boolean] = CancellableFuture.successful(cachedBitmapResult(im).isDefined)

    override def hasCachedData(asset: ImageAssetData, im: ImageData): CancellableFuture[Boolean] = CancellableFuture.successful(cachedDataResult(im).isDefined)

    override def loadCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] = cachedBitmapResult(im).fold(CancellableFuture.cancelled[Bitmap]())(CancellableFuture.successful)

    override def loadBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] = CancellableFuture.delayed(loadDelay(im))(bitmapResult(im).get)

    override def loadCachedGif(asset: ImageAssetData, im: ImageData): CancellableFuture[Gif] = cachedGifResult(im).fold(CancellableFuture.cancelled[Gif]())(CancellableFuture.successful)

    override def loadGif(asset: ImageAssetData, im: ImageData): CancellableFuture[Gif] = CancellableFuture.delayed(loadDelay(im))(gifResult(im).get)

    override def loadRawCachedData(im: ImageData, convId: RConvId): CancellableFuture[Option[LocalData]] = CancellableFuture.successful(cachedDataResult(im))

    override def loadRawImageData(im: ImageData, convId: RConvId): CancellableFuture[Option[LocalData]] = CancellableFuture.delayed(loadDelay(im))(rawDataResult(im))
  }

  class SignalListener(asset: ImageAssetData, req: BitmapRequest) {
    var results = Seq.empty[BitmapResult]
    val signal = new BitmapSignal(asset, req, loader, imageCache)
    val obs = signal { result => results = results :+ result }
    
    def preview = results.collectFirst { case BitmapLoaded(b, true, _) => b }
    def medium = results.collectFirst { case BitmapLoaded(b, false, _) => b }
  }

  def image(tag: String, w: Int, h: Int, ow: Int, oh: Int, mime: String = Mime.Png, size: Int = 0) = ImageData(tag, mime, w, h, ow, oh, size, Some(RAssetDataId()))

  def asset(is: ImageData*) = ImageAssetData(AssetId(), RConvId(), is)

  before {
    Try { ContextCompat.getDrawable(testContext, android.R.color.transparent) } // force resource loading
    cachedBitmapResult = { _ => None }
    cachedGifResult = { _ => None }
    gifResult = { _ => None }
    bitmapResult = mockBitmap
    rawDataResult = { _ => None }
    cachedDataResult = { _ => None }
    loadDelay = mockDelay(medium = 500.millis)
  }

  after {
    ShadowLog.stream = null
  }

  def checkLoaded(req: BitmapRequest, preview: Option[(Int, Int)] = None, full: Option[(Int, Int)] = None, delay: FiniteDuration = Duration.Zero)(im: ImageData*) = {
    val data = asset(im: _*)
    val listener = new SignalListener(data, req)
    awaitUi(delay)
    withDelay {
      withClue(listener.results.mkString(", ")) {
        listener.preview.map(b => (b.getWidth, b.getHeight)) shouldEqual preview
        listener.medium.map(b => (b.getWidth, b.getHeight)) shouldEqual full
      }
    }
  }

  feature("Wire asset loading") {

    scenario("Load asset with invalid metadata") {
      ShadowLog.stream = System.out
      checkLoaded(Regular(500), Some((500, 500)), Some((996, 660)))(image("smallProfile", 280, 280, 0, 0), image("medium", 996, 660, 0, 0))
    }

    scenario("Request small image when only preview is available") {
      // result should be reported as full image
      checkLoaded(Regular(64), None, Some((64,64)))(image("preview", 64, 64, 512, 512))

      cachedBitmapResult = mockBitmap // result should be the same when loaded from cache
      checkLoaded(Regular(64), None, Some((64,64)))(image("preview", 64, 64, 512, 512))
    }

    scenario("Load big image when only preview is available") {
      checkLoaded(Regular(500), Some((500,500)), None)(image("preview", 64, 64, 512, 512))

      cachedBitmapResult = mockBitmap // result should be the same when loaded from cache
      checkLoaded(Regular(500), Some((500,500)), None)(image("preview", 64, 64, 512, 512))
    }

    scenario("Load image when both versions are available") {
      checkLoaded(Regular(500), Some((500,500)), Some((512, 512)))(image("preview", 64, 64, 512, 512), image("medium", 512, 512, 512, 512))
    }

    scenario("Only full image available") {
      checkLoaded(Regular(500), None, Some((512, 512)))(image("medium", 512, 512, 512, 512))
    }

    scenario("Loading full image, then preview") {
      loadDelay = mockDelay(preview = 500.millis)
      checkLoaded(Regular(500), None, Some((512, 512)), 1.second)(image("preview", 64, 64, 512, 512), image("medium", 512, 512, 512, 512))
    }
  }

  feature("Local asset loading") {
    def gifStream = getClass.getResourceAsStream("/gifs/regular1.gif")
    def gif = GifReader(gifStream).get

    scenario("Load gif from local source") {
      lazy val asset = ImageAssetData(AssetId(), RConvId(), Seq(ImageData("full", Mime.Unknown, 0, 0, 0, 0, 0, url = Some("content://test"))))
      gifResult = { _ => Some(gif) }
      rawDataResult = { _ => Some(LocalData(gifStream, -1))}
      val listener = new SignalListener(asset, Regular(200))
      withDelay {
        withClue(listener.results.mkString(", ")) {
          listener.results.size should be > 10 // animated gif will produce several frames
        }
      }
    }

    scenario("Load local gif with preview") {
      lazy val asset = ImageAssetData(AssetId(), RConvId(), Seq(image("preview", 64, 64, 512, 512), ImageData("full", Mime.Unknown, 512, 512, 512, 512, 0, url = Some("content://test"))))
      gifResult = { _ => Some(gif) }
      rawDataResult = { _ => Some(LocalData(gifStream, -1))}
      loadDelay = { data => if (data.tag == "preview") Duration.Zero else 250.millis }
      val listener = new SignalListener(asset, Regular(200))
      withDelay {
        withClue(listener.results.mkString(", ")) {
          listener.preview.map(b => (b.getWidth, b.getHeight)) shouldEqual Some((200, 200)) // results should include preview
          listener.results.size should be > 10 // animated gif will produce several frames
        }
      }
    }
  }
}
