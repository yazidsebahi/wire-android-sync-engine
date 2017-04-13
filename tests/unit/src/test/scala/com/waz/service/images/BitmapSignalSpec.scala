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
import com.waz.{RobolectricUtils, utils}
import com.waz.bitmap.gif.{Gif, GifReader}
import com.waz.cache.LocalData
import com.waz.model.AssetMetaData.Image
import com.waz.model.AssetMetaData.Image.Tag.Medium
import com.waz.model._
import com.waz.service.assets.AssetService.BitmapResult.BitmapLoaded
import com.waz.service.assets.AssetService.BitmapResult
import com.waz.threading.CancellableFuture
import com.waz.ui.MemoryImageCache
import com.waz.ui.MemoryImageCache.BitmapRequest
import com.waz.ui.MemoryImageCache.BitmapRequest.Regular
import org.robolectric.annotation.Config
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._
import scala.util.Try

@Config(manifest = "/src/main/AndroidManifest.xml", reportSdk = 17)
class BitmapSignalSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  var cachedBitmapResult: AssetData => Option[Bitmap] = { _ => None }
  var bitmapResult: AssetData => Option[Bitmap] = { _ => None }
  var cachedGifResult: AssetData => Option[Gif] = { _ => None }
  var gifResult: AssetData => Option[Gif] = { _ => None }
  var rawDataResult: AssetData => Option[LocalData] = { _ => None }
  var cachedDataResult: AssetData => Option[LocalData] = { _ => None }
  var loadDelay: AssetData => FiniteDuration = { _ => Duration.Zero }
  
  def mockBitmap(im: AssetData) = Some(Bitmap.createBitmap(im.dimensions.width, im.dimensions.height, Bitmap.Config.ARGB_8888))
  def mockDelay(preview: FiniteDuration = Duration.Zero, medium: FiniteDuration = Duration.Zero)(im: AssetData) = im.tag match {
    case Medium => medium
    case _ => preview
  }

  lazy val imageCache = new MemoryImageCache(testContext)

  lazy val loader = new ImageLoader(testContext, null, imageCache, null, null, null) {

    override def hasCachedBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Boolean] = CancellableFuture.successful(cachedBitmapResult(asset).isDefined)

    override def hasCachedData(asset: AssetData): CancellableFuture[Boolean] = CancellableFuture.successful(cachedDataResult(asset).isDefined)

    override def loadCachedBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Bitmap] = cachedBitmapResult(asset).fold(CancellableFuture.cancelled[Bitmap]())(CancellableFuture.successful)

    override def loadBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Bitmap] = CancellableFuture.delayed(loadDelay(asset))(bitmapResult(asset).get)

    override def loadCachedGif(asset: AssetData): CancellableFuture[Gif] = cachedGifResult(asset).fold(CancellableFuture.cancelled[Gif]())(CancellableFuture.successful)

    override def loadGif(asset: AssetData): CancellableFuture[Gif] = CancellableFuture.delayed(loadDelay(asset))(gifResult(asset).get)

    override def loadRawCachedData(asset: AssetData): CancellableFuture[Option[LocalData]] = CancellableFuture.successful(cachedDataResult(asset))

    override def loadRawImageData(asset: AssetData): CancellableFuture[Option[LocalData]] = CancellableFuture.delayed(loadDelay(asset))(rawDataResult(asset))
  }

  class SignalListener(asset: AssetData, req: BitmapRequest) {
    var results = Seq.empty[BitmapResult]
    val signal = new AssetBitmapSignal(asset, req, loader, imageCache)
    val obs = signal { result => results = results :+ result }
    
    def medium = results.collectFirst { case BitmapLoaded(b, _) => b }
  }

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

  def checkLoaded(req: BitmapRequest, preview: Option[(Int, Int)] = None, full: Option[(Int, Int)] = None, delay: FiniteDuration = Duration.Zero)(data: AssetData) = {
    val listener = new SignalListener(data, req)
    awaitUi(delay)
    withDelay {
      withClue(listener.results.mkString(", ")) {
        listener.medium.map(b => (b.getWidth, b.getHeight)) shouldEqual full
      }
    }
  }

  feature("Wire asset loading") {

    scenario("Load asset with invalid metadata") {
      checkLoaded(Regular(500), Some((500, 500)), Some((996, 660)))(AssetData(metaData = Some(AssetMetaData.Image(Dim2(996, 660), Medium))))
    }

    scenario("Request small image when only preview is available") {
      fail()
      // result should be reported as full image
//      checkLoaded(Regular(64), None, Some((64,64)))(image("preview", 64, 64, 512, 512))
//
//      cachedBitmapResult = mockBitmap // result should be the same when loaded from cache
//      checkLoaded(Regular(64), None, Some((64,64)))(image("preview", 64, 64, 512, 512))
    }

    scenario("Load big image when only preview is available") {
      fail()
//      val data = AssetData(metaData = Some(AssetMetaData.Image(Dim2(64, 64), "preview")))
//      checkLoaded(Regular(500), Some((500,500)), None)(image("preview", 64, 64, 512, 512))
//
//      cachedBitmapResult = mockBitmap // result should be the same when loaded from cache
//      checkLoaded(Regular(500), Some((500,500)), None)(image("preview", 64, 64, 512, 512))
    }

    scenario("Load image when both versions are available") {
      fail()
//      checkLoaded(Regular(500), Some((500,500)), Some((512, 512)))(image("preview", 64, 64, 512, 512), image("medium", 512, 512, 512, 512))
    }

    scenario("Only full image available") {
      checkLoaded(Regular(500), None, Some((512, 512)))(AssetData(metaData = Some(AssetMetaData.Image(Dim2(512, 512), Medium))))
    }

    scenario("Loading full image, then preview") {
      fail()
//      loadDelay = mockDelay(preview = 500.millis)
//      checkLoaded(Regular(500), None, Some((512, 512)), 1.second)(image("preview", 64, 64, 512, 512), image("medium", 512, 512, 512, 512))
    }
  }

  feature("Local asset loading") {
    def gifStream = getClass.getResourceAsStream("/gifs/regular1.gif")
    def gif = GifReader(gifStream).get

    scenario("Load gif from local source") {
      lazy val asset = AssetData(AssetId(), metaData = Some(Image(Dim2(0, 0), Medium)), source = Some(utils.URI.parse("content://test")), convId = Some(RConvId()))
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
      fail()
//      lazy val asset = ImageAssetData(AssetId(), RConvId(), Seq(image("preview", 64, 64, 512, 512), ImageData("full", Mime.Unknown, 512, 512, 512, 512, 0, url = Some("content://test"))))
//      gifResult = { _ => Some(gif) }
//      rawDataResult = { _ => Some(LocalData(gifStream, -1))}
//      loadDelay = { data => if (data.tag == "preview") Duration.Zero else 250.millis }
//      val listener = new SignalListener(asset, Regular(200))
//      withDelay {
//        withClue(listener.results.mkString(", ")) {
//          listener.preview.map(b => (b.getWidth, b.getHeight)) shouldEqual Some((200, 200)) // results should include preview
//          listener.results.size should be > 10 // animated gif will produce several frames
//        }
//      }
    }
  }
}
