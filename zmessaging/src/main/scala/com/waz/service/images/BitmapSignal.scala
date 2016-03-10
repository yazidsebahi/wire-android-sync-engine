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
import com.waz.ZLog._
import com.waz.bitmap
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.bitmap.gif.{Gif, GifAnimator}
import com.waz.bitmap.{BitmapPolka, BitmapUtils}
import com.waz.cache.LocalData
import com.waz.model.{ImageAssetData, ImageData}
import com.waz.service.images.ImageAssetService.BitmapRequest._
import com.waz.service.images.ImageAssetService.BitmapResult.{BitmapLoaded, LoadingFailed}
import com.waz.service.images.ImageAssetService.{BitmapRequest, BitmapResult}
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.Threading.Implicits.Background
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache
import com.waz.utils.events.Signal
import com.waz.utils.{IoUtils, WeakMemCache}

// TODO: we could listen for AssetData changes and restart this signal,
// this isn't really needed currently since ImageAsset will be updated and UI will restart this loading
// but this could be useful in future, if UI forgets to reload or we could stop requiring them to do so
class BitmapSignal(img: ImageAssetData, req: BitmapRequest, imageLoader: ImageLoader, imageCache: MemoryImageCache) extends Signal[BitmapResult] { signal =>
  private var future = CancellableFuture.successful[Unit](())
  private implicit val tag: LogTag = logTagFor[BitmapSignal]

  require(img.versions.nonEmpty, s"Passed empty data to bitmap signal: $img")

  override protected def onWire(): Unit = {
    if (req.width > 0) { // ignore requests with invalid size
      future = req match {
        case Single(_) => load(img, EmptyLoader, new BitmapLoader(img, chooseImage(img, static = true)))
        case Round(_, _, _) => load(img, new BitmapLoader(img, img.versions.head), new BitmapLoader(img, chooseImage(img, static = true)))
        case Static(_, _) => load(img, new BitmapLoader(img, img.versions.head), new BitmapLoader(img, chooseImage(img, static = true)))
        case _ => load(img, new BitmapLoader(img, img.versions.head), getLoader(img, chooseImage(img)))
      }
    }
  }

  override protected def onUnwire(): Unit = future.cancel()

  /*
   * Tries loading and processing of full cached image first
   * If loading (od processing) from cache fails does following:
   * - starts preview loading / processing
   * - start full image loading
   * - cancels preview when full image is loaded
   * - starts full image processing
   */
  private def load(img: ImageAssetData, preview: ImageLoader, full: ImageLoader) = {
    val future = full.loadCached() flatMap {
      case Some(data) =>
        full.process(data)
      case None => // will try with download
        val pf = preview.load() flatMap preview.process
        val ff = full.load()
        ff.onSuccess { case _ => pf.cancel() }
        ff flatMap full.process
    }
    future onFailure {
      case _: CancelException => // ignore
      case ex =>
        warn("bitmap loading failed", ex)
        signal publish LoadingFailed(ex)
    }
    future
  }

  private def getLoader(img: ImageAssetData, im: ImageData) =
    im.mime.toLowerCase match {
      case Mime.Unknown => new MimeCheckLoader(img, im)
      case Mime.Gif => new GifLoader(img, im)
      case _ => new BitmapLoader(img, im)
    }

  /**
   * @param static - true if only static image is required, will choose static formats before gifs if sizes are the same.
   */
  private def chooseImage(img: ImageAssetData, static: Boolean = false) = {
    if (! static && img.versions.last.mime == Mime.Gif) img.versions.last // always use the last version when loading gifs, smaller versions are not animated
    else {
      val max = img.versions.find(d => d.width >= req.width).getOrElse(img.versions.last)
      if (static) img.versions.find(_.width == max.width).getOrElse(max)
      else max
    }
  }

  sealed trait ImageLoader {
    type Data

    def loadCached(): CancellableFuture[Option[Data]]
    def load(): CancellableFuture[Data]
    def process(result: Data): CancellableFuture[Unit]
  }

  object EmptyLoader extends ImageLoader {
    override type Data = Unit
    override def loadCached() = CancellableFuture.successful(None)
    override def load() = CancellableFuture.successful(())
    override def process(result: Unit) = CancellableFuture.successful(())
  }

  class MimeCheckLoader(img: ImageAssetData, im: ImageData) extends ImageLoader {
    override type Data = Either[Bitmap, Gif]

    lazy val gifLoader = new GifLoader(img, im)
    lazy val bitmapLoader = new BitmapLoader(img, im)

    def detectMime(data: LocalData) = Threading.IO { IoUtils.withResource(data.inputStream)(BitmapUtils.detectImageType) }

    override def loadCached() = imageLoader.loadRawCachedData(im, img.convId) flatMap {
      case Some(data) => detectMime(data) flatMap {
        case Mime.Gif => gifLoader.loadCached() map (_.map(Right(_)))
        case _ => bitmapLoader.loadCached() map (_.map(Left(_)))
      }
      case None => CancellableFuture.successful(None)
    }

    override def load(): CancellableFuture[Data] = imageLoader.loadRawImageData(im, img.convId) flatMap {
      case Some(data) => detectMime(data) flatMap {
        case Mime.Gif => gifLoader.load() map (Right(_))
        case _ => bitmapLoader.load() map (Left(_))
      }
      case None => CancellableFuture.failed(new Exception(s"No data could be downloaded for $im"))
    }

    override def process(result: Data) = result match {
      case Right(gif) => gifLoader.process(gif)
      case Left(bitmap) => bitmapLoader.process(bitmap)
    }
  }

  class BitmapLoader(img: ImageAssetData, im: ImageData) extends ImageLoader {
    override type Data = Bitmap
    override def loadCached() = imageLoader.hasCachedBitmap(img, im, req) flatMap {
      case true => imageLoader.loadCachedBitmap(img, im, req).map(Some(_))
      case false => CancellableFuture.successful(None)
    }
    override def load() = imageLoader.loadBitmap(img, im, req)
    override def process(result: Bitmap) = {

      def generateResult: CancellableFuture[Bitmap] = {
        if (result == bitmap.EmptyBitmap) CancellableFuture.successful(result)
        else req match {
          case Round(width, borderWidth, borderColor) =>
            withCache(s"#round $width $borderWidth $borderColor", width) {
              Threading.ImageDispatcher { BitmapUtils.createRoundBitmap(result, width, borderWidth, borderColor) }
            }
          case Regular(width, _) if isPreview(im) =>
            withCache(s"#polka $width", width) {
              BitmapPolka(imageLoader.context, result, width)
            }
          case _ =>
            CancellableFuture.successful(result)
        }
      }

      def withCache(tagSuffix: String, width: Int)(loader: => CancellableFuture[Bitmap]) = {
        val tag = im.tag + tagSuffix
        imageCache.reserve(img.id, tag, width * width * 2)
        imageCache(img.id, tag, loader)
      }

      def isPreview(image: ImageData) = {
        def shouldReportFullImage = image.tag == ImageData.Tag.Medium // XXX: this is a workaround for iOS bug, some images have wrong origWidth meta-data
        image.width < req.width && image.width != image.origWidth  && !shouldReportFullImage
      }

      generateResult map {
        case bitmap.EmptyBitmap => signal publish BitmapResult.Empty
        case bmp => signal publish BitmapLoaded(bmp, isPreview(im))
      }
    }
  }

  class GifLoader(img: ImageAssetData, im: ImageData) extends ImageLoader {
    override type Data = Gif
    override def loadCached() = imageLoader.hasCachedData(img, im) flatMap {
      case true => imageLoader.loadCachedGif(img, im).map(Some(_))
      case false => CancellableFuture.successful(None)
    }
    override def load() = imageLoader.loadGif(img, im)
    override def process(gif: Gif) = {
      if (gif.frames.length <= 1) {
        val loader = new BitmapLoader(img, im)
        loader.load() flatMap loader.process
      } else {
        var etag = 0 // to make sure signal does not cache dispatched result
        def reserveFrameMemory() = imageCache.reserve(img.id, im.tag, gif.width, gif.height * 2)
        def frameLoaded(frame: Bitmap) = signal publish BitmapLoaded(frame, preview = false, { etag += 1; etag })
        new GifAnimator(imageLoader.context, gif, reserveFrameMemory, frameLoaded).run()
      }
    }
  }
}

object BitmapSignal {

  private[images] val signalCache = new WeakMemCache[(ImageAssetData, BitmapRequest), Signal[BitmapResult]]

  def apply(img: ImageAssetData, req: BitmapRequest, service: ImageLoader, imageCache: MemoryImageCache): Signal[BitmapResult] = {
    if (img.versions.isEmpty) Signal(BitmapResult.Empty)
    else signalCache((img, req), new BitmapSignal(img, req, service, imageCache))
  }
}
