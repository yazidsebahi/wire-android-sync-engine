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
import com.waz.bitmap.BitmapUtils
import com.waz.bitmap.gif.{Gif, GifAnimator}
import com.waz.cache.LocalData
import com.waz.model.{AssetData, AssetId, Mime}
import com.waz.service.assets.AssetService.BitmapRequest._
import com.waz.service.assets.AssetService.BitmapResult.{BitmapLoaded, LoadingFailed}
import com.waz.service.assets.AssetService.{BitmapRequest, BitmapResult}
import com.waz.threading.CancellableFuture.CancelException
import com.waz.threading.Threading.Implicits.Background
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache
import com.waz.utils.events.Signal
import com.waz.utils.{IoUtils, WeakMemCache}

// TODO: restart on network changes if it previously failed
abstract class BitmapSignal(req: BitmapRequest) extends Signal[BitmapResult] {
  signal =>

  import BitmapSignal._

  private var future = CancellableFuture.successful[Unit](())

  override protected def onWire(): Unit = {
    if (req.width > 0) {
      // ignore requests with invalid size
      future = load(previewLoader(req), fullLoader(req))
    } else {
      warn(s"invalid bitmap request, width <= 0: $req")
    }
  }

  protected def previewLoader(req: BitmapRequest): Loader
  protected def fullLoader(req: BitmapRequest): Loader

  override protected def onUnwire(): Unit = future.cancel()

  /*
   * Tries loading and processing of full cached image first
   * If loading (od processing) from cache fails does following:
   * - starts preview loading / processing
   * - start full image loading
   * - cancels preview when full image is loaded
   * - starts full image processing
   */
  private def load(preview: Loader, full: Loader) = {
    val future = full.loadCached() flatMap {
      case Some(data) =>
        full.process(data, signal)
      case None => // will try with download
        val pf = preview.load() flatMap {preview.process(_, signal)}
        val ff = full.load()
        ff.onSuccess { case _ => pf.cancel() }
        ff flatMap {full.process(_, signal)}
    }
    future onFailure {
      case _: CancelException => // ignore
      case ex =>
        warn("bitmap loading failed", ex)
        signal publish LoadingFailed(ex)
    }
    future
  }
}

object BitmapSignal {
  private implicit val tag: LogTag = logTagFor[BitmapSignal]

  private[images] val signalCache = new WeakMemCache[Any, Signal[BitmapResult]]

  def apply(asset: AssetData, req: BitmapRequest, service: ImageLoader, imageCache: MemoryImageCache): Signal[BitmapResult] = {
    if (asset.isImage) Signal(BitmapResult.Empty)
    else signalCache((asset, req), new AssetBitmapSignal(asset, req, service, imageCache))
  }

  sealed trait Loader {
    type Data
    def loadCached(): CancellableFuture[Option[Data]]
    def load(): CancellableFuture[Data]
    def process(result: Data, signal: BitmapSignal): CancellableFuture[Unit]
  }

  object EmptyLoader extends Loader {
    override type Data = Unit
    override def loadCached() = CancellableFuture.successful(None)
    override def load() = CancellableFuture.successful(())
    override def process(result: Unit, signal: BitmapSignal) = CancellableFuture.successful(())
  }

  class MimeCheckLoader(asset: AssetData, req: BitmapRequest, imageLoader: ImageLoader, imageCache: MemoryImageCache) extends Loader {
    override type Data = Either[Bitmap, Gif]

    lazy val gifLoader    = new GifLoader(asset, req, imageLoader, imageCache)
    lazy val bitmapLoader = new AssetBitmapLoader(asset, req, imageLoader, imageCache)

    def detectMime(data: LocalData) = Threading.IO {IoUtils.withResource(data.inputStream)(in => Mime(BitmapUtils.detectImageType(in)))}

    override def loadCached() = imageLoader.loadRawCachedData(asset).flatMap {
      case Some(data) => detectMime(data) flatMap {
        case Mime.Image.Gif => gifLoader.loadCached() map (_.map(Right(_)))
        case _ => bitmapLoader.loadCached() map (_.map(Left(_)))
      }
      case None => CancellableFuture.successful(None)
    }.recover {
      case e: Throwable => None
    }

    override def load(): CancellableFuture[Data] = imageLoader.loadRawImageData(asset, asset.convId) flatMap {
      case Some(data) => detectMime(data) flatMap {
        case Mime.Image.Gif => gifLoader.load() map (Right(_))
        case _ => bitmapLoader.load() map (Left(_))
      }
      case None => CancellableFuture.failed(new Exception(s"No data could be downloaded for $asset"))
    }

    override def process(result: Data, signal: BitmapSignal) = result match {
      case Right(gif) => gifLoader.process(gif, signal)
      case Left(bitmap) => bitmapLoader.process(bitmap, signal)
    }
  }

  abstract class BitmapLoader(req: BitmapRequest, imageLoader: ImageLoader, imageCache: MemoryImageCache) extends Loader {
    override type Data = Bitmap

    def id: AssetId
    def tag: String

    override def process(result: Bitmap, signal: BitmapSignal) = {

      def generateResult: CancellableFuture[Bitmap] = {
        if (result == bitmap.EmptyBitmap) CancellableFuture.successful(result)
        else req match {
          case Round(width, borderWidth, borderColor) =>
            withCache(s"#round $width $borderWidth $borderColor", width) {
              Threading.ImageDispatcher {BitmapUtils.createRoundBitmap(result, width, borderWidth, borderColor)}
            }
          case _ =>
            CancellableFuture.successful(result)
        }
      }

      def withCache(tagSuffix: String, width: Int)(loader: => CancellableFuture[Bitmap]) = {
        val t = tag + tagSuffix
        imageCache.reserve(id, t, width * width * 2)
        imageCache(id, t, loader)
      }

      generateResult map {
        case bitmap.EmptyBitmap => signal publish BitmapResult.Empty
        case bmp => signal publish BitmapLoaded(bmp, preview = false)
      }
    }
  }

  class AssetBitmapLoader(asset: AssetData, req: BitmapRequest, imageLoader: ImageLoader, imageCache: MemoryImageCache) extends BitmapLoader(req, imageLoader, imageCache) {

    override def id = asset.id
    override def tag = asset.tag

    override def loadCached() = imageLoader.hasCachedBitmap(asset, req).flatMap {
      case true => imageLoader.loadCachedBitmap(asset, req).map(Some(_))
      case false => CancellableFuture.successful(None)
    }.recover {
      case e: Throwable => None
    }

    override def load() = imageLoader.loadBitmap(asset, req)
  }

  class GifLoader(asset: AssetData, req: BitmapRequest, imageLoader: ImageLoader, imageCache: MemoryImageCache) extends Loader {
    override type Data = Gif

    override def loadCached() = imageLoader.hasCachedData(asset).flatMap {
      case true => imageLoader.loadCachedGif(asset).map(Some(_))
      case false => CancellableFuture.successful(None)
    }.recover {
      case e: Throwable => None
    }
    override def load() = imageLoader.loadGif(asset)
    override def process(gif: Gif, signal: BitmapSignal) = {
      if (gif.frames.length <= 1) {
        val loader = new AssetBitmapLoader(asset, req, imageLoader, imageCache)
        loader.load() flatMap {loader.process(_, signal)}
      } else {
        var etag = 0 // to make sure signal does not cache dispatched result
        def reserveFrameMemory() = imageCache.reserve(asset.id, asset.tag, gif.width, gif.height * 2)
        def frameLoaded(frame: Bitmap) = signal publish BitmapLoaded(frame, preview = false, {etag += 1; etag})
        new GifAnimator(imageLoader.context, gif, reserveFrameMemory, frameLoaded).run()
      }
    }
  }

}


// TODO: we could listen for AssetData changes and restart this signal,
// this isn't really needed currently since ImageAsset will be updated and UI will restart this loading
// but this could be useful in future, if UI forgets to reload or we could stop requiring them to do so
class AssetBitmapSignal(asset: AssetData, req: BitmapRequest, imageLoader: ImageLoader, cache: MemoryImageCache) extends BitmapSignal(req) {
  signal =>

  import BitmapSignal._

  require(asset.isImage, s"Passed non-image data to bitmap signal: $asset")

  override protected def previewLoader(req: BitmapRequest): Loader =
    req match {
      case Single(_, _) => EmptyLoader
      case _ => new AssetBitmapLoader(asset, req, imageLoader, cache)
    }

  override protected def fullLoader(req: BitmapRequest): Loader =
    req match {
      case Single(_, _) | Round(_, _, _) | Static(_, _) => new AssetBitmapLoader(asset, req, imageLoader, cache)
      case _ =>
        asset.mime match {
          case Mime.Image.Unknown => new MimeCheckLoader(asset, req, imageLoader, cache)
          case Mime.Image.Gif => new GifLoader(asset, req, imageLoader, cache)
          case _ => new AssetBitmapLoader(asset, req, imageLoader, cache)
        }
    }
}

