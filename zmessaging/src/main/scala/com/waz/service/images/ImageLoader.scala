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

import java.io._

import android.content.{ContentResolver, Context, Intent}
import android.graphics.{Bitmap, BitmapFactory}
import android.media.ExifInterface
import android.media.ExifInterface._
import android.net.Uri
import com.waz.ZLog._
import com.waz.api.Permission
import com.waz.bitmap.gif.{Gif, GifReader}
import com.waz.bitmap.{BitmapDecoder, BitmapUtils}
import com.waz.cache.{CacheEntry, CacheService, LocalData}
import com.waz.model.AssetData.IsImage
import com.waz.model.{Mime, _}
import com.waz.service.assets.AssetService.BitmapRequest
import com.waz.service.assets.{AssetLoader, AssetService}
import com.waz.service.images.ImageLoader.Metadata
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache
import com.waz.utils.IoUtils._
import com.waz.utils.{IoUtils, Serialized, returning}
import com.waz.{PermissionsService, bitmap}

import scala.concurrent.Future

class ImageLoader(val context: Context, cache: CacheService, val imageCache: MemoryImageCache,
                  bitmapLoader: BitmapDecoder, permissions: PermissionsService, assetLoader: AssetLoader) {

  import Threading.Implicits.Background

  protected def tag = "User"
  private implicit val logTag: LogTag = s"${logTagFor[ImageLoader]}[$tag]"

  def hasCachedBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Boolean] = {
    def inMemory(dims: Dim2, tag: String) = imageCache.get(asset.id, tag).exists { bmp => bmp.getWidth >= req.width || (dims.width > 0 && bmp.getWidth > dims.width) }
    asset match {
      case IsImage(dims, tag) => CancellableFuture successful inMemory(dims, tag)
      case _ => CancellableFuture successful false
    }
  }

  def hasCachedData(asset: AssetData): CancellableFuture[Boolean] =
    asset match {
      case IsImage(_, _) =>
        CancellableFuture {(asset.data, asset.source)} flatMap {
          case (Some(data), _) if data.nonEmpty => CancellableFuture.successful(true)
          case (_, Some(uri)) if isLocalUri(uri) => CancellableFuture.successful(true)
          case _ => CancellableFuture lift cache.getEntry(asset.id).map(_.isDefined)
        }
      case _ => CancellableFuture successful false
    }

  def loadCachedBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Bitmap] = ifIsImage(asset) { (dims, tag) =>
    verbose(s"load cached bitmap for: ${asset.id}")
    withMemoryCache(asset.id, tag, if (dims.width == 0) req.width else req.width min dims.width) {
      loadLocalAndDecode(asset)(decodeBitmap(asset.id, tag, req, _)) map {
        case Some(bmp) => bmp
        case None => throw new Exception(s"No local data for: $asset")
      }
    }
  }

  def loadBitmap(asset: AssetData, req: BitmapRequest): CancellableFuture[Bitmap] = ifIsImage(asset) { (dims, tag) =>
    Serialized(("loadBitmap", asset.id)) {
      // serialized to avoid cache conflicts, we don't want two same requests running at the same time
      withMemoryCache(asset.id, tag, if (dims.width == 0) req.width else req.width min dims.width) {
        downloadAndDecode(asset)(decodeBitmap(asset.id, tag, req, _))
      }
    }
  }

  def loadCachedGif(asset: AssetData): CancellableFuture[Gif] = ifIsImage(asset) { (_, _) =>
    loadLocalAndDecode(asset)(decodeGif) map {
      case Some(gif) => gif
      case None => throw new Exception(s"No local data for: $asset")
    }
  }

  def loadGif(asset: AssetData): CancellableFuture[Gif] = ifIsImage(asset) { (_, tag) =>
    Serialized(("loadBitmap", asset.id, tag)) {
      downloadAndDecode(asset)(decodeGif)
    }
  }

  def loadRawCachedData(asset: AssetData) = ifIsImage(asset)((_, _) => loadLocalData(asset))

  def loadRawImageData(asset: AssetData) = ifIsImage(asset) { (_, _) =>
    verbose(s"loadRawImageData: assetId: $asset")
    loadLocalData(asset) flatMap {
      case None => downloadImageData(asset)
      case Some(data) => CancellableFuture.successful(Some(data))
    }
  }

  def saveImageToGallery(asset: AssetData): Future[Option[Uri]] = ifIsImage(asset) { (_, _) =>
    loadRawImageData(asset).future flatMap {
      case Some(data) =>
        saveImageToGallery(data, asset.mime)
      case None =>
        error(s"No image data found for: $asset")
        Future.successful(None)
    }
  }

  private def saveImageToGallery(data: LocalData, mime: Mime) =
    permissions.requiring(Set(Permission.WRITE_EXTERNAL_STORAGE), delayUntilProviderIsSet = false)(
    {
      warn("permission to save image to gallery denied")
      Future successful None
    },
    Future {
      val newFile = AssetService.saveImageFile(mime)
      IoUtils.copy(data.inputStream, new FileOutputStream(newFile))
      val uri = Uri.fromFile(newFile)
      context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, uri))
      Some(uri)
    }(Threading.IO)
    )

  private def downloadAndDecode[A](asset: AssetData)(decode: LocalData => CancellableFuture[A]): CancellableFuture[A] =
    downloadAndDecode[A](loadLocalData(asset), downloadImageData(asset), decode, asset)

  private def downloadAndDecode[A](load: => CancellableFuture[Option[LocalData]], download: => CancellableFuture[Option[LocalData]], decode: LocalData => CancellableFuture[A], im: Any): CancellableFuture[A] = {

    // retry download, maybe local data is corrupt
    def retryDownload(entry: CacheEntry) = {
      verbose(s"Decoding failed, will clear cache and retry download for: $im, cached result: $entry")
      entry.delete()
      download flatMap {
        case Some(d) => decode(d)
        case None => CancellableFuture.failed(new Exception(s"No data downloaded for: $im"))
      }
    }

    load flatMap {
      case None => download
      case Some(data) => CancellableFuture.successful(Some(data))
    } flatMap {
      case Some(data) =>
        // retry if decoding from cache entry fails
        decode(data).recoverWith {
          case e: Throwable =>
            warn(s"decoding failed for $im", e)
            data match {
              case entry: CacheEntry => retryDownload(entry)
              case _ => CancellableFuture.failed(e)
            }
        }
      case None => CancellableFuture.failed(new Exception(s"No data downloaded for: $im"))
    }
  }

  private def ifIsImage[A](asset: AssetData)(f: (Dim2, String) => A) = asset match {
    case IsImage(dims, tag) => f(dims, tag)
    case _ => throw new Exception(s"Asset is not an image: $asset")
  }

  private def isLocalUri(uri: Uri) = uri.getScheme match {
    case ContentResolver.SCHEME_FILE | ContentResolver.SCHEME_ANDROID_RESOURCE => true
    case _ => false
  }

  private def loadLocalAndDecode[A](asset: AssetData)(decode: LocalData => CancellableFuture[A]): CancellableFuture[Option[A]] =
    loadLocalAndDecode(loadLocalData(asset))(decode)

  private def loadLocalAndDecode[A](load: CancellableFuture[Option[LocalData]])(decode: LocalData => CancellableFuture[A]): CancellableFuture[Option[A]] =
    load flatMap {
      case Some(data) =>
        decode(data)
          .map(Some(_))
          .recover {
            case e: Throwable =>
              warn(s"loadLocalAndDecode(), decode failed, will delete local data", e)
              data.delete()
              None
          }
      case None =>
        CancellableFuture successful None
    }

  private def loadLocalData(asset: AssetData): CancellableFuture[Option[LocalData]] = {
    verbose(s"loadLocalData: ${asset.id}, url: ${asset.source}")

    // wrapped in future to ensure that img.data is accessed from background thread, this is needed for local image assets (especially the one generated from bitmap), see: Images
    CancellableFuture {(asset.data, asset.source)} flatMap {
      case (Some(data), _) if data.nonEmpty =>
        verbose(s"asset: ${asset.id} contained data already")
        CancellableFuture.successful(Some(LocalData(data)))
      case (_, Some(uri)) if isLocalUri(uri) =>
        verbose(s"asset: ${asset.id} contained no data, but had a url")
        CancellableFuture.successful(Some(LocalData(assetLoader.openStream(uri), -1)))
      case _ =>
        verbose(s"asset: ${asset.id} contained no data or a url, trying cached storage")
        CancellableFuture lift cache.getEntry(asset.id)
    }
  }

  private def downloadImageData(asset: AssetData): CancellableFuture[Option[LocalData]] = {
    val req = asset.loadRequest
    verbose(s"downloadImageData($asset), req: $req")
    assetLoader.downloadAssetData(req)
  }

  private def decodeGif(data: LocalData) = Threading.ImageDispatcher {
    data.byteArray.fold(GifReader(data.inputStream))(GifReader(_)).get
  }

  private def decodeBitmap(assetId: AssetId, tag: String, req: BitmapRequest, data: LocalData): CancellableFuture[Bitmap] = {

    def computeInSampleSize(srcWidth: Int, srcHeight: Int): Int = {
      val pixelCount = srcWidth * srcHeight
      val minSize = if (pixelCount <= ImageAssetGenerator.MaxImagePixelCount) req.width else (req.width * math.sqrt(ImageAssetGenerator.MaxImagePixelCount / pixelCount)).toInt
      BitmapUtils.computeInSampleSize(minSize, srcWidth)
    }

    for {
      meta <- getImageMetadata(data, req.mirror)
      inSample = computeInSampleSize(meta.width, meta.height)
      _ = verbose(s"image meta: $meta, inSampleSize: $inSample")
      _ = imageCache.reserve(assetId, tag, meta.width / inSample, meta.height / inSample)
      bmp <- bitmapLoader(() => data.inputStream, inSample, meta.orientation)
      _ = if (bmp == bitmap.EmptyBitmap) throw new Exception("Bitmap decoding failed, got empty bitmap")
    } yield bmp
  }

  def getImageMetadata(data: LocalData, mirror: Boolean = false) =
    Threading.IO {
      val o = BitmapUtils.getOrientation(data.inputStream)
      Metadata(data).withOrientation(if (mirror) Metadata.mirrored(o) else o)
    }

  private def withMemoryCache(assetId: AssetId, tag: String, minWidth: Int)(loader: => CancellableFuture[Bitmap]): CancellableFuture[Bitmap] =
    imageCache.get(assetId, tag) match {
      case Some(image) if image.getWidth >= minWidth => // cache could contain smaller version, will need to reload in that case
        verbose(s"getBitmap(${MemoryImageCache.Key(assetId, tag)}, $minWidth) - got from cache: $image (${image.getWidth}, ${image.getHeight})")
        CancellableFuture.successful(image)
      case _ =>
        loader map (returning(_) {
          verbose(s"adding asset to memory cache: $assetId, $tag")
          imageCache.add(assetId, tag, _)
        })
    }
}

object ImageLoader {

  //TODO if orientation could be useful ever to other clients, we might want to merge with AssetMetaData.Image
  case class Metadata(width: Int, height: Int, mimeType: String, orientation: Int = ExifInterface.ORIENTATION_UNDEFINED) {
    def isRotated: Boolean = orientation != ExifInterface.ORIENTATION_NORMAL && orientation != ExifInterface.ORIENTATION_UNDEFINED

    def withOrientation(orientation: Int) = {
      if (Metadata.shouldSwapDimens(this.orientation) != Metadata.shouldSwapDimens(orientation))
        copy(width = height, height = width, orientation = orientation)
      else
        copy(orientation = orientation)
    }
  }

  object Metadata {

    def apply(data: LocalData): Metadata = {
      val opts = new BitmapFactory.Options
      opts.inJustDecodeBounds = true
      opts.inScaled = false
      withResource(data.inputStream) {BitmapFactory.decodeStream(_, null, opts)}
      Metadata(opts.outWidth, opts.outHeight, opts.outMimeType)
    }

    def shouldSwapDimens(o: Int) = o match {
      case ExifInterface.ORIENTATION_ROTATE_90 | ExifInterface.ORIENTATION_ROTATE_270 | ExifInterface.ORIENTATION_TRANSPOSE | ExifInterface.ORIENTATION_TRANSVERSE => true
      case _ => false
    }

    def mirrored(o: Int) = o match {
      case ORIENTATION_UNDEFINED | ORIENTATION_NORMAL => ORIENTATION_FLIP_HORIZONTAL
      case ORIENTATION_FLIP_HORIZONTAL => ORIENTATION_NORMAL
      case ORIENTATION_FLIP_VERTICAL => ORIENTATION_ROTATE_180
      case ORIENTATION_ROTATE_90 => ORIENTATION_TRANSPOSE
      case ORIENTATION_ROTATE_180 => ORIENTATION_FLIP_VERTICAL
      case ORIENTATION_ROTATE_270 => ORIENTATION_TRANSVERSE
      case ORIENTATION_TRANSPOSE => ORIENTATION_ROTATE_90
      case ORIENTATION_TRANSVERSE => ORIENTATION_ROTATE_270
    }
  }

}
