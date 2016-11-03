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
import com.waz.model.AssetStatus.UploadDone
import com.waz.model.GenericContent.Asset
import com.waz.model.GenericContent.Asset.Original
import com.waz.model.{Mime, _}
import com.waz.service.assets.AssetService.BitmapRequest
import com.waz.service.assets.{AssetLoader, AssetService}
import com.waz.service.downloads.DownloadRequest._
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

  def hasCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Boolean] = {
    val inMemory = imageCache.get(asset.id, im.tag).exists { bmp => bmp.getWidth >= req.width || (im.width > 0 && bmp.getWidth > im.width) }
    if (inMemory) CancellableFuture.successful(true)
    else hasCachedData(asset, im)
  }

  def hasCachedData(asset: ImageAssetData, im: ImageData): CancellableFuture[Boolean] =
    CancellableFuture { (im.data, im.uri) } flatMap {
      case (Some(data), _) if data.nonEmpty => CancellableFuture.successful(true)
      case (_, Some(uri)) if isLocalUri(uri) => CancellableFuture.successful(true)
      case _ => CancellableFuture lift cache.getEntry(im.cacheKey).map(_.isDefined)
    }

  def loadCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] =
    withMemoryCache(asset.id, im.tag, if (im.width == 0) req.width else req.width min im.width) {
      loadLocalAndDecode(im)(decodeBitmap(asset.id, im.tag, req, _)) map {
        case Some(bmp) => bmp
        case None => throw new Exception(s"No local data for: $im")
      }
    }

  def loadBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] =
    Serialized(("loadBitmap", asset.id, im.tag)) {
      // serialized to avoid cache conflicts, we don't want two same requests running at the same time
      withMemoryCache(asset.id, im.tag, if (im.width == 0) req.width else req.width min im.width) {
        downloadAndDecode(asset, im)(decodeBitmap(asset.id, im.tag, req, _))
      }
    }

  private def assetImageData(asset: Asset): Option[(Mime, Dim2, AssetKey)] =
    asset match {
      case Asset(Some(Original(mime@Mime.Image(), _, _, Some(AssetMetaData.Image(d, _)), _)), _, UploadDone(key)) => Some((mime, d, key))
      case Asset(Some(Original(mime@Mime.Image(), _, _, _, _)), _, UploadDone(key)) => Some((mime, Dim2.Empty, key))
      case _ => None
    }

  def hasCachedData(asset: Asset): CancellableFuture[Boolean] =
    assetImageData(asset) match {
      case None => CancellableFuture successful false
      case Some((_, _, key)) =>
        CancellableFuture lift cache.getEntry(key.cacheKey).map(_.isDefined)
    }

  def loadCachedBitmap(asset: Asset, req: BitmapRequest): CancellableFuture[Bitmap] =
    assetImageData(asset) match {
      case None => CancellableFuture.failed(new Exception(s"Unexpected asset data: $asset"))
      case Some((mime, dim, key)) =>
        withMemoryCache(key.assetId, "", if (dim.width == 0) req.width else req.width min dim.width) {
          loadLocalAndDecode(CancellableFuture lift cache.getEntry(key.cacheKey))(decodeBitmap(key.assetId, "", req, _)) map {
            case Some(bmp) => bmp
            case None => throw new Exception(s"No local data for: $asset")
          }
        }
    }

  def loadBitmap(asset: Asset, req: BitmapRequest): CancellableFuture[Bitmap] =
    assetImageData(asset) match {
      case None => CancellableFuture.failed(new Exception(s"Unexpected asset data: $asset"))
      case Some((mime, dim, key)) =>
        // serialized to avoid cache conflicts, we don't want two same requests running at the same time
        Serialized(("loadBitmap", key.cacheKey)) {
          withMemoryCache(key.assetId, "", if (dim.width == 0) req.width else req.width min dim.width) {
            def load = CancellableFuture lift cache.getEntry(key.cacheKey)
            def download = assetLoader.downloadAssetData(ImageAssetRequest(key.cacheKey, RConvId.Empty, key, mime))
            downloadAndDecode(load, download, decodeBitmap(key.assetId, "", req, _), asset)
          }
        }
    }

  def loadCachedGif(asset: ImageAssetData, im: ImageData): CancellableFuture[Gif] =
    loadLocalAndDecode(im)(decodeGif) map {
      case Some(gif) => gif
      case None => throw new Exception(s"No local data for: $im")
    }

  def loadGif(asset: ImageAssetData, im: ImageData): CancellableFuture[Gif] =
    Serialized(("loadBitmap", asset.id, im.tag)) {
      downloadAndDecode(asset, im)(decodeGif)
    }

  def loadRawCachedData(im: ImageData, convId: RConvId) = loadLocalData(im)

  def loadRawImageData(im: ImageData, convId: RConvId) =
    loadLocalData(im) flatMap {
      case None => downloadImageData(im, convId)
      case Some(data) => CancellableFuture.successful(Some(data))
    }

  def loadRawImageData(key: AssetKey, mime: Mime) =
    CancellableFuture lift cache.getEntry(key.cacheKey) flatMap {
      case None => assetLoader.downloadAssetData(ImageAssetRequest(key.cacheKey, RConvId.Empty, key, mime))
      case Some(data) => CancellableFuture.successful(Some(data))
    }

  def saveImageToGallery(image: ImageAssetData): Future[Option[Uri]] =
    if (image.versions.isEmpty) Future.successful(None)
    else loadRawImageData(image.versions.last, image.convId).future flatMap {
      case Some(data) =>
        saveImageToGallery(data, Mime(image.versions.last.mime))
      case None =>
        error(s"No image data found for: $image")
        Future.successful(None)
    }

  def saveImageToGallery(image: Asset): Future[Option[Uri]] =
    image match {
      case Asset(Some(Original(mime, _, _, _, _)), _, UploadDone(key)) =>
        loadRawImageData(key, mime).future flatMap {
          case Some(data) => saveImageToGallery(data, mime)
          case None =>
            error(s"No image data found for: $image")
            Future successful None
        }
      case _ => Future successful None
    }

  private def saveImageToGallery(data: LocalData, mime: Mime) =
    permissions.requiring(Set(Permission.WRITE_EXTERNAL_STORAGE), delayUntilProviderIsSet = false) (
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
      } (Threading.IO)
    )

  private def downloadAndDecode[A](asset: ImageAssetData, im: ImageData)(decode: LocalData => CancellableFuture[A]): CancellableFuture[A] =
    downloadAndDecode[A](loadLocalData(im), downloadImageData(im, asset.convId), decode, im)

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

  private def isLocalUri(uri: Uri) = uri.getScheme match {
    case ContentResolver.SCHEME_FILE | ContentResolver.SCHEME_ANDROID_RESOURCE => true
    case _ => false
  }

  private def loadLocalAndDecode[A](im: ImageData)(decode: LocalData => CancellableFuture[A]): CancellableFuture[Option[A]] =
    loadLocalAndDecode(loadLocalData(im))(decode)

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

  private def loadLocalData(img: ImageData): CancellableFuture[Option[LocalData]] = {
    verbose(s"loadLocalData")

    // wrapped in future to ensure that img.data is accessed from background thread, this is needed for local image assets (especially the one generated from bitmap), see: Images
    CancellableFuture { (img.data, img.uri) } flatMap {
      case (Some(data), _) if data.nonEmpty => CancellableFuture.successful(Some(LocalData(data)))
      case (_, Some(uri)) if isLocalUri(uri) => CancellableFuture.successful(Some(LocalData(assetLoader.openStream(uri), -1)))
      case _ => CancellableFuture lift cache.getEntry(img.cacheKey)
    }
  }

  private def downloadImageData(img: ImageData, convId: RConvId): CancellableFuture[Option[LocalData]] = {
    val req = (img.remoteId, img.uri, img.proxyPath) match {
      case (Some(id), _, _)         => Some(ImageAssetRequest(img.cacheKey, convId, AssetKey(Left(id), None, img.otrKey.getOrElse(AESKey.Empty), img.sha256.getOrElse(Sha256.Empty)), Mime(img.mime)))
      case (None, Some(uri), _)     => Some(External(img.cacheKey, uri))
      case (None, None, Some(path)) => Some(Proxied(img.cacheKey, path))
      case (None, None, None)       => None
    }

    verbose(s"downloadImageData($img, $convId), req: $req")
    req.fold(CancellableFuture successful Option.empty[LocalData]) { assetLoader.downloadAssetData }
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

  private def withMemoryCache(asset: AssetId, tag: String, minWidth: Int)(loader: => CancellableFuture[Bitmap]): CancellableFuture[Bitmap] =
    imageCache.get(asset, tag) match {
      case Some(image) if image.getWidth >= minWidth => // cache could contain smaller version, will need to reload in that case
        verbose(s"getBitmap($asset, $minWidth) - got from cache: $image (${image.getWidth}, ${image.getHeight})")
        CancellableFuture.successful(image)
      case _ =>
        loader map (returning(_) {
          imageCache.add(asset, tag, _)
        })
    }
}

object ImageLoader {

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
      withResource(data.inputStream) { BitmapFactory.decodeStream(_, null, opts) }
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
