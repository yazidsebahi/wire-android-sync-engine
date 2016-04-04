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
import com.waz.{PermissionsService, bitmap}
import com.waz.bitmap.{BitmapDecoder, BitmapUtils}
import com.waz.bitmap.gif.{Gif, GifReader}
import com.waz.cache.{CacheEntry, CacheService, LocalData}
import com.waz.model.otr.Sha256
import com.waz.model.{AssetId, ImageAssetData, ImageData, RConvId}
import com.waz.service.assets.DownloadKey.{Proxied, OtrWireAsset, External, WireAsset}
import com.waz.service.assets.DownloaderService
import com.waz.service.images.ImageAssetService.BitmapRequest
import com.waz.service.images.ImageLoader.Metadata
import com.waz.service.otr.OtrService
import com.waz.sync.client.ImageAssetClient
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache
import com.waz.utils.IoUtils._
import com.waz.utils.{IoUtils, Serialized, returning}

import scala.concurrent.Future

class ImageLoader(val context: Context, downloader: DownloaderService, cache: CacheService, val imageCache: MemoryImageCache,
    client: ImageAssetClient, bitmapLoader: BitmapDecoder, permissions: PermissionsService) {

  import Threading.Implicits.Background
  protected def tag = "User"
  private implicit val logTag: LogTag = s"${logTagFor[ImageLoader]}[$tag]"

  def hasCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Boolean] = {
    val inMemory = imageCache.get(asset.id, im.tag).exists(_.getWidth >= (im.width min req.width))
    if (inMemory) CancellableFuture.successful(true)
    else hasCachedData(asset, im)
  }

  def hasCachedData(asset: ImageAssetData, im: ImageData): CancellableFuture[Boolean] =
    CancellableFuture { (im.data, im.uri) } flatMap {
      case (Some(data), _) if data.nonEmpty => CancellableFuture.successful(true)
      case (_, Some(uri)) if isLocalUri(uri) => CancellableFuture.successful(true)
      case _ => cache.getEntry(im.cacheKey).map(_.isDefined)
    }

  def loadCachedBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] =
    withMemoryCache(asset.id, im.tag, if (im.width == 0) req.width else req.width min im.width) {
      loadLocalAndDecode(im)(decodeBitmap(asset.id, im, req, _)) map {
        case Some(bmp) => bmp
        case None => throw new Exception(s"No local data for: $im")
      }
    }

  def loadBitmap(asset: ImageAssetData, im: ImageData, req: BitmapRequest): CancellableFuture[Bitmap] =
    Serialized(("loadBitmap", asset.id, im.tag)) {
      // serialized to avoid cache conflicts, we don't want two same requests running at the same time
      withMemoryCache(asset.id, im.tag, req.width) {
        downloadAndDecode(asset, im)(decodeBitmap(asset.id, im, req, _))
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

  def saveImageToGallery(image: ImageAssetData): Future[Option[Uri]] = {
    def addNewFile(writer: File => Unit) = {
      val newFile = ImageAssetService.saveImageFile(image)
      writer(newFile)
      val uri = Uri.fromFile(newFile)
      context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, uri))
      Some(uri)
    }

    if (image.versions.isEmpty) Future.successful(None)
    else loadRawImageData(image.versions.last, image.convId).future flatMap {
      case Some(data) =>
        permissions.requiring(Set(Permission.WRITE_EXTERNAL_STORAGE), false)(
          {
            warn("permission to save image to gallery denied")
            Future.successful(None)
          },
          Future.successful(addNewFile { newFile =>
            IoUtils.copy(data.inputStream, new FileOutputStream(newFile))
          }))
      case None =>
        error(s"No image data found for: $image")
        Future.successful(None)
    }
  }

  private def downloadAndDecode[A](asset: ImageAssetData, im: ImageData)(decode: LocalData => CancellableFuture[A]) = {

    // retry download, maybe local data is corrupt
    def retryDownload(entry: CacheEntry) = {
      verbose(s"Decoding failed, will clear cache and retry download for: $im, cached result: $entry")
      entry.delete()
      downloadImageData(im, asset.convId) flatMap {
        case Some(d) => decode(d)
        case None => CancellableFuture.failed(new Exception(s"No data downloaded for: $im"))
      }
    }

    loadLocalData(im) flatMap {
      case None => downloadImageData(im, asset.convId)
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

  private def openStream(uri: Uri) = {
    val cr = context.getContentResolver
    Option(cr.openInputStream(uri))
      .orElse(Option(cr.openFileDescriptor(uri, "r")).map(file => new FileInputStream(file.getFileDescriptor)))
      .getOrElse(throw new FileNotFoundException(s"Can not load image from: $uri"))
  }

  private def loadLocalAndDecode[A](im: ImageData)(decode: LocalData => CancellableFuture[A]): CancellableFuture[Option[A]] =
    loadLocalData(im) flatMap {
      case Some(data) =>
        decode(data)
          .map(Some(_))
          .recover {
            case e: Throwable =>
              warn(s"loadLocalAndDecode($im), decode failed, will delete local data", e)
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
      case (_, Some(uri)) if isLocalUri(uri) => CancellableFuture.successful(Some(LocalData(openStream(uri), -1)))
      case _ => cache.getEntry(img.cacheKey)
    }
  }

  private def downloadImageData(img: ImageData, convId: RConvId): CancellableFuture[Option[LocalData]] = {
    verbose(s"downloadImageData($img, $convId)")
    def isContentUri(uri: Uri) = uri.getScheme == ContentResolver.SCHEME_CONTENT

    def decrypt(in: InputStream, out: OutputStream): Unit =
      OtrService.decryptSymmetric(img.otrKey.get, in, out)

    Serialized(("downloadImageData", img.cacheKey)) {
      ((img.remoteId, img.uri, img.proxyPath) match {
        case (_, Some(uri), _) if isContentUri(uri) => CancellableFuture.successful(Some(LocalData(openStream(uri), -1)))
        case (Some(id), _, _) if img.otrKey.isDefined => downloader.download(OtrWireAsset(id, convId))(client)
        case (Some(id), _, _) => downloader.download(WireAsset(id, convId))(client)
        case (None, Some(uri), _) => downloader.download(External(uri))(client)
        case (None, None, Some(path)) => downloader.download(Proxied(path))(client)
        case (None, None, None) => CancellableFuture.failed(new Exception(s"Invalid ImageData: $img"))
      }) flatMap {
        case Some(entry) if img.otrKey.isDefined =>
          verbose(s"downloaded image $img, need to decrypt it")
          if (img.sha256.forall(_ == Sha256(IoUtils.sha256(entry.inputStream))))
            CancellableFuture.lift(cache.processWithCaching(s"dec${img.cacheKey}_${img.otrKey.get}", decrypt, entry)) map { Some(_) }
          else
            CancellableFuture.failed(new Exception(s"SHA256 doesn't match for encrypted image: $img"))
        case res =>
          CancellableFuture.successful(res)
      } flatMap {
        case Some(entry) =>
          verbose(s"downloaded image $img, got entry: $entry, data: $entry")
          CancellableFuture.lift(cache.move(img.cacheKey, entry).map(Some(_)))
        case None => CancellableFuture.successful(None)
      }
    }
  }

  private def decodeGif(data: LocalData) = Threading.ImageDispatcher {
    data.byteArray.fold(GifReader(data.inputStream))(GifReader(_)).get
  }

  private def decodeBitmap(assetId: AssetId, im: ImageData, req: BitmapRequest, data: LocalData): CancellableFuture[Bitmap] = {

    def computeInSampleSize(srcWidth: Int, srcHeight: Int): Int = {
      val pixelCount = srcWidth * srcHeight
      val minSize = if (pixelCount <= ImageAssetGenerator.MaxImagePixelCount) req.width else (req.width * math.sqrt(ImageAssetGenerator.MaxImagePixelCount / pixelCount)).toInt
      BitmapUtils.computeInSampleSize(minSize, srcWidth)
    }

    for {
      meta <- getImageMetadata(im, data, req.mirror)
      inSample = computeInSampleSize(meta.width, meta.height)
      _ = verbose(s"image meta: $meta, inSampleSize: $inSample")
      _ = imageCache.reserve(assetId, im.tag, meta.width / inSample, meta.height / inSample)
      bmp <- bitmapLoader(() => data.inputStream, inSample, meta.orientation)
      _ = if (bmp == bitmap.EmptyBitmap) throw new Exception("Bitmap decoding failed, got empty bitmap")
    } yield bmp
  }

  def getImageMetadata(im: ImageData, data: LocalData, mirror: Boolean = false) =
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
