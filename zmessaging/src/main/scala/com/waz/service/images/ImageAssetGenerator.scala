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

import java.io.{File, FileInputStream, FileOutputStream}

import android.content.Context
import android.graphics.Bitmap
import android.util.Base64
import com.waz.ZLog._
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.bitmap.{BitmapDecoder, BitmapUtils}
import com.waz.cache.{CacheService, LocalData}
import com.waz.model._
import com.waz.service.images.ImageLoader.Metadata
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.MemoryImageCache
import com.waz.utils.IoUtils

class ImageAssetGenerator(context: Context, cache: CacheService, loader: ImageLoader, imageCache: MemoryImageCache, bitmapLoader: BitmapDecoder) {
  import com.waz.service.images.ImageAssetGenerator._
  implicit private val dispatcher = Threading.ImageDispatcher
  implicit private val tag: LogTag = "ImageAssetGenerator"

  lazy val saveDir = new File(context.getFilesDir, "assets")

  // generate wire asset from local ImageData
  def generateWireAsset(assetId: AssetId, input: ImageData, convId: RConvId, profilePicture: Boolean): CancellableFuture[ImageAssetData] =
    loader.loadRawImageData(input, convId) flatMap {
      case Some(data) =>
        loader.getImageMetadata(input, data) flatMap { meta =>
          generateAssets(assetId, data, meta, convId, if (profilePicture) SelfOptions else ImageOptions)
        }
      case _ =>
        CancellableFuture.failed(new IllegalArgumentException(s"ImageAsset could not be added to cache: $input"))
    }

  private def generateAssets(assetId: AssetId, input: LocalData, meta: Metadata, convId: RConvId, options: Array[CompressionOptions]): CancellableFuture[ImageAssetData] = {
    def generateAssetData(co: CompressionOptions, origWidth: Int, origHeight: Int): CancellableFuture[ImageData] = {
      val remoteId = RImageDataId()
      generateImageData(assetId, remoteId, co, input, meta) flatMap {
        case (file, m) =>
          verbose(s"generated image, size: ${input.length}, meta: $m")
          if (shouldRecode(file, m, co)) recode(assetId, file, co, m)
          else CancellableFuture.successful((file, m))
      } map {
        case (file, m) =>
          val size = file.length().toInt
          verbose(s"final image, size: $size, meta: $m")
          val data = if (size > 2 * 1024) None else Some(Base64.encodeToString(IoUtils.toByteArray(new FileInputStream(file)), Base64.NO_WRAP | Base64.NO_PADDING))
          data foreach { data => cache.remove(ImageData.cacheKey(Some(remoteId))) } // no need to cache preview images
          ImageData(co.tag, m.mimeType, m.width, m.height, math.max(m.width, origWidth), math.max(m.height, origHeight), size, Some(remoteId), data)
      }
    }

    val result = Seq.newBuilder[ImageData]
    val dummy = ImageData("", "", 0, 0, 0, 0, 0, Some(RImageDataId()))
    options.foldRight(CancellableFuture.successful(dummy)) { (co, f) =>
      f.flatMap { prev =>
        generateAssetData(co, prev.origWidth, prev.origHeight) map { data =>
          result += data
          data
        }
      }
    } map { _ =>
      ImageAssetData(assetId, convId, result.result().sorted)
    }
  }

  private def generateImageData(id: AssetId, remoteId: RImageDataId, options: CompressionOptions, input: LocalData, meta: Metadata) = {

    def loadScaled(w: Int, h: Int, crop: Boolean) = {
      val minWidth = if (crop) math.max(w, w * meta.width / meta.height) else w
      val sampleSize = BitmapUtils.computeInSampleSize(minWidth, meta.width)
      val memoryNeeded = (w * h) + (meta.width / sampleSize * meta.height / sampleSize) * 4
      imageCache.reserve(id, options.tag, memoryNeeded)
      bitmapLoader(() => input.inputStream, sampleSize, meta.orientation) map { image =>
        if (crop) {
          verbose(s"cropping to $w")
          BitmapUtils.cropRect(image, w)
        } else if (image.getWidth > w) {
          verbose(s"scaling to $w, $h")
          BitmapUtils.scale(image, w, h)
        } else image
      }
    }

    def generateScaled(): CancellableFuture[(File, Metadata)] = {
      val (w, h) = options.calculateScaledSize(meta.width, meta.height)
      verbose(s"calculated scaled size: ($w, $h) for $meta and $options")
      loadScaled(w, h, options.cropToSquare) map { image =>
        verbose(s"loaded scaled: (${image.getWidth}, ${image.getHeight})")
        imageCache.add(id, options.tag, image)
        saveImage(remoteId, image, meta.mimeType, options)
      }
    }

    if (options.shouldScaleOriginalSize(meta.width, meta.height)) generateScaled()
    else cache.addStream(ImageData.cacheKey(Some(remoteId)), input.inputStream, cacheLocation = Some(saveDir)) map { entry => (entry.cacheFile, meta) }
  }

  private def saveFormat(mime: String, forceLossy: Boolean) =
    if (!forceLossy && mime == Mime.Png) Bitmap.CompressFormat.PNG
    else Bitmap.CompressFormat.JPEG

  private def saveImage(id: RImageDataId, image: Bitmap, mime: String, options: CompressionOptions): (File, Metadata) =
    saveImage(cache.createForFile(ImageData.cacheKey(Some(id)), cacheLocation = Some(saveDir)).cacheFile, image, mime, options)

  private def saveImage(file: File, image: Bitmap, mime: String, options: CompressionOptions): (File, Metadata) = {
    val format = saveFormat(mime, options.forceLossy)
    IoUtils.withResource(new FileOutputStream(file)) { os => image.compress(format, options.quality, os) }
    (file, Metadata(image.getWidth, image.getHeight, if (format == Bitmap.CompressFormat.PNG) Mime.Png else Mime.Jpg))
  }

  private[images] def shouldRecode(file: File, meta: Metadata, opts: CompressionOptions) = {
    val size = file.length()
    opts.recodeMimes(meta.mimeType) ||
      meta.mimeType != Mime.Gif && size > opts.byteCount ||
      size > MaxGifSize ||
      meta.isRotated
  }

  private def recode(id: AssetId, file: File, options: CompressionOptions, meta: Metadata) = {
    verbose(s"recode asset $id with opts: $options")

    def load = {
      imageCache.reserve(id, options.tag, meta.width, if (meta.isRotated) 2 * meta.height else meta.height)
      bitmapLoader(file, 1, meta.orientation)
    }

    imageCache(id, options.tag, load) map { bitmap => saveImage(file, bitmap, Mime.Jpg, options)}
  }
}

object ImageAssetGenerator {
  val SmallProfileSize = 280

  val PreviewCompressionQuality = 30
  val JpegCompressionQuality = 75
  val SmallProfileCompressionQuality = 70

  val MaxImagePixelCount = 1.3 * 1448 * 1448
  val MaxGifSize = 5 * 1024 * 1024

  val PreviewRecodeMimes = CompressionOptions.DefaultRecodeMimes + Mime.Gif

  val PreviewOptions = CompressionOptions(1024, 64, PreviewCompressionQuality, forceLossy = true, cropToSquare = false, ImageData.Tag.Preview, PreviewRecodeMimes)
  val MediumOptions = CompressionOptions(310 * 1024, 1448, JpegCompressionQuality, forceLossy = false, cropToSquare = false, ImageData.Tag.Medium)

  val SmallProfileOptions = new CompressionOptions(15 * 1024, 280, SmallProfileCompressionQuality, forceLossy = true, cropToSquare = true, ImageData.Tag.SmallProfile, PreviewRecodeMimes)
  val MediumProfileOptions = MediumOptions.copy(recodeMimes = PreviewRecodeMimes)

  val ImageOptions = Array(PreviewOptions, MediumOptions)
  val SelfOptions = Array(SmallProfileOptions, MediumProfileOptions)
}

case class CompressionOptions(byteCount: Int, dimension: Int, quality: Int, forceLossy: Boolean, cropToSquare: Boolean, tag: String, recodeMimes: Set[String] = CompressionOptions.DefaultRecodeMimes) {

  val maxPixelCount = 1.3 * dimension * dimension

  def shouldScaleOriginalSize(width: Int, height: Int): Boolean =
    width * height > maxPixelCount || (cropToSquare && width != height)


  def calculateScaledSize(origWidth: Int, origHeight: Int): (Int, Int) = {
    if (origWidth < 1 || origHeight < 1) (1, 1)
    else if (cropToSquare) {
      val size = math.min(dimension, math.min(origWidth, origHeight))
      (size, size)
    } else {
      val scale = math.sqrt((dimension * dimension).toDouble / (origWidth * origHeight))
      val width = math.ceil(scale * origWidth).toInt
      (width, (width.toDouble / origWidth * origHeight).round.toInt)
    }
  }

  def getOutputFormat(mime: String) =
    if (! forceLossy && mime == Mime.Png) Bitmap.CompressFormat.PNG
    else Bitmap.CompressFormat.JPEG
}

object CompressionOptions {

  // set of mime types that should be recoded to Jpeg before uploading
  val DefaultRecodeMimes = Set(Mime.WebP, Mime.Unknown, Mime.Tiff, Mime.Bmp)
}
