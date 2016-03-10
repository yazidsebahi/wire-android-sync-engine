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
import java.util.concurrent.atomic.AtomicReference

import android.graphics.Bitmap
import android.os.Environment
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.content._
import com.waz.model._

import scala.concurrent.Future

class ImageAssetService(storage: AssetsStorage, generator: ImageAssetGenerator) {
  private implicit val logTag: LogTag = logTagFor[ImageAssetService]
  import com.waz.threading.Threading.Implicits.Background

  def getImageAsset(id: AssetId) = storage.get(id) map {
    case Some(image: ImageAssetData) => Some(image)
    case _ => None
  }

  def updateImageAsset(asset: ImageAssetData): Future[AssetData] = storage.insert(asset)

  def updateImageAsset(id: AssetId, convId: RConvId, image: ImageData): Future[AssetData] = {
    storage.updateOrCreate(id, {
      case im @ ImageAssetData(`id`, _, _) => im.updated(image).copy(convId = convId)
      case data =>
        HockeyApp.saveException(new Exception(s"Unexpected asset data in updateImageAsset()"), s"data: $data, asset: $id, conv: $convId")
        data
    }, ImageAssetData(id, convId, Seq(image)))
  }

  def updateImageAssets(data: Seq[ImageAssetData]) =
    storage.updateOrCreateAll(data.map(d => d.id -> { (_: Option[AssetData]) => d })(collection.breakOut))

  def addImageAsset(assetId: AssetId, image: com.waz.api.ImageAsset, convId: RConvId, isSelf: Boolean): Future[ImageAssetData] = {
    image match {
      case im: com.waz.api.impl.ImageAsset if im.data.versions.nonEmpty =>
        val ref = new AtomicReference(image) // keep a strong reference until asset generation completes
        generator.generateWireAsset(assetId, im.data.versions.last, convId, isSelf).future.flatMap { data =>
          updateImageAssets(Seq(data)) map (_ => data)
        } andThen { case _ => ref set null }
      case _ =>
        Future.failed(new IllegalArgumentException(s"Unsupported ImageAsset: $image"))
    }
  }
}

object ImageAssetService {
  val SaveImageDirName = "Wire"

  lazy val SaveImageDir = {
    val path = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES) + File.separator + SaveImageDirName
    val dir = new File(path)
    dir.mkdirs()
    dir
  }

  def sanitizeFileName(name: String) = name.replace(' ', '_').replaceAll("[^\\w]", "")

  def saveImageFile(image: ImageAssetData) = new File(SaveImageDir,  s"wire_${System.currentTimeMillis}.${image.versions.last.fileExtension}")

  sealed trait BitmapResult
  object BitmapResult {
    case object Empty extends BitmapResult
    case class BitmapLoaded(bitmap: Bitmap, preview: Boolean, etag: Int = 0) extends BitmapResult {
      override def toString: LogTag = s"BitmapLoaded([${bitmap.getWidth}, ${bitmap.getHeight}], $preview, $etag)"
    }
    case class LoadingFailed(ex: Throwable) extends BitmapResult
  }

  sealed trait BitmapRequest {
    val width: Int
    val mirror: Boolean = false
  }
  object BitmapRequest {
    case class Regular(width: Int, override val mirror: Boolean = false) extends BitmapRequest
    case class Static(width: Int, override val mirror: Boolean = false) extends BitmapRequest
    case class Single(width: Int) extends BitmapRequest
    case class Round(width: Int, borderWidth: Int = 0, borderColor: Int = 0) extends BitmapRequest
  }
}
