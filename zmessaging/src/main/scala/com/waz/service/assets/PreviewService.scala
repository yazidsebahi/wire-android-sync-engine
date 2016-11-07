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
package com.waz.service.assets

import android.content.Context
import android.graphics.Bitmap
import android.media.MediaMetadataRetriever
import android.media.MediaMetadataRetriever._
import android.net.Uri
import com.waz.bitmap.BitmapUtils
import com.waz.cache.{CacheEntry, CacheService, LocalData}
import com.waz.content.AssetsStorage
import com.waz.model.{AssetData, AssetId, Mime}
import com.waz.service.images.ImageAssetGenerator
import com.waz.service.images.ImageAssetGenerator._
import com.waz.service.images.ImageLoader.Metadata
import com.waz.threading.CancellableFuture
import com.waz.utils.Serialized

import scala.util.Try

class PreviewService(context: Context, cache: CacheService, storage: AssetsStorage, assets: => AssetService, generator: ImageAssetGenerator) {
  import com.waz.threading.Threading.Implicits.Background

  //Be sure to update the previewId on any instances of AssetData this id references
  def getAssetPreview(id: AssetId): CancellableFuture[Option[AssetData]] =
  Serialized(('PreviewService, id))(CancellableFuture lift storage.get(id) flatMap {
    case Some(asset@AssetData.WithPreview(pId)) => CancellableFuture.lift(storage.get(pId)).flatMap {
      case Some(preview) => CancellableFuture.successful(Some(preview))
      case None => preview(asset)
    }
    case Some(asset) => CancellableFuture.lift(
        for {
          pData <- preview(asset)
          updated <- storage.updateAsset(asset.id, _.copy(previewId = pData.map(_.id)))
        } yield pData)
    case None => CancellableFuture.failed(new Exception(s"No asset data found for id: $id"))
  })

  private def preview(a: AssetData): CancellableFuture[Option[AssetData]] =
    assets.assetDataOrSource(a) flatMap {
      case Some(Left(entry)) => loadPreview(a.id, a.mime, entry)
      case Some(Right(uri)) => loadPreview(a.id, a.mime, uri)
      case _ => CancellableFuture successful None
    }

  def loadPreview(id: AssetId, mime: Mime, data: LocalData): CancellableFuture[Option[AssetData]] = (mime, data) match {
    case (Mime.Video(), entry: CacheEntry) if entry.data.encKey.isEmpty =>
      CancellableFuture lift MetaDataRetriever(entry.cacheFile)(loadPreview) flatMap { createVideoPreview(id, _) }
    case _ => CancellableFuture successful None
  }

  def loadPreview(id: AssetId, mime: Mime, uri: Uri): CancellableFuture[Option[AssetData]] = mime match {
    case Mime.Video() =>
      CancellableFuture lift MetaDataRetriever(context, uri)(loadPreview) flatMap { createVideoPreview(id, _) }
    case _ => CancellableFuture successful None
  }

  private def loadPreview(retriever: MediaMetadataRetriever) = Try(Option(retriever.getFrameAtTime(-1L, OPTION_CLOSEST_SYNC))).toOption.flatten

  private def createVideoPreview(id: AssetId, bitmap: Option[Bitmap]) = bitmap match {
    case None => CancellableFuture successful None
    case Some(b) => generator.generateAssetData(id, Right(b), Metadata(b.getWidth, b.getHeight, BitmapUtils.Mime.Jpg), MediumOptions).map(Some(_))
  }
}
