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
import com.waz.content.{AssetsStorage, Mime}
import com.waz.model.{AnyAssetData, AssetId, AssetPreviewData}
import com.waz.service.images.ImageAssetGenerator
import com.waz.service.images.ImageAssetGenerator._
import com.waz.service.images.ImageLoader.Metadata
import com.waz.threading.{CancellableFuture, Threading}

import scala.util.Try

class PreviewService(context: Context, cache: CacheService, storage: AssetsStorage, assets: => AssetService, generator: ImageAssetGenerator) {
  import com.waz.threading.Threading.Implicits.Background

  def getAssetWithPreview(id: AssetId): CancellableFuture[Option[AnyAssetData]] =
    getAssetPreview(id) flatMap { _ => CancellableFuture lift storage.getAsset(id) }

  def getAssetPreview(id: AssetId): CancellableFuture[Option[AssetPreviewData]] =
    CancellableFuture lift storage.get(id) flatMap {
      case Some(AnyAssetData(_, _, _, _, _, _, Some(preview), _, _, _)) => CancellableFuture successful Some(preview)
      case Some(a: AnyAssetData) =>
        for {
          p       <- preview(a)
          updated <- CancellableFuture lift storage.updateAsset(id, { asset => asset.copy(preview = asset.preview.orElse(p)) })
        } yield
          updated.flatMap(_.preview).orElse(p)
      case _ =>
        CancellableFuture successful None
    }

  private def preview(a: AnyAssetData): CancellableFuture[Option[AssetPreviewData]] =
    assets.assetDataOrSource(a) flatMap {
      case Some(Left(entry)) => loadPreview(a.id, a.mimeType, entry)
      case Some(Right(uri)) => loadPreview(a.id, a.mimeType, uri)
      case _ => CancellableFuture successful None
    }

  def loadPreview(id: AssetId, mime: Mime, data: LocalData): CancellableFuture[Option[AssetPreviewData]] = (mime, data) match {
    case (Mime.Video(), entry: CacheEntry) if entry.data.encKey.isEmpty =>
      CancellableFuture { MetaDataRetriever(entry.cacheFile)(loadPreview) } (Threading.IO) flatMap { createVideoPreview(id, _) }
    case _ =>
      CancellableFuture successful Some(AssetPreviewData.Empty)
  }

  def loadPreview(id: AssetId, mime: Mime, uri: Uri): CancellableFuture[Option[AssetPreviewData]] = mime match {
    case Mime.Video() =>
      CancellableFuture { MetaDataRetriever(context, uri)(loadPreview) } (Threading.BlockingIO) flatMap { createVideoPreview(id, _) }
    case _ =>
      CancellableFuture successful Some(AssetPreviewData.Empty)
  }

  private def loadPreview(retriever: MediaMetadataRetriever) = Try(Option(retriever.getFrameAtTime(-1L, OPTION_CLOSEST_SYNC))).toOption.flatten

  private def createVideoPreview(id: AssetId, bitmap: Option[Bitmap]) = bitmap match {
    case None => CancellableFuture successful None
    case Some(b) =>
      generator.generateAssetData(id, Right(b), Metadata(b.getWidth, b.getHeight, BitmapUtils.Mime.Jpg), MediumOptions, b.getWidth, b.getHeight) map { img => Some(AssetPreviewData.Image(img)) }
  }
}
