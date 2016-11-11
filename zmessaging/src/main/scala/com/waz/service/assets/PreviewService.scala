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

  def loadPreview(id: AssetId, mime: Mime, data: LocalData): CancellableFuture[Option[AssetData]] = (mime, data) match {
    case (Mime.Video(), entry: CacheEntry) if entry.data.encKey.isEmpty =>
      CancellableFuture.lift(for {
        Some(prev) <- MetaDataRetriever(entry.cacheFile)(loadPreview) flatMap { createVideoPreview(id, _) }
        updated <- storage.updateOrCreateAsset(prev)
      } yield updated)
    case _ => CancellableFuture successful None
  }

  private def loadPreview(retriever: MediaMetadataRetriever) = Try(Option(retriever.getFrameAtTime(-1L, OPTION_CLOSEST_SYNC))).toOption.flatten

  private def createVideoPreview(id: AssetId, bitmap: Option[Bitmap]) = bitmap match {
    case None => CancellableFuture successful None
    case Some(b) => generator.generateAssetData(id, Right(b), Metadata(b.getWidth, b.getHeight, BitmapUtils.Mime.Jpg), MediumOptions).map(Some(_))
  }
}
