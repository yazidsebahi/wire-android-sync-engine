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
import android.net.Uri
import com.waz.ZLog._
import com.waz.cache.{CacheEntry, CacheService, LocalData}
import com.waz.content.AssetsStorage
import com.waz.model.AssetMetaData.Empty
import com.waz.model._
import com.waz.threading.{CancellableFuture, Threading}

import scala.concurrent.Future
import scala.concurrent.duration._

class MetaDataService(context: Context, cache: CacheService, storage: AssetsStorage, assets: => AssetService) {
  import com.waz.threading.Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[MetaDataService]

  def getAssetWithMetadata(id: AssetId): CancellableFuture[Option[AssetData]] =
    getAssetMetadata(id) flatMap { _ => CancellableFuture lift storage.get(id) }

  def getAssetMetadata(id: AssetId): CancellableFuture[Option[AssetMetaData]] =
    CancellableFuture lift storage.get(id) flatMap {
      case Some(AssetData.HasMetaData(metaData)) => CancellableFuture successful Some(metaData)
      case Some(a: AssetData) =>
        for {
          meta <- metaData(a)
          updated <- CancellableFuture lift storage.updateAsset(id, _.copy(metaData = meta))
        } yield updated.flatMap(_.metaData).orElse(meta)
      case _ =>
        CancellableFuture successful None
    }

  private def metaData(asset: AssetData): CancellableFuture[Option[AssetMetaData]] =
    assets.assetDataOrSource(asset) flatMap {
      case Some(Left(entry)) => loadMetaData(asset.mime, entry)
      case Some(Right(uri)) => loadMetaData(asset.mime, uri)
      case None => CancellableFuture successful None
    }

  def loadMetaData(mime: Mime, data: LocalData): CancellableFuture[Option[AssetMetaData]] = {

    def load(entry: CacheEntry) = {
      mime match {
        case Mime.Video() => AssetMetaData.Video(entry.cacheFile).map { _.fold({msg => error(msg); None}, Some(_)) }
        case Mime.Audio() => AssetMetaData.Audio(entry.cacheFile)
        case Mime.Image() => Future { AssetMetaData.Image(entry.cacheFile) } (Threading.IO)
        case _ => Future successful Some(Empty)
      }
    }

    data match {
      case entry: CacheEntry if entry.data.encKey.isEmpty => CancellableFuture lift load(entry)
      case _ =>
        warn("loading metadata from stream (encrypted cache, or generic local data) this is slow, please avoid that")
        for {
          entry <- CancellableFuture lift cache.addStream(AssetId(), data.inputStream, cacheLocation = Some(cache.intCacheDir))(10.minutes)
          res <- CancellableFuture lift load(entry)
        } yield {
          entry.delete()
          res
        }
    }
  }

  def loadMetaData(mime: Mime, uri: Uri): CancellableFuture[Option[AssetMetaData]] = CancellableFuture lift {
    mime match {
      case Mime.Video() => AssetMetaData.Video(context, uri).map(_.fold({msg => error(msg); None}, Some(_)))
      case Mime.Audio() => AssetMetaData.Audio(context, uri)
      case Mime.Image() => Future { AssetMetaData.Image(context, uri) } (Threading.BlockingIO)
      case _ => Future successful Some(Empty)
    }
  }
}
