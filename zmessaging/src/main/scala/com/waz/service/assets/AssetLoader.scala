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

import java.io._

import android.content.Context
import android.net.Uri
import com.waz.ZLog._
import com.waz.cache.{CacheService, LocalData}
import com.waz.content.Mime
import com.waz.service.downloads.DownloadRequest._
import com.waz.service.downloads.{Downloader, DownloaderService}
import com.waz.threading.CancellableFuture

class AssetLoader(val context: Context, downloader: DownloaderService, assetDownloader: Downloader[AssetRequest],
    streamDownloader: Downloader[AssetFromInputStream], videoDownloader: Downloader[VideoAsset],
    unencodedAudioDownloader: Downloader[UnencodedAudioAsset], cache: CacheService) {

  import AssetLoader._
  import com.waz.threading.Threading.Implicits.Background

  def getAssetData(asset: AssetRequest): CancellableFuture[Option[LocalData]] =
    CancellableFuture.lift(cache.getEntry(asset.cacheKey)) flatMap {
      case Some(entry) => CancellableFuture successful Some(entry)
      case None        => downloadAssetData(asset)
    }

  def downloadAssetData(asset: AssetRequest): CancellableFuture[Option[LocalData]] =
    asset match {
      case CachedAssetRequest(_, _, _) => CancellableFuture successful None
      case LocalAssetRequest(cacheKey, uri, mime @ Mime.Video(), name) =>
        downloader.download(VideoAsset(cacheKey, uri, mime, name))(videoDownloader)
      case LocalAssetRequest(cacheKey, uri, mime @ Mime.Audio.PCM, name) =>
        downloader.download(UnencodedAudioAsset(cacheKey, uri, name))(unencodedAudioDownloader)
      case LocalAssetRequest(cacheKey, uri, mime, name) =>
        downloader.download(AssetFromInputStream(cacheKey, () => AssetLoader.openStream(context, uri), mime, name))(streamDownloader)
      case External(cacheKey, uri) if Option(uri.getScheme).forall(! _.startsWith("http")) =>
        downloader.download(AssetFromInputStream(cacheKey, () => AssetLoader.openStream(context, uri), Mime.Unknown, None))(streamDownloader)
      case _ =>
        downloader.download(asset)(assetDownloader)
    }

  def openStream(uri: Uri) = AssetLoader.openStream(context, uri)
}

object AssetLoader {
  private implicit val Tag: LogTag = logTagFor[AssetLoader]

  def openStream(context: Context, uri: Uri) = {
    val cr = context.getContentResolver
    Option(cr.openInputStream(uri))
      .orElse(Option(cr.openFileDescriptor(uri, "r")).map(file => new FileInputStream(file.getFileDescriptor)))
      .getOrElse(throw new FileNotFoundException(s"Can not load image from: $uri"))
  }
}
