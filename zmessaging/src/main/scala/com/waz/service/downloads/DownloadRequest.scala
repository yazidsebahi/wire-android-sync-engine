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
package com.waz.service.downloads

import java.io.InputStream

import android.net.Uri
import com.waz.model.{Mime, _}
import com.waz.znet.Request

sealed trait DownloadRequest {
  val assetId: AssetId
}

object DownloadRequest {

  sealed trait AssetRequest extends DownloadRequest {
    val mime: Mime
    val name: Option[String]
  }

  case class CachedAssetRequest(assetId: AssetId, mime: Mime, name: Option[String]) extends AssetRequest

  case class LocalAssetRequest(assetId: AssetId, uri: Uri, mime: Mime, name: Option[String]) extends AssetRequest

  sealed trait ExternalAssetRequest extends AssetRequest {
    def request: Request[Unit]

    override val mime: Mime = Mime.Unknown
    override val name: Option[String] = None
  }

  case class WireAssetRequest(assetId: AssetId, key: AssetKey, mime: Mime, name: Option[String] = None) extends AssetRequest

  case class WireV2AssetRequest(assetId: AssetId, key: AssetKey, mime: Mime, convId: RConvId, name: Option[String] = None) extends AssetRequest

  case class AssetFromInputStream(assetId: AssetId, stream: () => InputStream, mime: Mime = Mime.Unknown, name: Option[String] = None) extends DownloadRequest

  case class VideoAsset(assetId: AssetId, uri: Uri, mime: Mime = Mime.Unknown, name: Option[String] = None) extends DownloadRequest

  case class UnencodedAudioAsset(assetId: AssetId, uri: Uri, name: Option[String]) extends DownloadRequest

  case class External(assetId: AssetId, uri: Uri) extends ExternalAssetRequest {
    override def request: Request[Unit] = Request[Unit](absoluteUri = Some(uri), requiresAuthentication = false)
  }

  // external asset downloaded from wire proxy, path is relative to our proxy endpoint
  case class Proxied(assetId: AssetId, path: String) extends ExternalAssetRequest {
    override def request: Request[Unit] = Request.Get(path)
  }
}
