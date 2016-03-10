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
package com.waz.service.media

import com.waz.ZLog._
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.model._
import com.waz.service.images.ImageAssetService
import com.waz.sync.client.GoogleMapsClient
import com.waz.threading.Threading
import com.waz.znet.ZNetClient.ErrorOr

import scala.concurrent.Future

class GoogleMapsMediaService(assets: ImageAssetService) {
  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[YouTubeMediaService]

  val (mapWidth, mapHeight) = (800, 600)
  val (previewWidth, previewHeight) = (80, 60)

  def updateMedia(msg: MessageData, content: MessageContent): ErrorOr[MessageContent] = {
    RichMediaContentParser.googleMapsLocation(content.content) match {
      case Some(location) =>
        val Seq(previewPath, mediumPath) = Seq((previewWidth, previewHeight), (mapWidth, mapHeight)) map { case (w, h) =>
          GoogleMapsClient.getStaticMapPath(location, w, h)
        }

        val preview = ImageData(ImageData.Tag.Preview, Mime.Png, previewWidth, previewHeight, mapWidth, mapHeight, sent = true, proxyPath = Some(previewPath))
        val medium  = ImageData(ImageData.Tag.Medium,  Mime.Png, mapWidth,     mapHeight,     mapWidth, mapHeight, sent = true, proxyPath = Some(mediumPath))

        assets.updateImageAsset(ImageAssetData(AssetId(), RConvId(), Seq(preview, medium))) map (_.id) map { assetId =>
          Right(content.copy(asset = Some(assetId), width = mapWidth, height = mapHeight))
        }

      case None =>
        warn(s"no valid google maps location found in message: $content")
        Future.successful(Right(content))
    }
  }
}
