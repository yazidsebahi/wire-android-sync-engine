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
import com.waz.service.media.RichMediaContentParser.GoogleMapsLocation
import com.waz.sync.client.GoogleMapsClient

object GoogleMapsMediaService {
  private implicit val logTag: LogTag = logTagFor[YouTubeMediaService]

  val MaxImageWidth = 640 // free maps api limitation
  val ImageDimensions = Dim2(MaxImageWidth, MaxImageWidth * 3 / 4)
  val PreviewWidth = 64

  def mapImageAsset(id: AssetId, loc: com.waz.api.MessageContent.Location, dimensions: Dim2 = ImageDimensions): ImageAssetData =
    mapImageAsset(id, GoogleMapsLocation(loc.getLatitude.toString, loc.getLongitude.toString, loc.getZoom.toString), dimensions)

  def mapImageAsset(id: AssetId, location: GoogleMapsLocation, dimensions: Dim2): ImageAssetData = {

    val mapWidth = math.min(MaxImageWidth, dimensions.width)
    val mapHeight = mapWidth * dimensions.height / dimensions.width
    val previewWidth = PreviewWidth
    val previewHeight = previewWidth * mapHeight / mapWidth

    val Seq(previewPath, mediumPath) = Seq((previewWidth, previewHeight), (mapWidth, mapHeight)) map { case (w, h) =>
      GoogleMapsClient.getStaticMapPath(location, w, h)
    }

    val preview = ImageData(ImageData.Tag.Preview, Mime.Png, previewWidth, previewHeight, mapWidth, mapHeight, sent = true, proxyPath = Some(previewPath))
    val medium  = ImageData(ImageData.Tag.Medium,  Mime.Png, mapWidth,     mapHeight,     mapWidth, mapHeight, sent = true, proxyPath = Some(mediumPath))

    ImageAssetData(AssetId(), RConvId(), Seq(preview, medium))
  }
}
