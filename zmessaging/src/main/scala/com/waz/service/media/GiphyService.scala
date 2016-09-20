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

import com.waz.model.{AssetId, ImageAssetData, ImageData, RConvId}
import com.waz.service.images.ImageAssetGenerator
import com.waz.sync.client.GiphyClient
import com.waz.threading.{CancellableFuture, Threading}

class GiphyService(client: GiphyClient) {
  import Threading.Implicits.Background
  
  def getRandomGiphyImage: CancellableFuture[Seq[ImageAssetData]] = {
    client.loadRandom().map {
      case Nil => Nil
      case images =>
        val sorted = images.sorted
        val medium = sorted.last
        val preview = sorted.head
        Seq(ImageAssetData(AssetId(), RConvId(), Seq(preview.copy(origWidth = medium.width, origHeight = medium.height), medium)))
    }
  }

  def searchGiphyImage(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[ImageAssetData]] = {
    client.search(keyword, offset, limit).map {
      case Nil => Nil
      case images => images flatMap imageAssetData
    }
  }

  def trending(offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[ImageAssetData]] = {
    client.loadTrending(offset, limit).map {
      case Nil => Nil
      case images => images flatMap imageAssetData
    }
  }

  private def imageAssetData(is: Seq[ImageData]) = {
    val previews = is.filter(_.tag == ImageData.Tag.Preview)
    val gifs = is.filter(i => i.tag != ImageData.Tag.Preview && i.size <= ImageAssetGenerator.MaxGifSize)
    if (gifs.isEmpty || previews.isEmpty) Nil
    else {
      val medium = gifs.maxBy(_.size)
      val preview = previews.minBy(_.width).copy(origWidth = medium.width, origHeight = medium.height)
      val still = previews.find(_.width == medium.width).map(_.copy(tag = ImageData.Tag.MediumPreview, origWidth = medium.width, origHeight = medium.height))
      Seq(ImageAssetData(AssetId(), RConvId(), Seq(Some(preview), still, Some(medium)).flatten))
    }
  }
}
