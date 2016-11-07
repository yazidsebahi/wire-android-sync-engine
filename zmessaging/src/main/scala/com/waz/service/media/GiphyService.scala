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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.warn
import com.waz.model.{AssetData, AssetId}
import com.waz.service.assets.AssetService
import com.waz.service.images.ImageAssetGenerator
import com.waz.sync.client.GiphyClient
import com.waz.threading.{CancellableFuture, Threading}

import scala.concurrent.Future

class GiphyService(client: GiphyClient, assets: AssetService) {
  import Threading.Implicits.Background

  //TODO Dean - this is all a bit ugly - It might be better to re-introduce an Image AssetData container type that links previews together
  implicit lazy val ImageDataOrdering: Ordering[AssetData] = new Ordering[AssetData] {
    override def compare(x: AssetData, y: AssetData): Int = {
      if (x.dimensions.width == y.dimensions.width) {
        if (x.tag == "preview" || y.tag == "medium") -1
        else if (x.tag == "medium" || y.tag == "preview") 1
        else Ordering.Int.compare(x.size.toInt, y.size.toInt)
      } else Ordering.Int.compare(x.dimensions.width, y.dimensions.width)
    }
  }

  def getRandomGiphyImage: CancellableFuture[Seq[AssetId]] = {
    client.loadRandom().flatMap {
      case Nil => CancellableFuture.successful(Nil)
      case images =>
        val sorted = images.sorted
        val medium = sorted.last
        val preview = sorted.head
        CancellableFuture.lift(cacheGiphyResults(Seq((preview, medium))))
    }
  }

  def searchGiphyImage(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[AssetId]] = {
    client.search(keyword, offset, limit).flatMap {
      case Nil => CancellableFuture.successful(Nil)
      case images => CancellableFuture.lift(cacheGiphyResults(images.flatMap(assetData)))
    }
  }

  def trending(offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[AssetId]] = {
    client.loadTrending(offset, limit).flatMap {
      case Nil => CancellableFuture.successful(Nil)
      case images => CancellableFuture.lift(cacheGiphyResults(images.flatMap(assetData)))
    }
  }

  private def assetData(is: Seq[AssetData]): Seq[(AssetData, AssetData)] = {
    val previews = is.filter(_.tag == "preview")
    val gifs = is.filter(i => i.tag != "preview" && i.size <= ImageAssetGenerator.MaxGifSize)
    if (gifs.isEmpty || previews.isEmpty) Nil
    else {
      val medium = gifs.maxBy(_.size)
      val preview = previews.minBy(_.dimensions.width)
      //TODO Dean: reintroduce still versions?
      //      val still = previews.find(_.dimensions.width == medium.dimensions.width).map(a => a.copy(metaData = Some(AssetMetaData.Image(a.dimensions, "medium"))))
      Seq((preview, medium.copy(previewId = Some(preview.id))))
    }
  }

  private def cacheGiphyResults(results: Seq[(AssetData, AssetData)]): Future[Seq[AssetId]] = {
    Future.sequence(results.map { case (prev, medium) => cacheGiphyResult(prev, medium).collect { case Some(id) => id } })
  }

  private def cacheGiphyResult(preview: AssetData, medium: AssetData): Future[Option[AssetId]] = {
    Future.sequence(Seq(preview, medium.copy(previewId = Some(preview.id))).map { asset =>
      (asset.source match {
        case Some(uri) => assets.storage.getByUri(uri).flatMap {
          case Some(cur) => assets.storage.updateAsset(cur.id, _ => asset.copy(id = cur.id))
          case None => assets.storage.insert(asset).map(Some(_))
        }
        case None =>
          warn("Tried to save giphy result that doesn't contain a URI - ignoring")
          Future successful None
      }).collect { case Some(saved) => saved.id }
    }).map(_.lastOption)
  }

}
