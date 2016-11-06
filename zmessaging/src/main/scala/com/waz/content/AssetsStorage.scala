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
package com.waz.content

import android.content.Context
import com.waz.model.AssetData.AssetDataDao
import com.waz.model._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.EventStream
import com.waz.utils.{CachedStorage, TrimmingLruCache, _}

import scala.collection.mutable
import scala.concurrent.Future

class AssetsStorage(context: Context, storage: Database) extends CachedStorage[AssetId, AssetData](new TrimmingLruCache(context, Fixed(100)), storage)(AssetDataDao, "AssetsStorage") {
  private implicit val dispatcher = new SerialDispatchQueue(name = "AssetsStorage")

  private val remoteMap = new mutable.HashMap[RAssetId, AssetId]()
  private val assetsById = new mutable.HashMap[AssetId, AssetData]()

  private def addIds(assets: Iterable[AssetData]) = {
    for {
      asset <- assets
      rId <- asset.remoteId
    } {
      assetsById.put(asset.id, asset)
      remoteMap.put(rId, asset.id)
    }
    assetsById
  }

  private val init = find[AssetData, Vector[AssetData]](_ => true, db => AssetDataDao.iterating(AssetDataDao.listCursor(db)), identity) map addIds

  onAdded.on(dispatcher)(as => addIds(as))

  onUpdated.on(dispatcher) {
    _.foreach { case (prev, cur) =>
      assetsById.put(cur.id, cur)
      //The only update case should be when a locally created asset is uploaded successfully, so the previous rId was empty
      if (prev.remoteId.isEmpty) {
        cur.remoteId.foreach(remoteMap.put(_, cur.id))
      }
    }
  }

  def getByRemoteId(remoteId: RAssetId): Future[Option[AssetData]] = init map (assetsById => remoteMap.get(remoteId).flatMap(assetsById.get))

  val onUploadFailed = EventStream[AssetData]()

  def updateAsset(id: AssetId, updater: AssetData => AssetData): Future[Option[AssetData]] = update(id, updater).mapOpt {
    case (_, updated) => Some(updated)
  }
}
