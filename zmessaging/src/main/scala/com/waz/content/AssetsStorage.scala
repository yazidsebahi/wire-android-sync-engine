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
import android.net.Uri
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.verbose
import com.waz.model.AssetData.AssetDataDao
import com.waz.model._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.{EventContext, EventStream}
import com.waz.utils.{CachedStorage, TrimmingLruCache, _}

import scala.collection.mutable
import scala.concurrent.Future

class AssetsStorage(context: Context, storage: Database) extends CachedStorage[AssetId, AssetData](new TrimmingLruCache(context, Fixed(100)), storage)(AssetDataDao, "AssetsStorage") {
  private implicit val dispatcher = new SerialDispatchQueue(name = "AssetsStorage")
  import EventContext.Implicits.global

  private val remoteMap = new mutable.HashMap[RAssetId, AssetId]()
  private val uriMap = new mutable.HashMap[Uri, AssetId]()
  private val assetsById = new mutable.HashMap[AssetId, AssetData]()

  private def updateLinks(assets: Iterable[AssetData]) = {
    verbose(s"updating links to assets: $assets")
    assets.foreach { asset =>
      asset.source.foreach(uriMap.put(_, asset.id))
      asset.remoteId.foreach(remoteMap.put(_, asset.id))
      assetsById.put(asset.id, asset)
    }
    verbose(s"remoteMap: $remoteMap")
    verbose(s"uriMap: $uriMap")
    assetsById
  }

  private val init = {
    verbose("initialising assets storage")
    find[AssetData, Vector[AssetData]](_ => true, db => AssetDataDao.iterating(AssetDataDao.listCursor(db)), identity) map updateLinks
  }

  onAdded.on(dispatcher)(as => updateLinks(as))

  onUpdated.on(dispatcher) {
    _.foreach { case (prev, cur) =>
      assetsById.put(cur.id, cur)
      //The only update case should be when a locally created asset is uploaded successfully, so the previous rId was empty
      if (prev.remoteId.isEmpty) {
        cur.remoteId.foreach(remoteMap.put(_, cur.id))
      }
      if (prev.source.isEmpty) {
        cur.source.foreach(uriMap.put(_, cur.id))
      }
    }
  }

  //TODO Dean - remove this: it's no longer useful since we can receive assets that don't yet have remote data
  def getByRemoteId(remoteId: RAssetId): Future[Option[AssetData]] = init map (assetsById => remoteMap.get(remoteId).flatMap(assetsById.get))

  def getByUri(uri: Uri): Future[Option[AssetData]] = init map (assetsById => uriMap.get(uri).flatMap(assetsById.get))

  val onUploadFailed = EventStream[AssetData]()

  def updateAsset(id: AssetId, updater: AssetData => AssetData): Future[Option[AssetData]] = update(id, updater).mapOpt {
    case (_, updated) => Some(updated)
  }

  def updateOrCreateAsset(asset: AssetData) = updateOrCreate(asset.id, cur => cur.merge(asset), asset).map(Some(_))
}
