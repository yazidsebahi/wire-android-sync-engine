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
package com.waz.sync.handler

import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse._
import com.waz.cache.CacheService
import com.waz.model.AssetStatus.UploadDone
import com.waz.model._
import com.waz.service.PreferenceService
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.images.ImageLoader
import com.waz.sync.client.AssetClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.ZNetClient._

import scala.util.control.NoStackTrace

class AssetSyncHandler(cache: CacheService, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, client: AssetClient,
                       assets: AssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler, prefs: PreferenceService) {

  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[AssetSyncHandler]

  def uploadAssetData(assetId: AssetId, public: Boolean = false): ErrorOrResponse[Option[AssetData]] = {
    CancellableFuture.lift(assets.storage.get(assetId).zip(assets.getAssetData(assetId))) flatMap {
      case (Some(asset), Some(data)) if data.length > AssetData.MaxAllowedAssetSizeInBytes =>
        CancellableFuture successful Left(internalError(AssetSyncHandler.AssetTooLarge))
      case (Some(asset), _) if asset.remoteId.isDefined =>
        warn(s"asset has already been uploaded, skipping: $asset")
        CancellableFuture.successful(Right(None))
      case (Some(asset), Some(data)) =>
        otrSync.uploadAssetDataV3(data, if (public) None else Some(AESKey()), asset.mime).flatMap {
          case Right(remoteData) =>
            for {
              Some(updated) <- CancellableFuture.lift(assets.storage.updateAsset(asset.id, _.copyWithRemoteData(remoteData)))
              _ <- CancellableFuture.lift(cache.addStream(updated.cacheKey, data.inputStream, updated.mime, updated.name, length = data.length))
            } yield Right(Some(updated))
          case Left(err) => CancellableFuture successful Left(err)
        }
      case asset =>
        debug(s"No asset data found in postAssetData, got: $asset")
        CancellableFuture successful Right(None)
    }
  }

}

class SyncException(msg: String) extends Exception(msg) with NoStackTrace

object AssetSyncHandler {
  val AssetTooLarge = "Failed to upload asset: Asset is too large"
}