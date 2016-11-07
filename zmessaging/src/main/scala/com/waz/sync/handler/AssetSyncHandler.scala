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
import com.waz.cache.{CacheService, LocalData}
import com.waz.model.AssetStatus.{UploadDone, UploadInProgress}
import com.waz.model.GenericContent.Asset
import com.waz.model._
import com.waz.service.{ErrorsService, PreferenceService}
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.images.ImageLoader
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.concurrent.Future.{failed, successful}
import scala.util.control.NoStackTrace

class AssetSyncHandler(cache: CacheService, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, client: AssetClient,
                       assets: AssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler, prefs: PreferenceService, errors: ErrorsService) {

  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[AssetSyncHandler]

  def uploadAssetData(assetId: AssetId, convId: ConvId, msgId: MessageId): ErrorOrResponse[Option[AssetData]] = {
    CancellableFuture.lift(assets.storage.get(assetId).zip(assets.getAssetData(assetId))) flatMap {
      case (Some(asset), Some(data)) if data.length > AssetData.MaxAllowedAssetSizeInBytes =>
        CancellableFuture lift errors.addAssetTooLargeError(convId, msgId) map { _ => Left(internalError("asset too large")) }
      case (Some(asset), _) if asset.remoteId.isDefined =>
        warn(s"asset has already been uploaded, skipping: $asset")
        CancellableFuture.successful(Right(None))
      case (Some(asset), Some(data)) =>
        val key = AESKey()
        otrSync.uploadAssetDataV3(data, Some(key)).map {
          case Right(ak) => Right(Some(asset.copy(status = UploadDone(ak))))
          case Left(err) => Left(err)
        }.flatMap {
          case Right(Some(asset)) =>
            for {
              Some(updated) <- CancellableFuture.lift(assets.storage.updateAsset(asset.id, a => asset))
              _ <- CancellableFuture.lift(cache.addStream(updated.id, data.inputStream, updated.mime, updated.name, length = data.length))
            } yield Right(Some(updated))
          case Left(error) => CancellableFuture.successful(Left(error))
        }
      case asset =>
        debug(s"No asset data found in postAssetData, got: $asset")
        CancellableFuture successful Right(None)
    }
  }

  def postSelfImageAsset(convId: RConvId, id: AssetId): Future[SyncResult] =
    (for {
      asset <- assets.storage.get(id)
      data <- cache.getEntry(id)
    } yield (asset, data)) flatMap {
      case (Some(a), Some(d)) => postImageData(convId, a, d, nativePush = false)
      case _ => Future.successful(SyncResult.Failure(None))
    }

  // plain text asset upload should only be used for self image
  private[handler] def postImageData(convId: RConvId, asset: AssetData, data: LocalData, nativePush: Boolean = true): Future[SyncResult] = {
    verbose(s"postImageData($convId, $asset, $data)")

    // save just posted data under new cacheKey, so we don't need to download it when image is accessed
    def updateImageCache(sent: AssetData) =
    if (sent.data64.isDefined) successful(())
    else cache.addStream(sent.id, data.inputStream)

    client.postImageAssetData(asset, data, nativePush).future flatMap {
      case Right(updated) =>
        verbose(s"postImageAssetData returned event: $updated with local data: $data")
        assert(updated.assetKey.isDefined, "sent flag should be set to true by event parser")
        for {
          _ <- updateImageCache(updated)
          _ <- assets.storage.updateAsset(updated.id, _ => updated)
        } yield SyncResult.Success
      case Left(error) =>
        successful(SyncResult(error))
    }
  }

}

class SyncException(msg: String) extends Exception(msg) with NoStackTrace
