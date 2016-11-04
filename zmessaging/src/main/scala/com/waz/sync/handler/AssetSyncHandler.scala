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
import com.waz.cache.{CacheService, LocalData}
import com.waz.model.AssetStatus.UploadDone
import com.waz.model.GenericContent.Asset
import com.waz.model._
import com.waz.service.PreferenceService
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
                       assets: AssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler, prefs: PreferenceService) {

  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[AssetSyncHandler]

  def postAsset(assetId: AssetId, nativePush: Boolean = false, recipients: Option[Set[UserId]] = None): ErrorOr[Option[AssetData]] = {
    assets.storage.get(assetId) flatMap {
      case Some(asset) =>
        if (asset.assetKey.isDefined) { //TODO check this
          warn(s"asset has already been uploaded, skipping: $asset")
          successful(Right(None))
        }
        cache.getEntry(assetId).flatMap {
          case Some(cacheData) =>
            val key = AESKey()
            asset.convId match {
              case Some(id) if prefs.sendWithV3 => convs.convByRemoteId(id) flatMap {
                //TODO remove v2 sending after testing
                case Some(convData) =>
                  def proto(sha: Sha256): GenericMessage = GenericMessage(Uid(), Asset(asset)) // = GenericMessage(Uid(assetId.str), exp, Proto.ImageAsset(im.tag, im.width, im.height, im.origWidth, im.origHeight, im.mime, im.size, Some(key), Some(sha)))
                  otrSync.postAssetDataV2(convData, key, proto, cacheData, nativePush, recipients) map {
                    case Right((ak, date)) => Right(Some(asset.copy(status = UploadDone(ak))))
                    case Left(err) => Left(err)
                  }
              }
              case _ =>
                otrSync.uploadAssetDataV3(cacheData, Some(key)) map {
                  case Right(ak) => Right(Some(asset.copy(status = UploadDone(ak))))
                  case Left(err) => Left(err)
                }
            }
          case None =>
            error(s"No cache entry found for asset: $asset")
            CancellableFuture successful Right(None)
        }
      case None => failed(new SyncException(s"postAsset($assetId) - no asset data found"))
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
