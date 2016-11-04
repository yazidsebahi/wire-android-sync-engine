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

import java.util.Date

import com.waz.ZLog._
import com.waz.api.EphemeralExpiration
import com.waz.api.impl.ErrorResponse.internalError
import com.waz.cache.{CacheService, LocalData}
import com.waz.model._
import com.waz.service.PreferenceService
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.images.ImageLoader
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.znet.ZNetClient._
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.Future.{failed, successful}
import scala.util.control.NoStackTrace

class AssetSyncHandler(cache: CacheService, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, client: AssetClient,
                       assets: AssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler, prefs: PreferenceService) {

  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[AssetSyncHandler]

  def postAsset(assetId: AssetId, conv: Option[ConversationData]): ErrorOr[Option[Instant]] = {
    assets.storage.get(assetId) flatMap {
      case Some(asset) =>
        if (asset.assetKey.isDefined) { //TODO check this
          warn(s"asset has already been uploaded, skipping: $asset")
          successful(Right(None))
        }


        successful(Right(None))
      case None => failed(new SyncException(s"postAsset($assetId) - no asset data found"))
    }
  }



  def postOtrImageData(convId: RConvId, assetId: AssetId, exp: EphemeralExpiration, im: ImageData, recipients: Option[Set[UserId]]): ErrorOr[Option[Date]] =
    if (im.sent && im.otrKey.isDefined) {
      warn(s"image asset has already been sent, skipping: $im")
      successful(Right(None))
    } else {
      convs.convByRemoteId(convId) flatMap {
        case Some(conv) => withRawImageData(convId, im) { data =>
          otrSync.postOtrImageData(conv, assetId, im, data, exp, im.tag == ImageData.Tag.Medium, recipients = recipients).mapRight(Some(_))
        }
        case None => successful(Left(internalError(s"postOtrImageData($convId, $im) - no conversation found with given id")))
      }
    }

  def postSelfImageAsset(convId: RConvId, id: AssetId): Future[SyncResult] =
    withImageAsset(convId, id) { asset =>
      Future.traverse(asset.versions) { im =>
        if (im.sent) {
          warn(s"image asset has already been sent, skipping: $im")
          successful(SyncResult.Success)
        } else withRawImageData(convId, im)(data => postImageData(convId, asset.id, im, data, im.tag == ImageData.Tag.Medium))
      } map { results =>
        results.collectFirst { case error: SyncResult.Failure => error }.getOrElse(SyncResult.Success)
      }
    }

  // plain text asset upload should only be used for self image
  private[handler] def postImageData(convId: RConvId, assetId: AssetId, asset: ImageData, data: LocalData, nativePush: Boolean = true): Future[SyncResult] = {
    verbose(s"postImageData($convId, $assetId, $asset, $data)")

    // save just posted data under new cacheKey, so we don't need to download it when image is accessed
    def updateImageCache(sent: ImageData) =
    if (sent.data64.isDefined) successful(())
    else cache.addStream(sent.cacheKey, data.inputStream)

    client.postImageAssetData(asset, assetId, convId, data, nativePush).future flatMap {
      case Right(image) =>
        verbose(s"postImageAssetData returned event: $image with local data: $data")
        assert(image.sent, "sent flag should be set to true by event parser")
        for {
          _ <- updateImageCache(image)
          _ <- assets.updateImageAsset(assetId, convId, image)
        } yield SyncResult.Success
      case Left(error) =>
        successful(SyncResult(error))
    }
  }

}

class SyncException(msg: String) extends Exception(msg) with NoStackTrace
