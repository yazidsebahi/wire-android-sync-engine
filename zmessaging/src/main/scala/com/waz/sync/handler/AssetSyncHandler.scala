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
import com.waz.api.impl.ErrorResponse.internalError
import com.waz.cache.{CacheService, LocalData}
import com.waz.model._
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.images.ImageLoader
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.concurrent.Future.{failed, successful}
import scala.util.control.NoStackTrace

class AssetSyncHandler(cache: CacheService, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, client: AssetClient,
                       assets: AssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler) {

  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[AssetSyncHandler]

  private[handler] def withImageAsset[A](id: AssetId)(f: ImageAssetData => Future[A]): Future[A] =
    assets.storage.getImageAsset(id) flatMap {
      case Some(asset) => f(asset)
      case None => failed(new SyncException(s"withImageAsset($id) - no image asset data found"))
    }

  private[handler] def withImageAsset[A](convId: RConvId, id: AssetId)(f: ImageAssetData => Future[A]): Future[A] =
    withImageAsset(id) { asset =>
      if (asset.convId == convId) f(asset)
      else assets.updateImageAsset(asset.copy(convId = convId)) flatMap { a => f(a.asInstanceOf[ImageAssetData]) }
    }

  private[handler] def withRawImageData[A](convId: RConvId, asset: ImageData)(f: LocalData => Future[A]): Future[A] =
    imageLoader.loadRawImageData(asset, convId).future flatMap {
      case Some(data) => f(data)
      case None => failed(new SyncException(s"No local data found for image asset: $asset"))
    }

  private[handler] def withRawAssetData[A](convId: RConvId, asset: ImageData)(f: LocalData => Future[A]): Future[A] =
    imageLoader.loadRawImageData(asset, convId).future flatMap {
      case Some(data) => f(data)
      case None => failed(new SyncException(s"No local data found for image asset: $asset"))
    }

  private[handler] def withImageAssetVersion[A](id: AssetId, tag: String)(f: (ImageAssetData, ImageData) => Future[A]): Future[A] =
    withImageAsset(id) { asset =>
      asset.versions.find(_.tag == tag).fold {
        failed[A](new SyncException(s"postImageAssetVersion($id, $tag) - no image data found with given tag: $asset"))
      } { image =>
        f(asset, image)
      }
    }

  def postOtrImageData(convId: RConvId, assetId: AssetId, asset: ImageData): ErrorOr[Option[Date]] =
    if (asset.sent && asset.otrKey.isDefined) {
      warn(s"image asset has already been sent, skipping: $asset")
      successful(Right(None))
    } else {
      convs.convByRemoteId(convId) flatMap {
        case Some(conv) => withRawImageData(convId, asset)(data => otrSync.postOtrImageData(conv, assetId, asset, data, asset.tag == ImageData.Tag.Medium).mapRight(Some(_)))
        case None       => successful(Left(internalError(s"postOtrImageData($convId, $asset) - no conversation found with given id")))
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
        results.collectFirst { case error: SyncResult.Failure => error } .getOrElse(SyncResult.Success)
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
