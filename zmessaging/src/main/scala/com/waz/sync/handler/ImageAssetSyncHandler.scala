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
import com.waz.api.impl.ErrorResponse
import com.waz.cache.{CacheService, LocalData}
import com.waz.model._
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.images.{ImageAssetService, ImageLoader}
import com.waz.sync.SyncResult
import com.waz.sync.client.ImageAssetClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.Threading

import scala.concurrent.Future
import scala.util.control.NoStackTrace

class ImageAssetSyncHandler(cache: CacheService, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, client: ImageAssetClient, imageService: ImageAssetService, imageLoader: ImageLoader, otrSync: OtrSyncHandler) {

  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[ImageAssetSyncHandler]

  def postSelfImageAsset(convId: RConvId, id: AssetId): Future[SyncResult] = withImageAsset(convId, id) { asset =>
    Future.traverse(asset.versions) { im =>
      postImageData(asset.convId, asset.id, im)
    } map { results =>
      results.collectFirst { case error: SyncResult.Failure => error } .getOrElse(SyncResult.Success)
    }
  }

  private[handler] def withImageAsset[A](id: AssetId)(f: ImageAssetData => Future[A]): Future[A] =
    imageService.getImageAsset(id) flatMap {
      case Some(asset) => f(asset)
      case None => Future.failed(new SyncException(s"postImageAsset($id) - no image asset data found"))
    }

  private[handler] def withImageAsset[A](convId: RConvId, id: AssetId)(f: ImageAssetData => Future[A]): Future[A] =
    withImageAsset(id) { asset =>
      if (asset.convId == convId) f(asset)
      else imageService.updateImageAsset(asset.copy(convId = convId)) flatMap { a => f(a.asInstanceOf[ImageAssetData]) }
    }

  private[handler] def withRawImageData[A](convId: RConvId, asset: ImageData)(f: LocalData => Future[A]): Future[A] =
    imageLoader.loadRawImageData(asset, convId).future flatMap {
      case Some(data) => f(data)
      case None => Future.failed(new SyncException(s"No local data found for image asset: $asset"))
    }

  private[handler] def withImageAssetVersion[A](id: AssetId, tag: String)(f: (ImageAssetData, ImageData) => Future[A]): Future[A] =
    withImageAsset(id) { asset =>
      asset.versions.find(_.tag == tag).fold {
        Future.failed[A](new SyncException(s"postImageAssetVersion($id, $tag) - no image data found with given tag: $asset"))
      } { image =>
        f(asset, image)
      }
    }

  def postOtrImageData(convId: RConvId, assetId: AssetId, asset: ImageData): Future[Either[ErrorResponse, Option[Date]]] =
    if (asset.sent && asset.otrKey.isDefined) {
      warn(s"image asset has already been sent, skipping: $asset")
      Future.successful(Right(None))
    } else {
      convs.convByRemoteId(convId) flatMap {
        case Some(conv) =>
          withRawImageData(convId, asset) { data =>
            otrSync.postOtrAssetData(conv, assetId, asset, data, asset.tag == ImageData.Tag.Medium) map {
              case Right(time) => Right(Some(time))
              case Left(err) => Left(err)
            }
          }
        case None =>
          Future.successful(Left(ErrorResponse.internalError(s"postOtrImageData($convId, $asset) - no conversation found with given id")))
      }
    }

  private[handler] def postImageData(convId: RConvId, assetId: AssetId, asset: ImageData): Future[SyncResult] =
    if (asset.sent) {
      warn(s"image asset has already been sent, skipping: $asset")
      Future.successful(SyncResult.Success)
    } else {
      withRawImageData(convId, asset) { data =>
        postImageData(convId, assetId, asset, data, asset.tag == ImageData.Tag.Medium)
      }
    }

  private[handler] def postImageData(convId: RConvId, assetId: AssetId, asset: ImageData, data: LocalData, nativePush: Boolean = true): Future[SyncResult] = {
    verbose(s"postImageData($convId, $assetId, $asset, $data)")

    // save just posted data under new cacheKey, so we don't need to download it when image is accessed
    def updateImageCache(sent: ImageData) =
      if (sent.data64.isDefined) Future.successful(())
      else cache.addStream(sent.cacheKey, data.inputStream).future

    client.postImageAssetData(asset, assetId, convId, data, nativePush).future flatMap {
      case Right(ev@AssetAddEvent(_, _, _, _, _, _, image: ImageData)) =>
        verbose(s"postImageAssetData returned event: $ev with local data: $data")
        assert(image.sent, "sent flag should be set to true by event parser")
        for {
          _ <- updateImageCache(image)
          _ <- imageService.updateImageAsset(assetId, convId, image)
          _ <- convEvents.handlePostConversationEvent(ev)
        } yield SyncResult.Success
      case Right(ev@AssetAddEvent(_, _, _, _, _, _, other)) =>
        error(s"postImageData received unsupported asset: $other")
        Future.successful(SyncResult.failed())
      case Left(error) =>
        Future.successful(SyncResult(error))
    }
  }
}

class SyncException(msg: String) extends Exception(msg) with NoStackTrace
