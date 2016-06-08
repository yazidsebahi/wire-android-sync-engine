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
package com.waz.service.tracking

import com.waz.ZLog._
import com.waz.api.NotificationsHandler.NotificationsHandlerFactory
import com.waz.api.{TrackingEvent, impl}
import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.TrackingEvent._
import com.waz.content.{AssetsStorage, ConversationStorage, MessagesStorage}
import com.waz.model.{AnyAssetData, AssetStatus, MessageId}
import com.waz.service.call.{AvsMetrics, FlowManagerService}
import com.waz.service.downloads.AssetDownloader
import com.waz.service.downloads.DownloadRequest.AnyAssetRequest
import com.waz.threading.Threading
import com.waz.utils._
import org.threeten.bp.{Duration, Instant}

import scala.concurrent.Future

class TrackingEventsService(handlerFactory: => NotificationsHandlerFactory, assets: AssetsStorage, messages: MessagesStorage, downloader: AssetDownloader) {
  import Threading.Implicits.Background
  import TrackingEventsService._
  import com.waz.utils.events.EventContext.Implicits.global

  private lazy val handler = handlerFactory.getTrackingEventsHandler

  downloader.onDownloadStarting {
    case AnyAssetRequest(_, id, _, _, _, _, mime, _) =>
      assets.getAsset(id) flatMapSome { asset => track(impl.TrackingEvent.assetDownloadStarted(asset.sizeInBytes)) }
    case _ => // ignore
  }

  downloader.onDownloadDone {
    case AnyAssetRequest(_, id, _, _, _, _, mime, _) =>
      assets.getAsset(id) flatMapSome { asset => track(impl.TrackingEvent.assetDownloadSuccessful(asset.sizeInBytes, mime.str)) }
    case _ => // ignore
  }

  downloader.onDownloadFailed {
    case (AnyAssetRequest(_, id, _, _, _, _, mime, _), err) if err.code != ErrorResponse.CancelledCode =>
      assets.getAsset(id) flatMapSome { asset => track(impl.TrackingEvent.assetDownloadFailed(asset.sizeInBytes)) }
    case _ => // ignore
  }

  messages.onMessageSent {
    case msg if msg.isAssetMessage =>
      assets.getAsset(msg.assetId).flatMapSome(a => track(assetUploadSuccessful(a.sizeInBytes, a.mimeType.str, Duration.between(msg.localTime, Instant.now))))
    case _ =>
  }

  messages.onMessageFailed {
    case (msg, error) if msg.isAssetMessage => track(assetUploadFailed(error))
    case _ =>
  }

  assets.onUploadFailed {
    case asset: AnyAssetData if asset.status == AssetStatus.UploadCancelled =>
      track(assetUploadCancelled(Some(asset.sizeInBytes), asset.mimeType.str))
    case _ =>
  }

  def sendAvsMetrics(avsMetrics: => AvsMetrics): Future[Unit] = Future {
    val metrics = avsMetrics
    verbose(s"avsMetrcs $metrics")
    handler.onAvsMetricsEvent(metrics)
  }(Threading.Ui).recoverWithLog()

  def track(event: => TrackingEvent): Future[Unit] = Future {
    val ev = event
    verbose(s"track $ev")
    handler.onTrackingEvent(ev)
  }(Threading.Ui).recoverWithLog()
}
object TrackingEventsService {
  private implicit val logTag: LogTag = logTagFor[TrackingEventsService]
}
