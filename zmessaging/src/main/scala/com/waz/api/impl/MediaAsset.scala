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
package com.waz.api.impl

import java.util

import android.net.Uri
import com.waz.api
import com.waz.api.MediaAsset.StreamingCallback
import com.waz.api.{KindOfMedia, MediaProvider}
import com.waz.model.messages.media.{EmptyMediaAssetData, PlaylistData, TrackData, MediaAssetData}
import com.waz.threading.Threading
import com.waz.ui.UiModule
import org.threeten.bp.Duration
import scala.collection.JavaConverters._

abstract class BaseMediaAsset(protected val data: MediaAssetData) extends api.MediaAsset {
  override def getKind: KindOfMedia = data.kind
  override def getProvider: MediaProvider = data.provider
  override def getTitle: String = data.title
  override def getLinkUri: Uri = Uri.parse(data.linkUrl)
  override def getDuration: Duration = data.duration.orNull
  override def getArtistName: String = data.artist.map(_.name).getOrElse("")
  override def isStreamable: Boolean = data match {
    case t: TrackData           => t.streamable
    case _: PlaylistData        => data.tracks.exists(_.streamable)
    case _: EmptyMediaAssetData => false
  }
  override def isEmpty: Boolean = data.kind == KindOfMedia.UNKNOWN
}

class MediaAsset(mad: MediaAssetData)(implicit context: UiModule) extends BaseMediaAsset(mad) {
  override def getArtwork: api.ImageAsset = data.artwork.map(context.images.getImageAsset).getOrElse(ImageAsset.Empty)
  override def getArtistAvatar: api.ImageAsset = data.artist.flatMap(_.avatar).map(context.images.getImageAsset).getOrElse(ImageAsset.Empty)
  override def getTracks: java.util.List[api.MediaAsset] = data.tracks.map(new MediaAsset(_): api.MediaAsset).asJava

  override def prepareStreaming(cb: StreamingCallback): Unit = {
    context.zms.flatMapFuture(_.richmedia.prepareStreaming(data)).map {
      case Right(uris) => cb.onSuccess(uris.asJava)
      case Left(ErrorResponse(code, msg, label)) => cb.onFailure(code, msg, label)
    } (Threading.Ui)
  }
}

object EmptyMediaAsset extends BaseMediaAsset(EmptyMediaAssetData(MediaProvider.YOUTUBE)) {
  override def getArtwork: api.ImageAsset = ImageAsset.Empty
  override def getArtistAvatar: api.ImageAsset = ImageAsset.Empty
  override def getTracks: util.List[api.MediaAsset] = util.Collections.emptyList()
  override def prepareStreaming(cb: StreamingCallback): Unit = cb.onSuccess(util.Collections.emptyList())
}
