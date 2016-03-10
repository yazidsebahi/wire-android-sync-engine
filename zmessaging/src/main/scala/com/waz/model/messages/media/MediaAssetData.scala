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
package com.waz.model.messages.media

import com.waz.api.{Message, KindOfMedia, MediaProvider}
import com.waz.bitmap.BitmapUtils
import com.waz.model.{ImageData, RConvId, ImageAssetData, AssetId}
import org.threeten.bp.{Instant, Duration}
import scala.concurrent.duration._

sealed trait MediaAssetData {
  def kind: KindOfMedia
  def provider: MediaProvider
  def title: String
  def artist: Option[ArtistData]
  def duration: Option[Duration]
  def linkUrl: String
  def artwork: Option[AssetId]
  def tracks: Vector[TrackData]
  def expires: Instant

  def hasExpired: Boolean = Instant.now isAfter expires
}

case class TrackData(provider: MediaProvider, title: String, artist: Option[ArtistData], linkUrl: String, artwork: Option[AssetId], duration: Option[Duration], streamable: Boolean, streamUrl: Option[String], previewUrl: Option[String], expires: Instant) extends MediaAssetData {
  val kind = KindOfMedia.TRACK

  def tracks: Vector[TrackData] = Vector(this)
}

case class PlaylistData(provider: MediaProvider, title: String, artist: Option[ArtistData], linkUrl: String, artwork: Option[AssetId], duration: Option[Duration], tracks: Vector[TrackData], expires: Instant) extends MediaAssetData {
  val kind = KindOfMedia.PLAYLIST
}

case class EmptyMediaAssetData(provider: MediaProvider) extends MediaAssetData {
  val kind = KindOfMedia.UNKNOWN
  val title = ""
  val artist = None
  val linkUrl = ""
  val artwork = None
  val duration = None
  val expires = Instant.EPOCH
  val tracks = Vector.empty
}

case class ArtistData(name: String, avatar: Option[AssetId])

object MediaAssetData {
  import com.waz.utils._

  case class MediaWithImages[+T <: MediaAssetData](media: T, images: Set[ImageAssetData])

  case class Thumbnail(tag: String, url: String, width: Int, height: Int)

  val DefaultExpiryTime = 7.days
  def defaultExpiry: Instant = Instant.now plus DefaultExpiryTime
  def expiryAfter(d: FiniteDuration) = Instant.now plus d

  def empty(partType: Message.Part.Type): MediaAssetData = EmptyMediaAssetData(partType match {
    case Message.Part.Type.SOUNDCLOUD => MediaProvider.SOUNDCLOUD
    case Message.Part.Type.SPOTIFY => MediaProvider.SPOTIFY
    case _ => MediaProvider.YOUTUBE
  })

  implicit lazy val KindOfMediaCodec: EnumCodec[KindOfMedia, String] = EnumCodec.injective {
    case KindOfMedia.PLAYLIST => "playlist"
    case KindOfMedia.TRACK => "track"
    case KindOfMedia.UNKNOWN => "unknown"
  }

  implicit lazy val MediaProviderCodec: EnumCodec[MediaProvider, String] = EnumCodec.injective {
    case MediaProvider.SOUNDCLOUD => "soundcloud"
    case MediaProvider.SPOTIFY => "spotify"
    case MediaProvider.YOUTUBE => "youtube"
  }

  def imageAsset(thumbs: Vector[Thumbnail]): ImageAssetData = {
    val orig = thumbs.lastOption

    ImageAssetData(AssetId(), RConvId.Empty, thumbs map { thumb =>
      ImageData(thumb.tag, BitmapUtils.Mime.Jpg, thumb.width, thumb.height, orig map (_.width) getOrElse 0, orig map (_.height) getOrElse 0, sent = true, url = Some(thumb.url))
    })
  }

  def extractImageAssets[T <: MediaAssetData](src: Vector[MediaWithImages[T]]) = src.foldLeft((Vector.empty[T], Set.empty[ImageAssetData])) { case ((tracks, images), MediaWithImages(track, image)) => (tracks :+ track, images ++ image) }
}
