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
package com.waz.service.media

import java.util.Locale

import com.waz.ZLog._
import com.waz.api.Message
import com.waz.content.KeyValueStorage
import com.waz.content.KeyValueStorage.KeyValuePref
import com.waz.content.Preference.PrefCodec
import com.waz.model.messages.media.MediaAssetData
import com.waz.model.{MessageContent, MessageData}
import com.waz.service.assets.AssetService
import com.waz.sync.client.OAuth2Client.{AccessToken, AuthorizationCode, ClientId, RefreshToken}
import com.waz.sync.client.SpotifyClient
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.wrappers.URI
import com.waz.znet.ZNetClient.ErrorOr

import scala.concurrent.Future

class SpotifyMediaService(client: SpotifyClient, assets: AssetService, keyValue: KeyValueStorage) {
  import SpotifyMediaService._
  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[SpotifyMediaService]

  private val spotifyRefreshTokenPref: KeyValuePref[Option[RefreshToken]] = keyValue.keyValuePref(KeyValueStorage.SpotifyRefreshToken, None)
  private val accessToken = new SourceSignal[Option[AccessToken]](Some(None))

  val authentication: Signal[Authentication] = spotifyRefreshTokenPref.signal zip accessToken map { case (refresh, access) =>
    Authentication(refresh.isDefined, ClientId(client.clientId.id), access)
  }

  def connectAccount(code: AuthorizationCode): ErrorOr[Boolean] = client.connectWithCode(code) flatMapEither { refreshToken: RefreshToken =>
    client.isPremiumAccount(refreshToken) mapRight (returning(_: Boolean) { _ => spotifyRefreshTokenPref := Option(refreshToken) })
  }

  def disconnectAccount() = {
    client.clearAccessToken()
    spotifyRefreshTokenPref := None
  }

  def updateMedia(msg: MessageData, content: MessageContent): ErrorOr[MessageContent] = resolveId(content.content).fold2(Future.successful(Right(content)), { id =>
    spotifyRefreshTokenPref() flatMap (client.getMedia(id, _)) map {
      case Right(media) =>
        assets.updateAssets(media.images.to[Vector])
        Right(content.copy(tpe = Message.Part.Type.SPOTIFY, richMedia = Some(media.media)))

      case Left(error) if error.isFatal =>
        warn(s"spotify media loading for ${content.content} failed fatally: $error, switching back to text")
        Right(content.copy(tpe = Message.Part.Type.TEXT, richMedia = None))

      case Left(error) =>
        warn(s"unable to resolve Spotify media with id $id: $error")
        Left(error)
    }
  })

  private def resolveId(content: String): Option[SpotifyId] = {
    val uri = URI.parse(content)
    if (!SpotifyClient.domainNames.contains(uri.getHost.toLowerCase(Locale.ENGLISH))) None else {
      uri.getPathSegments match {
        case Seq("track", id) =>
          verbose(s"resolved track id (from $content): $id")
          Some(TrackId(id))
        case Seq("album", id) =>
          verbose(s"resolved album id (from $content): $id")
          Some(AlbumId(id))
        case Seq("user", user, "playlist", id) =>
          verbose(s"resolved user and playlist ids (from $content): $user, $id")
          Some(PlaylistId(user, id))
        case other =>
          warn(s"unable to resolve spotify id from $content: $other")
          None
      }
    }
  }

  def prepareStreaming(media: MediaAssetData): ErrorOr[Vector[URI]] = for {
    refresh <- spotifyRefreshTokenPref()
    access <- client.refreshAccessToken(refresh)
    _ = accessToken ! access
  } yield Right(media.tracks flatMap { t => if (refresh.isDefined) t.streamUrl else t.previewUrl } map URI.parse)
}

object SpotifyMediaService {

  case class Authentication(connected: Boolean, clientId: ClientId, accessToken: Option[AccessToken])

  sealed trait SpotifyId
  case class TrackId(id: String) extends SpotifyId
  case class AlbumId(id: String) extends SpotifyId
  case class PlaylistId(user: String, id: String) extends SpotifyId
}
