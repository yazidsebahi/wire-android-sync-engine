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
package com.waz.sync.client

import com.waz.api.MediaProvider
import com.waz.model.AssetMetaData.Image.Tag.Medium
import com.waz.model.{AssetData, AssetMetaData, Dim2}
import com.waz.model.messages.media.MediaAssetData.MediaWithImages
import com.waz.model.messages.media.{ArtistData, PlaylistData, TrackData}
import com.waz.testutils.Matchers._
import com.waz.utils.wrappers.URI
import com.waz.znet.JsonResponseFromResources
import org.scalatest._
import org.threeten.bp.Duration

@Ignore class SpotifyClientSpec extends FeatureSpec with Matchers with JsonResponseFromResources with OptionValues with RobolectricTests {
  import SpotifyClient._

  feature("response parsing") {
    scenario("parse track response") {
      sampleTrackResponse should beMatching({
        case TrackResponse(MediaWithImages(TrackData(MediaProvider.SPOTIFY, "Don't Be So Hard On Yourself", Some(ArtistData("Jess Glynne", None)), "https://open.spotify.com/track/0sUyqewVzwv0e5tK3hS6vJ", Some(assetId), Some(d), true, Some("spotify:track:0sUyqewVzwv0e5tK3hS6vJ"), Some("https://p.scdn.co/mp3-preview/508e071166fb8c564b90a1dc6c37f41e6544f077"), _), assets)) if d == Duration.ofMillis(211460) && assets.size == 1 && assets.headOption.exists(_.id == assetId) => true
      })

      val asset = TrackResponse.unapply(sampleTrackResponse).value.images.headOption.value

      asset shouldEqual AssetData(metaData = Some(AssetMetaData.Image(Dim2(640, 640), Medium)), source = Some(URI.parse("https://i.scdn.co/image/64a7ae4a4804ae88f3b93af220c2eff8ee9b2882")))

//      asset.map(img => (img.tag, img.url.value, img.width, img.height)) should contain theSameElementsInOrderAs Seq(
//        ("0", "https://i.scdn.co/image/1c7c821199e9ca963108f6d7b99dede10b19605e", 64, 64),
//        ("1", "https://i.scdn.co/image/bae3fdea03d703ec0e06b5a584d17a6af8f3ed1a", 300, 300),
//        ("2", "https://i.scdn.co/image/64a7ae4a4804ae88f3b93af220c2eff8ee9b2882", 640, 640))
    }

    scenario("parse album response") {
      sampleAlbumResponse should beMatching({
        case AlbumResponse(MediaWithImages(PlaylistData(MediaProvider.SPOTIFY, "Embrace", Some(ArtistData("Armin van Buuren", None)), "https://open.spotify.com/album/7G53des9iRPuUydeFXXNGy", Some(assetId), Some(_), tracks, _), assets)) if tracks.size == 15 && assets.size == 1 && assets.exists(_.id == assetId) => true
      })
    }

    scenario("parse playlist response") {
      samplePlaylistResponse should beMatching({
        case PlaylistResponse(MediaWithImages(PlaylistData(MediaProvider.SPOTIFY, "New Music Friday", Some(ArtistData("spotify_germany", None)), "http://open.spotify.com/user/spotify_germany/playlist/4HdOsN3i6umE8rN1y75NBi", Some(assetId), Some(_), tracks, _), assets)) if tracks.size == 67 && assets.size == 68 && assets.exists(_.id == assetId) => true
      })

      val MediaWithImages(pl, assets) = PlaylistResponse.unapply(samplePlaylistResponse).value
      val image = assets.find(a => pl.artwork.contains(a.id)).value
      image shouldEqual AssetData(metaData = Some(AssetMetaData.Image(Dim2(0, 0), Medium)), source = Some(URI.parse("https://u.scdn.co/images/pl/default/02c1007d606188e383f00571520d97e488c7dc28")))
    }
  }

  lazy val sampleTrackResponse = jsonFromResource("/client/spotify-track.json")
  lazy val sampleAlbumResponse = jsonFromResource("/client/spotify-album.json")
  lazy val samplePlaylistResponse = jsonFromResource("/client/spotify-playlist.json")
}
