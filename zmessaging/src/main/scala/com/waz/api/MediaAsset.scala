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
package com.waz.api

import com.waz.api.Asset.LoadCallback
import com.waz.utils.URI
import org.threeten.bp.Duration

object MediaAsset {
  trait StreamingCallback {
    def onSuccess(uris: java.util.List[URI]): Unit
    def onFailure(code: Int, message: String, label: String): Unit
  }
}

trait MediaAsset {
  import MediaAsset._

  def getKind: KindOfMedia
  def getProvider: MediaProvider

  def getTitle: String
  def getDuration: Duration // should always be available

  def getLinkUri: URI // should always be available
  def getArtwork: ImageAsset // optional

  def getArtistName: String // should always be available
  def getArtistAvatar: ImageAsset // optional

  def getTracks: java.util.List[MediaAsset] // might be empty, might only contain self for tracks

  def isStreamable: Boolean

  def prepareStreaming(cb: StreamingCallback): Unit // retrieve the current streaming uris

  def isEmpty: Boolean // whether it's a placeholder (retrieval of real data in progress)

  def getPlaybackControls(callback: LoadCallback[PlaybackControls]): Unit
}
