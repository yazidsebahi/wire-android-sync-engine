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

import com.waz.ZLog._
import com.waz.api
import com.waz.api.KindOfSpotifyAccount
import com.waz.api.Spotify.ConnectCallback
import com.waz.service.ZMessaging
import com.waz.service.media.SpotifyMediaService.Authentication
import com.waz.sync.client.OAuth2Client.AuthorizationCode
import com.waz.threading.Threading
import com.waz.ui.{SignalLoading, UiModule}

class Spotify(implicit context: UiModule) extends api.Spotify with UiObservable with SignalLoading {
  private implicit lazy val logTag: LogTag = logTagFor[Spotify]

  private var auth = Option.empty[Authentication]

  addLoader((_: ZMessaging).spotifyMedia.authentication)(setAuthentication)

  override def connect(authorizationCode: String, callback: ConnectCallback): Unit =
    context.zms .flatMapFuture (_.spotifyMedia.connectAccount(AuthorizationCode(authorizationCode))) .map {
      case Right(c) =>
        debug(s"Authorization succeeded: premium = $c")
        auth = auth map (_.copy(connected = true))
        Option(callback) foreach (_.onConnect(if (c) KindOfSpotifyAccount.PREMIUM else KindOfSpotifyAccount.OPEN))
      case Left(other) =>
        debug(s"Authorization failed: $other")
        auth = auth map (_.copy(connected = false))
        Option(callback) foreach (_.onConnect(KindOfSpotifyAccount.NONE))
    } (Threading.Ui)

  override def disconnect(): Unit = {
    context.zms(_.spotifyMedia.disconnectAccount())
    auth = auth map (_.copy(connected = false))
  }

  override def isConnected: Boolean = auth exists (_.connected)

  override def getClientId: String = auth .map (_.clientId.str) .orNull

  override def getAccessToken: String = auth .flatMap (_.accessToken) .map (_.token) .orNull

  private def setAuthentication(auth: Authentication) = {
    this.auth = Some(auth)
    notifyChanged()
  }
}
