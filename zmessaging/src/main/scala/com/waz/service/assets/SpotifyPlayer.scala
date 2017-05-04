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
package com.waz.service.assets

import java.util.concurrent.TimeUnit

import android.content.Context
import com.spotify.sdk.android.player.Player.InitializationObserver
import com.spotify.sdk.android.player.PlayerNotificationCallback.{ErrorType, EventType}
import com.spotify.sdk.android.player.{Player => SPlayer, _}
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.service.assets.GlobalRecordAndPlayService.{MediaPointer, SpotifyContent, UnauthenticatedContent}
import com.waz.service.media.SpotifyMediaService.Authentication
import com.waz.sync.client.OAuth2Client.{AccessToken, ClientId}
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.wrappers.URI
import org.threeten.bp
import org.threeten.bp.Duration

import scala.concurrent.Future._
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.ref.WeakReference
import scala.{PartialFunction => =/>}


class SpotifyPlayer private (delegate: SPlayer, ownerToken: AnyRef, initialUri: URI, oauthToken: AccessToken, auth: () => Future[Authentication], observer: Player.Observer) extends Player {
  import Threading.Implicits.Background

  private val callback = returning(new SpotifyCallback(WeakReference(this)))(cb => delegate.addPlayerNotificationCallback(cb))
  @volatile private var reaction = Option.empty[PlayerNotificationCallback]

  def onPlaybackError(errorType: ErrorType, msg: String): Unit = serialized(Future {
    error(s"received error: $errorType - $msg")
    reaction.foreach(_.onPlaybackError(errorType, msg))
  })
  def onPlaybackEvent(eventType: EventType, playerState: PlayerState): Unit = serialized(Future {
    debug(s"received event: $eventType (uri=${playerState.trackUri},playing=${playerState.playing},position=${playerState.positionInMs.millis},activeDevice=${playerState.activeDevice})")
    reaction.foreach(_.onPlaybackEvent(eventType, playerState))
  })

  override def start(): Future[Unit] = serialized(loginIfNeeded().flatMap(_ => startWith {
    delegate.clearQueue()
    delegate.play(initialUri.toString)
  }))

  override def resume(): Future[Unit] = serialized(startWith(delegate.resume()))

  private def startWith(triggerStart: => Unit): Future[Unit] = {
    val promisedStart = Promise[Unit]

    react { case (EventType.PLAY, _) =>
      react { case (EventType.PAUSE, _) =>
        observer.onCompletion()
      }(errorMsg => observer.onError(errorMsg))
      promisedStart.success(())
    }(errorMsg => promisedStart.failure(new RuntimeException(errorMsg)))

    triggerStart

    promisedStart.future.withTimeout(10.seconds)
  }

  override def pause(): Future[MediaPointer] = serialized {
    val promisedPause = Promise[MediaPointer]

    react { case (EventType.PAUSE, state) =>
      promisedPause.success(MediaPointer(SpotifyContent(URI.parse(state.trackUri), auth), bp.Duration.ofMillis(state.positionInMs)))
    }(errorMsg => promisedPause.failure(new RuntimeException(errorMsg)))

    promisedPause.future
  }

  override def repositionPlayhead(pos: Duration): Future[Unit] = serialized(Future(delegate.seekToPosition(pos.toMillis.toInt)))

  override def playhead: Future[Duration] = serialized(playerState.map(state => bp.Duration.ofMillis(state.positionInMs)))

  override def release(): Future[Unit] = Future {
    delegate.pause()
    delegate.removePlayerNotificationCallback(callback)
    val destroyed = Spotify.awaitDestroyPlayer(ownerToken, 10, TimeUnit.SECONDS)
    debug(s"destroyed: $destroyed")
  }(Threading.BlockingIO)

  override def finalize: Unit = {
    delegate.removePlayerNotificationCallback(callback)
    Spotify.destroyPlayer(ownerToken)
  }

  private def loginIfNeeded(): Future[Unit] = Future(delegate.isLoggedIn).flatMap { loggedIn =>
    if (loggedIn) successful(())
    else {
      val promisedLogin = Promise[Unit]
      delegate.addConnectionStateCallback(new ConnectionStateCallback {
        override def onLoggedIn(): Unit = {
          delegate.removeConnectionStateCallback(this)
          promisedLogin.success(())
        }
        override def onLoginFailed(cause: Throwable): Unit = {
          delegate.removeConnectionStateCallback(this)
          promisedLogin.failure(cause)
        }
        override def onConnectionMessage(s: String): Unit = ()
        override def onTemporaryError(): Unit = ()
        override def onLoggedOut(): Unit = ()
      })
      delegate.login(oauthToken.token)
      promisedLogin.future.withTimeout(10.seconds)
    }
  }

  // needs to be called from within "serialized"
  private def react(onEvent: (EventType, PlayerState) =/> Unit)(onError: String => Unit): Unit = {
    reaction = Some(new PlayerNotificationCallback {
      override def onPlaybackError(errorType: ErrorType, msg: String): Unit = {
        reaction = None
        onError(s"$errorType: $msg")
      }

      override def onPlaybackEvent(eventType: EventType, playerState: PlayerState): Unit = if (onEvent.isDefinedAt((eventType, playerState))) {
        reaction = None
        onEvent((eventType, playerState))
      }
    })
  }

  private def playerState: Future[PlayerState] = {
    val promisedState = Promise[PlayerState]
    delegate.getPlayerState(new PlayerStateCallback {
      override def onPlayerState(state: PlayerState): Unit = promisedState.success(state)
    })
    promisedState.future
  }
}

class SpotifyCallback(playerRef: WeakReference[SpotifyPlayer]) extends PlayerNotificationCallback {
  override def onPlaybackError(errorType: ErrorType, msg: String): Unit = playerRef.get.foreach(_.onPlaybackError(errorType, msg))
  override def onPlaybackEvent(eventType: EventType, playerState: PlayerState): Unit = playerRef.get.foreach(_.onPlaybackEvent(eventType, playerState))
}

object SpotifyPlayer {
  import Threading.Implicits.Background

  def apply(content: SpotifyContent, observer: Player.Observer)(implicit context: Context): Future[Player] = content.auth().flatMap { auth =>
    auth.accessToken.filter(_ => auth.connected).fold2(
      successful(DefaultPlayer(UnauthenticatedContent(content.uri), observer)),
      token => create(content.uri, token, auth.clientId, content.auth, observer))
  }

  private def create(uri: URI, oauthToken: AccessToken, clientId: ClientId, auth: () => Future[Authentication], observer: Player.Observer)(implicit context: Context): Future[SpotifyPlayer] = Threading.BackgroundHandler.flatMap { handler =>
    val config = returning(new Config(context, oauthToken.token, clientId.str, Config.DeviceType.SMARTPHONE))(_.useCache(true))
    val builder = new SPlayer.Builder(config).setCallbackHandler(handler)
    val ownerToken = new AnyRef
    val initializedPlayer = Promise[SPlayer]

    Spotify.getPlayer(builder, ownerToken, new InitializationObserver {
      override def onInitialized(player: SPlayer): Unit = initializedPlayer.success(player)
      override def onError(throwable: Throwable): Unit = initializedPlayer.failure(throwable)
    })

    initializedPlayer.future.map(new SpotifyPlayer(_, ownerToken, uri, oauthToken, auth, observer))(Threading.Background)
  }
}
