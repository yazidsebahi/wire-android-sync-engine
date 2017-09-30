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
package com.waz.service.push

import java.io.IOException

import com.waz.ZLog
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.content.GlobalPreferences.{CurrentAccountPref, PushEnabledKey}
import com.waz.content.{AccountsStorage, GlobalPreferences}
import com.waz.model.{AccountId, PushToken, PushTokenRemoveEvent}
import com.waz.service._
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.GoogleApi
import com.waz.utils.{Backoff, ExponentialBackoff, returning}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
  * Responsible for deciding when to generate and register push tokens and whether they should be active at all.
  */
class PushTokenService(googleApi:    GoogleApi,
                       backend:      BackendConfig,
                       globalToken:  GlobalTokenService,
                       prefs:        GlobalPreferences,
                       lifeCycle:    ZmsLifeCycle,
                       accountId:    AccountId,
                       accStorage:   AccountsStorage,
                       sync:         SyncServiceHandle) {

  implicit lazy val logTag: LogTag = s"${logTagFor[PushTokenService]}#${accountId.str.take(8)}"

  implicit val dispatcher = globalToken.dispatcher
  implicit val ev = globalToken.ev

  val pushEnabled    = prefs.preference(PushEnabledKey)
  val currentAccount = prefs.preference(CurrentAccountPref)
  val currentToken   = globalToken.currentToken

  private val isLoggedIn = lifeCycle.accLoggedIn(accountId)

  private val userToken = accStorage.signal(accountId).map(_.registeredPush)

  val pushActive = (for {
    push           <- pushEnabled.signal                      if push
    play           <- googleApi.isGooglePlayServicesAvailable if play
    inForeground   <- lifeCycle.accInForeground(accountId)    if !inForeground
    current        <- currentToken.signal                     if current.isDefined
    userRegistered <- userToken.map(_ == current)
  } yield userRegistered)
    .orElse(Signal.const(false))

  val eventProcessingStage = EventScheduler.Stage[PushTokenRemoveEvent] { (_, events) =>
    globalToken.resetGlobalToken(events.map(_.token))
  }

  //on dispatcher prevents infinite register loop
  (for {
    true        <- isLoggedIn
    userToken   <- userToken
    globalToken <- currentToken.signal
  } yield (globalToken, userToken)).on(dispatcher) {
    case (Some(glob), Some(user)) if glob != user =>
      sync.deletePushToken(user)
      sync.registerPush(glob)
    case (Some(glob), None) =>
      sync.registerPush(glob)
    case (None, Some(user)) =>
      sync.deletePushToken(user)
    case _ => //do nothing
  }

  def onTokenRegistered(token: PushToken): Future[Unit] = {
    verbose(s"onTokenRegistered: $accountId, $token")
    (for {
      true <- isLoggedIn.head
      _    <- accStorage.update(accountId, _.copy(registeredPush = Some(token)))
    } yield {}).recover {
      case _ => warn("account was not logged in after token sync completed")
    }
  }
}

class GlobalTokenService(googleApi: GoogleApi,
                         prefs: GlobalPreferences,
                         network: NetworkModeService) {
  import PushTokenService._
  import ZLog.ImplicitTag._

  implicit val dispatcher = new SerialDispatchQueue(name = "PushTokenDispatchQueue")
  implicit val ev = EventContext.Global

  val currentToken = prefs.preference(GlobalPreferences.PushToken)
  val resetToken   = prefs.preference(GlobalPreferences.ResetPushToken)

  private var attempts = 0
  private var settingToken = CancellableFuture.successful({})

  for {
    play    <- googleApi.isGooglePlayServicesAvailable
    current <- currentToken.signal
    network <- network.networkMode
  } if (play && current.isEmpty && network != NetworkMode.OFFLINE) setNewToken()

  for {
    play    <- googleApi.isGooglePlayServicesAvailable
    reset   <- resetToken.signal
    network <- network.networkMode
  } if (play && reset && network != NetworkMode.OFFLINE) resetGlobalToken()

  //Specify empty to force remove all tokens, or else only remove if `toRemove` contains the current token.
  def resetGlobalToken(toRemove: Vector[PushToken] = Vector.empty) =
    currentToken().flatMap {
      case Some(t) if toRemove.contains(t) || toRemove.isEmpty =>
        verbose("Resetting push token")
        googleApi.deleteAllPushTokens()
        for {
          _ <- resetToken := false
          _ <- currentToken := None
        } yield {}
      case _ => Future.successful({})
    }

  def setNewToken(): Future[Unit] = {

    def generateToken(): CancellableFuture[PushToken] = {
      dispatcher {
        returning(googleApi.getPushToken) { t =>
          verbose(s"Setting new push token: $t")
        }
      }.recoverWith {
        case ex: IOException =>
          error(s"Failed to generate push token due to server connectivity error, will retry again", ex)
          attempts += 1
          for {
            _ <- CancellableFuture.delay(ResetBackoff.delay(attempts))
            _ <- CancellableFuture.lift(network.networkMode.filter(_ != NetworkMode.OFFLINE).head)
            t <- generateToken()
          } yield t

        case NonFatal(ex) =>
          error(s"Something went wrong trying to generate push token, aborting", ex)
          CancellableFuture.failed(ex)
      }
    }

    if (settingToken.isCompleted) {
      attempts = 0
      settingToken = for {
        t <- generateToken().map(Some(_))
        _ <- CancellableFuture.lift(currentToken := t)
      } yield {}
    }
    settingToken
  }
}

object PushTokenService {
  var ResetBackoff: Backoff = new ExponentialBackoff(1000.millis, 10.seconds)
  case class PushSenderId(str: String) extends AnyVal
}
