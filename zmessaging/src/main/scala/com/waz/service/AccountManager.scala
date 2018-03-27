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
package com.waz.service

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.ZmsVersion
import com.waz.api.impl.ErrorResponse
import com.waz.content.UserPreferences
import com.waz.content.UserPreferences.{ClientRegVersion, SelfClient}
import com.waz.model.AccountData.Password
import com.waz.model._
import com.waz.model.otr.{Client, ClientId}
import com.waz.service.AccountManager.ClientRegistrationState
import com.waz.service.AccountManager.ClientRegistrationState.{LimitReached, PasswordMissing, Registered, Unregistered}
import com.waz.service.otr.OtrService.sessionId
import com.waz.service.tracking.LoggedOutEvent
import com.waz.sync.client.OtrClient
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet.Response.Status
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Right

class AccountManager(val userId:   UserId,
                     val teamId:   Option[TeamId],
                     val global:   GlobalModule,
                     val accounts: AccountsService) {

  implicit val dispatcher = new SerialDispatchQueue()
  implicit val accountContext: AccountContext = new AccountContext(userId, accounts)

  verbose(s"Creating for: $userId")

  val storage   = global.factory.baseStorage(userId)
  val db        = storage.db
  val userPrefs = storage.userPrefs

  val account     = global.accountsStorage.signal(userId)
  val clientState = userPrefs(SelfClient).signal
  val clientId    = clientState.map(_.clientId)

  val context        = global.context
  val contextWrapper = Context.wrap(context)

  val cryptoBox         = global.factory.cryptobox(userId, storage)
  val auth              = global.factory.auth(userId)
  val netClient         = global.factory.client(auth)
  val otrClient         = new OtrClient(netClient)
  val credentialsClient = global.factory.credentialsClient(netClient)

  val timeouts       = global.timeouts
  val network        = global.network
  val lifecycle      = global.lifecycle
  val reporting      = global.reporting
  val tracking       = global.trackingService
  val clientsStorage = storage.otrClientsStorage

  val zmessaging: Future[ZMessaging] = {
    for {
      cId     <- clientId.collect { case Some(id) => id }.head
      Some(_) <- checkCryptoBox()
    } yield {
      verbose(s"Creating new ZMessaging instance for $userId, $cId, $teamId, service: $this")
      global.factory.zmessaging(teamId, cId, this, storage, cryptoBox)
    }
  }

  val firstLogin: Signal[Boolean] = db.dbHelper.wasCreated

  private val otrClients =
    storage.otrClientsStorage.signal(userId)
      .map(_.clients.values.toSet)
      .orElse(Signal.const(Set.empty[Client]))

  // listen to client changes, logout and delete cryptobox if current client is removed
  private val otrCurrentClient = clientId.flatMap {
    case Some(cId) => otrClients.map(_.find(_.id == cId))
    case _ => Signal const Option.empty[Client]
  }
  private var hasClient = false
  otrCurrentClient.map(_.isDefined) { exists =>
    if (hasClient && !exists) {
      info(s"client has been removed on backend, logging out")
      global.trackingService.loggedOut(LoggedOutEvent.RemovedClient, userId)
      logoutAndResetClient()
    }
    hasClient = exists
  }

  //TODO update locally
  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit] =
    credentialsClient.updateEmail(email)

  def clearEmail(): ErrorOr[Unit] =
    credentialsClient.clearEmail().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => (userPrefs(UserPreferences.Email) := None).map(Right(_))
    }

  //TODO update locally
  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit] =
    credentialsClient.updatePhone(phone)

  def clearPhone(): ErrorOr[Unit] =
    credentialsClient.clearPhone().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => (userPrefs(UserPreferences.Phone) := None).map(Right(_))
    }

  //TODO do we have to re-authenticate?
  def updatePassword(newPassword: Password, currentPassword: Option[Password]) =
    credentialsClient.updatePassword(newPassword, currentPassword).future.flatMap {
      case Left(err) => Future.successful(Left(err))
      case Right(_)  => global.accountsStorage.update(userId, _.copy(password = Some(newPassword))).map(_ => Right({}))
    }

  def updateHandle(handle: Handle): ErrorOr[Unit] =
    credentialsClient.updateHandle(handle).future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => storage.usersStorage.update(userId, _.copy(handle = Some(handle))).map(_ => Right({}))
    }

  def fingerprintSignal(uId: UserId, cId: ClientId): Signal[Option[Array[Byte]]] =
    for {
      selfClientId <- clientId
      fingerprint  <-
        if (userId == uId && selfClientId.contains(cId))
          Signal.future(cryptoBox(Future successful _.getLocalFingerprint))
        else
          cryptoBox.sessions.remoteFingerprint(sessionId(uId, cId))
    } yield fingerprint

  def registerClient(password: Option[Password] = None): Future[Either[ErrorResponse, ClientRegistrationState]] = {
    verbose(s"registerClient: pw: $password")
    Serialized.future("sync-self-clients", this) {
      for {
        curState <- clientState.head
        pw       <- password.fold2(account.map(_.password).head, p => Future.successful(Some(p)))
        client   <- cryptoBox.createClient()
        resp <- client match {
          case None => Future.successful(Left(ErrorResponse.internalError("CryptoBox missing")))
          case Some((c, lastKey, keys)) =>
            otrClient.postClient(userId, c, lastKey, keys, password).future.flatMap {
              case Right(cl) =>
                for {
                  _   <- userPrefs(ClientRegVersion) := ZmsVersion.ZMS_MAJOR_VERSION
                  ucs <- clientsStorage.updateClients(Map(userId -> Seq(c.copy(id = cl.id).updated(cl))))
                } yield Right(Registered(cl.id))
              case Left(error@ErrorResponse(Status.Forbidden, _, "missing-auth")) =>
                warn(s"client registration not allowed: $error, password missing")
                Future.successful(Right(PasswordMissing))
              case Left(error@ErrorResponse(Status.Forbidden, _, "too-many-clients")) =>
                warn(s"client registration not allowed: $error")
                Future.successful(Right(LimitReached))
              case Left(error) =>
                Future.successful(Left(error))
            }
        }
      } yield resp
    }
  }

  private def checkCryptoBox() =
    cryptoBox.cryptoBox.flatMap {
      case Some(cb) => Future.successful(Some(cb))
      case None =>
        for {
          _ <- userPrefs(SelfClient) := Unregistered
          _ <- cryptoBox.deleteCryptoBox()
          res <- cryptoBox.cryptoBox
        } yield res
    }

  private def logoutAndResetClient() =
    for {
      _ <- accounts.logout(userId)
      _ <- cryptoBox.deleteCryptoBox()
      _ <- userPrefs(SelfClient) := Unregistered
    } yield ()
}

object AccountManager {

  trait ClientRegistrationState {
    val clientId: Option[ClientId] = None
  }

  object ClientRegistrationState {
    case object Unregistered    extends ClientRegistrationState
    case object PasswordMissing extends ClientRegistrationState
    case object LimitReached    extends ClientRegistrationState
    case class  Registered(cId: ClientId) extends ClientRegistrationState {
      override val clientId = Some(cId)
    }
  }

  val ActivationThrottling = new ExponentialBackoff(2.seconds, 15.seconds)
}
