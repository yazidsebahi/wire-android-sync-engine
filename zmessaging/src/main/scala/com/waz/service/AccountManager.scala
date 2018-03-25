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

import com.softwaremill.macwire._
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.{Credentials, EmailCredentials, PhoneCredentials}
import com.waz.content.UserPreferences
import com.waz.content.UserPreferences.SelfClient
import com.waz.model.AccountData.Password
import com.waz.model._
import com.waz.model.otr.{Client, ClientId}
import com.waz.service.AccountManager.ClientRegistrationState
import com.waz.service.AccountManager.ClientRegistrationState.{Registered, Unregistered}
import com.waz.service.otr.OtrClientsService
import com.waz.service.otr.OtrService.sessionId
import com.waz.service.tracking.LoggedOutEvent
import com.waz.sync._
import com.waz.sync.client.OtrClient
import com.waz.sync.otr.{OtrClientsSyncHandler, OtrClientsSyncHandlerImpl}
import com.waz.sync.queue.{SyncContentUpdater, SyncContentUpdaterImpl}
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Right

class AccountManager(val userId:   UserId,
                     val teamId:   Option[TeamId],
                     val global:   GlobalModule,
                     val accounts: AccountsService,

                    //"first data" should not be relied on too long after construction!
                     firstCredentials: Option[Credentials] = None,
                     firstData:        Option[UserInfo]    = None) {

  implicit val dispatcher = new SerialDispatchQueue()
  implicit val accountContext: AccountContext = new AccountContext(userId, accounts)

  verbose(s"Creating for: $userId")

  val storage     = global.factory.baseStorage(userId)
  val db          = storage.db
  val userPrefs   = storage.userPrefs

  val account = global.accountsStorage.signal(userId)
  val clientState = userPrefs(SelfClient).signal
  val clientId = clientState.map(_.clientId)

  lazy val cryptoBox          = global.factory.cryptobox(userId, storage)
  lazy val auth               = global.factory.auth(userId)
  lazy val netClient          = global.factory.client(auth)
  lazy val usersClient        = global.factory.usersClient(netClient)
  lazy val teamsClient        = global.factory.teamsClient(netClient)
  lazy val credentialsClient  = global.factory.credentialsClient(netClient)

  lazy val context            = global.context
  lazy val contextWrapper     = Context.wrap(context)

  lazy val timeouts           = global.timeouts
  lazy val network            = global.network
  lazy val lifecycle          = global.lifecycle
  lazy val reporting          = global.reporting
  lazy val tracking           = global.trackingService
  lazy val usersStorage       = storage.usersStorage
  lazy val convsStorage       = storage.convsStorage
  lazy val membersStorage     = storage.membersStorage
  lazy val clientsStorage     = storage.otrClientsStorage

  lazy val otrClient:           OtrClient               = new OtrClient(netClient)
  lazy val clientsService:      OtrClientsService       = wire[OtrClientsService]
  lazy val clientsSync:         OtrClientsSyncHandler   = wire[OtrClientsSyncHandlerImpl] //TODO - just use otrClient directly?
  lazy val syncContent:         SyncContentUpdater      = wire[SyncContentUpdaterImpl]
  lazy val syncRequests:        SyncRequestServiceImpl  = wire[SyncRequestServiceImpl]
  lazy val sync:                SyncServiceHandle       = wire[AndroidSyncServiceHandle]
  lazy val syncHandler:         SyncHandler             = new AccountSyncHandler(zmessaging, clientsSync)

  val firstLogin: Signal[Boolean] = db.dbHelper.wasCreated

  firstCredentials.foreach {
    case c: EmailCredentials =>
      userPrefs(UserPreferences.Email) := Some(c.email)
    case c: PhoneCredentials =>
      userPrefs(UserPreferences.Phone) := Some(c.phone)
    case _ =>
  }

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

  val zmessaging: Future[ZMessaging] = {
    for {
      _       <- firstData.fold2(Future.successful({}), u => usersStorage.updateOrCreate(userId, _.updated(u), UserData(u).copy(connection = UserData.ConnectionStatus.Self)))
      cId     <- clientId.collect { case Some(id) => id }.head
      Some(_) <- checkCryptoBox()
    } yield {
      verbose(s"Creating new ZMessaging instance for $userId, $cId, $teamId, service: $this")
      global.factory.zmessaging(teamId, cId, this, storage, cryptoBox)
    }
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
      case Right(_)  => global.accountsStorage.update(userId, _.copy(password = Some(newPassword)))
    }

  def updateHandle(handle: Handle): ErrorOr[Unit] =
    credentialsClient.updateHandle(handle).future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => usersStorage.update(userId, _.copy(handle = Some(handle))).map(_ => Right({}))
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

  //TODO move to team service - no need to have here any more
  private def updateSelfPermissions(): Future[Unit] = {
    import UserPreferences._
    val fetchPermissions = teamId match {
      case Some(t) => teamsClient.getPermissions(t, userId).map {
        case Right(p) => Some(p)
        case Left(_) => None
      }.future
      case _ => Future.successful(None)
    }

    for {
      p <- fetchPermissions
      _ <- userPrefs(SelfPermissions) := p.map(_._1).getOrElse(0)
      _ <- userPrefs(CopyPermissions) := p.map(_._2).getOrElse(0)
    } yield {}
  }
  updateSelfPermissions()

  def registerClient(): ErrorOr[ClientRegistrationState] = {
    verbose(s"ensureClientRegistered: $userId")
    for {
      curState <- clientState.head
      pw   <- account.map(_.password).head
      resp <- curState match {
        case Registered(_) => Future.successful(Right(curState))
        case _ =>
          clientsSync.registerClient(pw).flatMap {
            case Right(state) =>
              verbose(s"Client registration complete: $state")
              if (state.clientId.isEmpty) {
                sync.syncSelfClients()
              }
              (userPrefs(SelfClient) := state).map(_ => Right(state))
            case Left(err) =>
              error(s"client registration failed: $err")
              Future.successful(Left(err))
          }
      }
    } yield resp
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
