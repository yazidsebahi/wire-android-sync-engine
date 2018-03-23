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
import com.waz.content.UserPreferences
import com.waz.content.UserPreferences.SelfClient
import com.waz.model._
import com.waz.model.otr.{Client, ClientId}
import com.waz.service.AccountManager.ClientRegistrationState.Unregistered
import com.waz.service.otr.OtrClientsService
import com.waz.service.otr.OtrService.sessionId
import com.waz.service.tracking.LoggedOutEvent
import com.waz.sync._
import com.waz.sync.client.OtrClient
import com.waz.sync.otr.{OtrClientsSyncHandler, OtrClientsSyncHandlerImpl}
import com.waz.sync.queue.{SyncContentUpdater, SyncContentUpdaterImpl}
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._
import com.waz.utils.events.{EventContext, Signal, SourceSignal}
import com.waz.utils.wrappers.Context
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Right

//Password will never be stored
class AccountManager(val id: UserId, val global: GlobalModule, val accounts: AccountsService, val password: SourceSignal[Option[String]])(implicit ec: EventContext) { self =>
  implicit val dispatcher = new SerialDispatchQueue()
  verbose(s"Creating for: $id")

  lazy val accountContext: AccountContext = new AccountContext(id, accounts)

  val storage: StorageModule = global.factory.baseStorage(id)
  val userPrefs = storage.userPrefs
  val accountData = global.accountsStorage.signal(id)

  val clientState = userPrefs(SelfClient).signal
  val clientId = clientState.map(_.clientId)

  lazy val cryptoBox          = global.factory.cryptobox(id, storage)
  lazy val auth               = global.factory.auth(id)
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
  lazy val usersStorage       = storage.usersStorage
  lazy val convsStorage       = storage.convsStorage
  lazy val membersStorage     = storage.membersStorage
  lazy val clientsStorage     = storage.otrClientsStorage

  lazy val otrClient:           OtrClient               = new OtrClient(netClient)
  lazy val clientsService:      OtrClientsService       = wire[OtrClientsService]
  lazy val clientsSync:         OtrClientsSyncHandler   = wire[OtrClientsSyncHandlerImpl]
  lazy val syncContent:         SyncContentUpdater      = wire[SyncContentUpdaterImpl]
  lazy val syncRequests:        SyncRequestServiceImpl  = wire[SyncRequestServiceImpl]
  lazy val sync:                SyncServiceHandle       = wire[AndroidSyncServiceHandle]
  lazy val syncHandler:         SyncHandler             = new AccountSyncHandler(zmessaging.collect { case Some(z) => z }, clientsSync)

  private val otrClients =
    storage.otrClientsStorage.signal(id)
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
      global.trackingService.loggedOut(LoggedOutEvent.RemovedClient, id)
      logoutAndResetClient()
    }
    hasClient = exists
  }

  // logged in zmessaging instance
  @volatile private var _zmessaging = Option.empty[ZMessaging]

  val zmessaging: Signal[Option[ZMessaging]] = (for {
    Some(cId)  <- clientId
    _          <- Signal.future(updateSelfTeam(id))
    Right(tId) <- userPrefs(UserPreferences.TeamId).signal
    Some(_)    <- Signal.future(checkCryptoBox)
  } yield {
    verbose(s"Creating new ZMessaging instance for $id, $cId, $tId, service: $this")
    _zmessaging = _zmessaging orElse LoggedTry(global.factory.zmessaging(tId, cId, this, storage, cryptoBox)).toOption
    _zmessaging
  }).orElse(Signal.const(Option.empty[ZMessaging]))

//  private var awaitActivationFuture = CancellableFuture successful Option.empty[AccountDataOld]
//
//  (for {
//    true      <- accounts.accountState(id).map(_ == InForeground)
//    Some(acc) <- global.accountsStorageOld.optSignal(id)
//  } yield
//    !acc.verified && acc.password.isDefined && (acc.pendingPhone.isDefined || acc.pendingEmail.isDefined)
//  ).orElse(Signal.const(false)).on(dispatcher) {
//    case true   => awaitActivationFuture = awaitActivationFuture.recover { case _: Throwable => () } flatMap { _ => awaitActivation(0) }
//    case false  => awaitActivationFuture.cancel()("stop_await_activate")
//  }

  def getZMessaging: Future[Option[ZMessaging]] = zmessaging.head

  //TODO update locally
  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit] = credentialsClient.updateEmail(email)

  def clearEmail(): ErrorOr[Unit] =
    credentialsClient.clearEmail().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => (userPrefs(UserPreferences.Email) := None).map(Right(_))
    }

  //TODO update locally
  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit] = credentialsClient.updatePhone(phone)

  def clearPhone(): ErrorOr[Unit] =
    credentialsClient.clearPhone().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => (userPrefs(UserPreferences.Phone) := None).map(Right(_))
    }

  //TODO do we have to re-authenitcate?
  def updatePassword(newPassword: String, currentPassword: Option[String]) =
    credentialsClient.updatePassword(newPassword, currentPassword).future.map {
      case Left(err) => Left(err)
      case Right(_)  => Right(password ! currentPassword)
    }

  def updateHandle(handle: Handle): ErrorOr[Unit] =
    credentialsClient.updateHandle(handle).future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => usersStorage.update(id, _.copy(handle = Some(handle))).map(_ => Right({}))
    }

  def fingerprintSignal(uId: UserId, cId: ClientId): Signal[Option[Array[Byte]]] =
    for {
      selfClientId <- clientId
      fingerprint  <-
        if (id == uId && selfClientId.contains(cId))
          Signal.future(cryptoBox(Future successful _.getLocalFingerprint))
        else
          cryptoBox.sessions.remoteFingerprint(sessionId(uId, cId))
    } yield fingerprint

  private def updateSelfTeam(userId: UserId): Future[Unit] = {

    import UserPreferences._
    userPrefs(TeamId).flatMutate {
      case t@Right(teamId) => teamId match {
        case Some(_) => //team members should be un-searchable (i.e., in privateMode) which is already set by default on BE
          (userPrefs(UserPreferences.PrivateMode) := true).map(_ => t)
        case None => Future.successful(t) //no team account - don't worry about privateMode
      }

      case Left(_) => teamsClient.findSelfTeam().future.flatMap {
        case Right(teamOpt) =>
          val updateUsers = teamOpt match {
            case Some(t) => storage.usersStorage.update(id, _.updated(Some(t.id)))
            case _ => Future.successful()
          }

          val updateTeams = teamOpt match {
            case Some(t) => global.teamsStorage.updateOrCreate(t.id, _ => t, t)
            case _ => Future.successful({})
          }

          val fetchPermissions = teamOpt match {
            case Some(t) => teamsClient.getPermissions(t.id, id).map {
              case Right(p) => Some(p)
              case Left(_) => None
            }.future
            case _ => Future.successful(None)
          }

          for {
            _ <- updateUsers
            _ <- updateTeams
            p <- fetchPermissions
            _ <- userPrefs(SelfPermissions) := p.map(_._1).getOrElse(0)
            _ <- userPrefs(CopyPermissions) := p.map(_._2).getOrElse(0)
          } yield Right(teamOpt.map(_.id))
        case Left(err) =>
          warn(s"Failed to update team information: ${err.message}")
          Future.successful(Left({}))
      }
    }
  }

  def registerClient(): ErrorOr[Unit] = {
    verbose(s"ensureClientRegistered: $id")
    for {
      client <- clientId.head
      pw     <- password.head
      resp   <-
        if (client.isDefined) Future.successful(Right({}))
        else {
          clientsSync.registerClient(pw).flatMap {
            case Right(state) =>
              verbose(s"Client registration complete: $state")
              if (state.clientId.isEmpty) {
                sync.syncSelfClients()
              }
              (userPrefs(SelfClient) := state).map(Right(_))
            case Left(err) =>
              error(s"client registration failed: $err")
              Future.successful(Left(err))
          }
        }
    } yield resp
  }

  def checkCryptoBox =
    cryptoBox.cryptoBox.flatMap {
      case Some(cb) => Future successful Some(cb)
      case None =>
        _zmessaging = None
        for {
          _ <- userPrefs(SelfClient) := Unregistered
          _ <- cryptoBox.deleteCryptoBox()
          res <- cryptoBox.cryptoBox
        } yield res
    }

  //TODO move to AccountService - account should be activated before we create an AccountManager
//  private def activate(accountId: AccountId): ErrorOr[AccountDataOld] =
//    global.accountsStorageOld.get(accountId).flatMap {
//      case Some(account) =>
//        if (account.verified && !account.regWaiting) Future successful Right(account)
//        else global.loginClient.login(account).future flatMap {
//          case Right((token, cookie)) =>
//            for {
//              Some((_, acc)) <- global.accountsStorageOld.update(id, _.updatedNonPending.copy(cookie = cookie, accessToken = Some(token)))
//            } yield Right(acc)
//          case Left((_, ErrorResponse(Status.Forbidden, _, "pending-activation"))) =>
//            global.accountsStorageOld.update(accountId, _.updatedPending).collect { case Some((_, acc)) => Left(ErrorResponse(Status.Forbidden, "", "pending-activation"))}
//          case Left((_, err)) =>
//            verbose(s"activate failed: $err")
//            Future.successful(Left(err))
//        }
//      case None => Future.successful(Left(ErrorResponse.internalError(s"no account for $accountId")))
//    }

//  private def awaitActivation(retry: Int = 0): CancellableFuture[Option[AccountDataOld]] =
//    CancellableFuture lift global.accountsStorageOld.get(id) flatMap {
//      case None => CancellableFuture successful None
//      case Some(data) if data.verified => CancellableFuture successful Some(data)
//      case Some(data) =>
//        CancellableFuture.lift(accounts.accountState(id).map(_ == LoggedOut).head).flatMap {
//          case true => CancellableFuture.lift(activate(data.id)).flatMap {
//            case Right(acc) if acc.verified => CancellableFuture successful Some(acc)
//            case _ =>
//              CancellableFuture.delay(ActivationThrottling.delay(retry)) flatMap { _ => awaitActivation(retry + 1) }
//          }
//          case false => CancellableFuture.successful(None)
//        }
//    }

  private def logoutAndResetClient() =
    for {
      _ <- accounts.logout(id)
      _ <- cryptoBox.deleteCryptoBox()
      _ =  _zmessaging = None // drop zmessaging instance, we need to create fresh one with new clientId // FIXME: dropped instance will still be active and using the same ZmsLifecycle instance
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
