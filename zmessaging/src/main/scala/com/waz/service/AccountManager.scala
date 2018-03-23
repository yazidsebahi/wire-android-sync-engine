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
import com.waz.api.impl._
import com.waz.content.UserPreferences.{SelfClient, ShouldSyncInitial}
import com.waz.model.otr.{Client, ClientId}
import com.waz.model.{UserData, _}
import com.waz.service.AccountsService.LoggedOut
import com.waz.service.AccountManager.ClientRegistrationState.Unregistered
import com.waz.service.AccountsService.{InForeground, LoggedOut}
import com.waz.service.otr.OtrService.sessionId
import com.waz.service.otr.OtrClientsService
import com.waz.service.tracking.{LoggedOutEvent, TrackingService}
import com.waz.sync._
import com.waz.sync.client.OtrClient
import com.waz.sync.otr.{OtrClientsSyncHandler, OtrClientsSyncHandlerImpl}
import com.waz.sync.queue.{SyncContentUpdater, SyncContentUpdaterImpl}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.wrappers.Context
import com.waz.utils.{RichInstant, _}
import com.waz.znet.Response.Status
import com.waz.znet.ZNetClient._
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Right
import scala.util.control.NonFatal

//TODO consider merging with AccountManager
class UserModule(val userId: UserId, val account: AccountManager, tracking: TrackingService) {
  private implicit val dispatcher = account.dispatcher

  implicit lazy val accountContext = account.accountContext

  lazy val storage = account.storage.currentValue.get
  lazy val cryptoBox = account.cryptoBox.currentValue.get

  def context = account.global.context
  def contextWrapper = Context.wrap(context)
  def db = storage.db
  def clientId = account.clientId
  def accountId = account.id
  def timeouts = account.global.timeouts
  def network = account.global.network
  def userPrefs = storage.userPrefs
  def usersStorage = storage.usersStorage
  def accStorage   = account.global.accountsStorageOld
  def convsStorage = storage.convsStorage
  def membersStorage = storage.membersStorage
  def clientsStorage = storage.otrClientsStorage
  def lifecycle = account.global.lifecycle
  def reporting = account.global.reporting
  def accountService = account.accounts

  lazy val otrClient = new OtrClient(account.netClient)
  lazy val clientsService:      OtrClientsService       = wire[OtrClientsService]
  lazy val clientsSync:         OtrClientsSyncHandler   = wire[OtrClientsSyncHandlerImpl]
  lazy val syncContent:         SyncContentUpdater      = wire[SyncContentUpdaterImpl]
  lazy val syncRequests:        SyncRequestServiceImpl  = wire[SyncRequestServiceImpl]
  lazy val sync:                SyncServiceHandle       = wire[AndroidSyncServiceHandle]
  lazy val syncHandler:         SyncHandler             = new AccountSyncHandler(account.zmessaging.collect { case Some(zms) => zms }, clientsSync)

  def ensureClientRegistered(accountId: AccountId): ErrorOr[Unit] = {
    verbose(s"ensureClientRegistered: $account")
    for {
      acc    <- accStorage.get(accountId)
      client <- clientId.head
      resp   <- acc.fold2(Future.successful(Left(ErrorResponse.InternalError)), acc =>
        if (client.isDefined) Future.successful(Right({}))
        else {
          clientsSync.registerClient(acc.password).flatMap {
            case Right(state) =>
              verbose(s"Client registration complete: $state")
              if (state.clientId.isEmpty) {
                sync.syncSelfClients()
              }
              account.setClientState(state).map(_ => Right(()))
            case Left(err) =>
              error(s"client registration failed: $err")
              Future.successful(Left(err))
          }
        })
    } yield resp
  }
}

class AccountManager(val id: UserId, val global: GlobalModule, val accounts: AccountsService)(implicit ec: EventContext) { self =>
  import AccountManager._
  implicit val dispatcher = new SerialDispatchQueue()
  verbose(s"Creating for: $id")

  lazy val accountContext: AccountContext = new AccountContext(id, accounts)

  val storage: StorageModule = global.factory.baseStorage(id)
  val accountData = global.accountsStorage.signal(id)

  val clientState = storage.flatMap(_.userPrefs.preference(SelfClient).signal)

  val clientId = clientState.map(_.clientId)

  val userId = accountData.map(_.userId)

  private val otrClients = storage.flatMap { st =>
    st.otrClientsStorage.signal(st.userId)
      .map(_.clients.values.toSet)
      .orElse(Signal.const(Set.empty[Client]))
  }

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

  accountData.onChanged { acc =>
    acc.userId.foreach { uId =>
      storage.head.flatMap(_.usersStorage.updateOrCreate(uId, identity, UserData(uId, None, "", acc.email, acc.phone, searchKey = SearchKey(""), connection = UserData.ConnectionStatus.Self, handle = acc.handle)))
    }
  }

  private val selfUserData = storage.flatMap(st => st.usersStorage.signal(st.userId))

  // listen to user data changes, update account email/phone if self user data is changed
  selfUserData.map(user => (user.email, user.phone)) { case (email, phone) =>
    verbose(s"self user data changed, email: $email, phone: $phone")
    global.accountsStorageOld.update(id, { acc =>
      if (acc.pendingPhone == phone) {
        acc.copy(email = email)
      } else {
        acc.copy(email = email, phone = phone)
      }
    })
  }

  selfUserData.map(_.deleted) { deleted =>
    if (deleted) {
      info(s"self user was deleted, logging out")
      global.trackingService.loggedOut(LoggedOutEvent.SelfDeleted, id)
      for {
        _ <- logoutAndResetClient()
        _ =  accounts.accountMap.remove(id)
        _ <- global.accountsStorageOld.remove(id)
      // TODO: delete database, account was deleted
      } yield ()
    }
  }

  lazy val cryptoBox          = storage.map(st => global.factory.cryptobox(st.userId, st))
  lazy val auth               = global.factory.auth(id)
  lazy val netClient          = global.factory.client(id, auth)
  lazy val usersClient        = global.factory.usersClient(netClient)
  lazy val teamsClient        = global.factory.teamsClient(netClient)
  lazy val credentialsClient  = global.factory.credentialsClient(netClient)

  auth.onInvalidCredentials.on(dispatcher){ _ =>
    global.trackingService.loggedOut(LoggedOutEvent.InvalidCredentials, id)
    logout(flushCredentials = true)
  }

  lazy val userModule = userId.collect {
    case Some(user) => global.factory.userModule(user, this)
  }

  lazy val clientId = accountData.map(_.clientId)

  lazy val userId = accountData.map(_.userId)

  // logged in zmessaging instance
  @volatile private var _zmessaging = Option.empty[ZMessaging]

  val shouldSyncPref = storage.userPrefs.preference(ShouldSyncInitial)
  (for {
    um             <- userModule
    shouldSync     <- shouldSyncPref.signal
  } if (shouldSync) um.sync.performFullSync().flatMap(_ => shouldSyncPref := false))(accountContext)

  val zmessaging: Signal[ZMessaging] = (for {
    Some(cId)  <- clientId
    aId        <- accountData.map(_.id)
    _          <- Signal.future(updateSelfTeam(aId))
    Right(tId) <- accountData.map(_.teamId)
    st         <- storage //wait until storage is defined, cryptobox and user module need this...
    um         <- userModule
    cb         <- cryptoBox
    Some(_)    <- Signal.future(cb.cryptoBox)
  } yield {
    verbose(s"Creating new ZMessaging instance, for $um, $cId, $tId, service: $this")
    _zmessaging = _zmessaging orElse LoggedTry(global.factory.zmessaging(tId, cId, um, st, cb)).toOption
    _zmessaging
  }).orElse(Signal.const(Option.empty[ZMessaging]))

  for {
    acc      <- accountData if acc.verified
    loggedIn <- accounts.accountState(id).map(_ != LoggedOut)
    client   <- otrCurrentClient
    _        <- otrClients.map(_.size)
  } {
    if (acc.userId.isEmpty || client.isEmpty) {
      verbose(s"account data needs registration: $acc")
      Serialized.future(self)(ensureFullyRegistered())
    }

    client.foreach { client =>
      if (client.signalingKey.isEmpty) {
        returning (s"Client registered ${client.regTime.map(_ until Instant.now).map(_.toDays).getOrElse(0)} ago is missing its signaling key") { msg =>
          warn(msg)
          global.trackingService.exception(new IllegalStateException(msg), msg, Some(id))
        }
        Serialized.future(self)(userModule.head.map(_.clientsSync.registerSignalingKey()))
      }
    }
  }

  private var awaitActivationFuture = CancellableFuture successful Option.empty[AccountDataOld]

  (for {
    true      <- accounts.accountState(id).map(_ == InForeground)
    Some(acc) <- global.accountsStorageOld.optSignal(id)
  } yield
    !acc.verified && acc.password.isDefined && (acc.pendingPhone.isDefined || acc.pendingEmail.isDefined)
  ).orElse(Signal.const(false)).on(dispatcher) {
    case true   => awaitActivationFuture = awaitActivationFuture.recover { case _: Throwable => () } flatMap { _ => awaitActivation(0) }
    case false  => awaitActivationFuture.cancel()("stop_await_activate")
  }

  def logout(flushCredentials: Boolean): Future[Unit] = {
    verbose(s"logout($id, flushCredentials: $flushCredentials)")
    accounts.logout(id, flushCredentials)
  }

  def getZMessaging: Future[Option[ZMessaging]] = zmessaging.head flatMap {
    case Some(zms) => Future successful Some(zms)
    case None =>
      Serialized.future(this) {
        global.accountsStorageOld.get(id) flatMap {
          case Some(acc) if acc.cookie.isEmpty =>
            verbose(s"account data has no cookie, user not logged in: $acc")
            Future successful None
          case Some(_) =>
            for {
              client <- clientId.head
              resp   <- ensureFullyRegistered() flatMap {
                case Right(a) if a.userId.isDefined && client.isDefined && a.verified =>
                  zmessaging.filter(_.isDefined).head // wait until loaded
                case _ =>
                  zmessaging.head
              }
            } yield resp
          case None =>
            Future successful None
        }
      }
  }

  def updateEmail(email: EmailAddress): ErrorOrResponse[Unit] = credentialsClient.updateEmail(email)

  def clearEmail(): ErrorOr[Unit] =
    credentialsClient.clearEmail().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => updateSelfAccountAndUser(_.copy(email = None), _.copy(email = None)).map(_ => Right({}))

    }

  def updatePhone(phone: PhoneNumber): ErrorOrResponse[Unit] = credentialsClient.updatePhone(phone)

  def clearPhone(): ErrorOr[Unit] =
    credentialsClient.clearPhone().future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => updateSelfAccountAndUser(_.copy(phone = None), _.copy(phone = None)).map(_ => Right({}))
    }

  def updatePassword(newPassword: String, currentPassword: Option[String]) =
    credentialsClient.updatePassword(newPassword, currentPassword).future flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_) =>
        global.accountsStorageOld.update(id, _.copy(password = Some(newPassword))) flatMap { _ =>
          getZMessaging map { _ => Right(()) }
        }
    }

  def updateHandle(handle: Handle): ErrorOr[Unit] =
    credentialsClient.updateHandle(handle).future.flatMap {
      case Left(err) => Future successful Left(err)
      case Right(_)  => updateSelfAccountAndUser(_.copy(handle = Some(handle)), _.copy(handle = Some(handle))).map(_ => Right({}))
    }

  def fingerprintSignal(uId: UserId, cId: ClientId): Signal[Option[Array[Byte]]] =
    for {
      selfId       <- userId
      selfClientId <- clientId
      fingerprint  <-
        if (selfId.contains(uId) && selfClientId.contains(cId))
          cryptoBox.flatMap { cb => Signal.future(cb(Future successful _.getLocalFingerprint)) }
        else
          cryptoBox.flatMap(_.sessions.remoteFingerprint(sessionId(uId, cId)))
    } yield fingerprint

  private def updateSelfAccountAndUser(acc: AccountDataOld => AccountDataOld, user: UserData => UserData) = {
    for {
      _         <- global.accountsStorageOld.update(id, acc)
      Some(zms) <- getZMessaging
      _         <- zms.usersStorage.update(zms.selfUserId, user)
    } yield {}
  }

  private def updateSelfTeam(accountId: AccountId): ErrorOr[Unit] = global.accountsStorageOld.get(accountId).flatMap {
    case Some(account) => account.teamId match {

      case Right(teamId) => teamId match {
        case Some(_) => //team members should be un-searchable (i.e., in privateMode) which is already set by default on BE
          accountsStorageOld.update(accountId, _.copy(privateMode = true)).map(_ => Right(()))
        case None => Future.successful(Right(())) //no team account - don't worry about privateMode
      }

      case Left(_) => teamsClient.findSelfTeam().future flatMap {
        case Right(teamOpt) =>
          val updateUsers = (teamOpt, account.userId) match {
            case (Some(t), Some(uId)) => storage.head.flatMap(_.usersStorage.update(uId, _.updated(Some(t.id))).map(_ => {}))
            case _ => Future.successful({})
          }

          val updateTeams = teamOpt match {
            case Some(t) => global.teamsStorage.updateOrCreate(t.id, _ => t, t)
            case _ => Future.successful({})
          }

          val fetchPermissions = (teamOpt, account.userId) match {
            case (Some(t), Some(u)) => teamsClient.getPermissions(t.id, u).map {
              case Right(p) => Some(p)
              case Left(_) => None
            }.future
            case _ => Future.successful(None)
          }

          for {
            _ <- updateUsers
            _ <- updateTeams
            p <- fetchPermissions
            _ <- global.accountsStorageOld.update(id, _.withTeam(teamOpt.map(_.id), p))
          } yield Right({})
        case Left(err) => Future.successful(Left(err))
      }
    }
    case _ => Future.successful(Left(ErrorResponse.InternalError))
  }

  def ensureFullyRegistered(): Future[Either[ErrorResponse, AccountDataOld]] = {
    verbose(s"ensureFullyRegistered()")

    def updateSelfUser(accountId: AccountId): ErrorOr[Unit] =
      global.accountsStorageOld.get(accountId).flatMap { account =>
        if (account.exists(_.userId.isDefined)) Future.successful(Right({}))
        else {
          usersClient.loadSelf().future flatMap {
            case Right(userInfo) =>
              verbose(s"got self user info: $userInfo")
              for {
                _ <- storage.head.flatMap(_.assetsStorage.mergeOrCreateAsset(userInfo.mediumPicture))
                _ <- storage.head.flatMap(_.usersStorage.updateOrCreate(userInfo.id, _.updated(userInfo).copy(syncTimestamp = System.currentTimeMillis()), UserData(userInfo).copy(connection = UserData.ConnectionStatus.Self, syncTimestamp = System.currentTimeMillis())))
                _ <- global.accountsStorageOld.update(id, _.updated(userInfo))
              } yield Right({})
            case Left(err) =>
              verbose(s"loadSelfUser failed: $err")
              Future successful Left(err)
          }
        }
      }

    def checkCryptoBox =
      cryptoBox.head.flatMap(_.cryptoBox flatMap {
        case Some(cb) => Future successful Some(cb)
        case None =>
          _zmessaging = None
          for {
            _ <- setClientState(Unregistered)
            _ <- cryptoBox.head.flatMap(_.deleteCryptoBox())
            res <- cryptoBox.head.flatMap(_.cryptoBox)
          } yield res
      })

    (for {
      Some(_)      <- checkCryptoBox
      Right(_)     <- activate(id)
      Right(_)     <- updateSelfUser(id)
      Right(_)     <- userModule.head.flatMap(_.ensureClientRegistered(id))
      Right(_)     <- updateSelfTeam(id)
      Some(after)  <- global.accountsStorageOld.get(id)
    } yield Right(after))
      .recover {
        case NonFatal(_) =>
          val msg = s"Error during registration for account: $id"
          warn(msg)
          Left(ErrorResponse.internalError(msg))
      }
  }

  private def activate(accountId: AccountId): ErrorOr[AccountDataOld] =
    global.accountsStorageOld.get(accountId).flatMap {
      case Some(account) =>
        if (account.verified && !account.regWaiting) Future successful Right(account)
        else global.loginClient.login(account).future flatMap {
          case Right((token, cookie)) =>
            for {
              Some((_, acc)) <- global.accountsStorageOld.update(id, _.updatedNonPending.copy(cookie = cookie, accessToken = Some(token)))
            } yield Right(acc)
          case Left((_, ErrorResponse(Status.Forbidden, _, "pending-activation"))) =>
            global.accountsStorageOld.update(accountId, _.updatedPending).collect { case Some((_, acc)) => Left(ErrorResponse(Status.Forbidden, "", "pending-activation"))}
          case Left((_, err)) =>
            verbose(s"activate failed: $err")
            Future.successful(Left(err))
        }
      case None => Future.successful(Left(ErrorResponse.internalError(s"no account for $accountId")))
    }

  private def awaitActivation(retry: Int = 0): CancellableFuture[Option[AccountDataOld]] =
    CancellableFuture lift global.accountsStorageOld.get(id) flatMap {
      case None => CancellableFuture successful None
      case Some(data) if data.verified => CancellableFuture successful Some(data)
      case Some(data) =>
        CancellableFuture.lift(accounts.accountState(id).map(_ == LoggedOut).head).flatMap {
          case true => CancellableFuture.lift(activate(data.id)).flatMap {
            case Right(acc) if acc.verified => CancellableFuture successful Some(acc)
            case _ =>
              CancellableFuture.delay(ActivationThrottling.delay(retry)) flatMap { _ => awaitActivation(retry + 1) }
          }
          case false => CancellableFuture.successful(None)
        }
    }

  private def logoutAndResetClient() =
    for {
      _ <- logout(flushCredentials = true)
      _ <- cryptoBox.head.flatMap(_.deleteCryptoBox())
      _ =  _zmessaging = None // drop zmessaging instance, we need to create fresh one with new clientId // FIXME: dropped instance will still be active and using the same ZmsLifecycle instance
      _ <- clientState (Unregistered)
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
