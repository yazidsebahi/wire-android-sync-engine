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

import java.io.File

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.impl._
import com.waz.api.{Credentials, KindOfAccess, KindOfVerification, PhoneCredentials}
import com.waz.client.RegistrationClientImpl.ActivateResult
import com.waz.content.GlobalPreferences.{ActiveAccountPef, CurrentAccountPrefOld, DatabasesRenamed, FirstTimeWithTeams}
import com.waz.content.UserPreferences
import com.waz.model._
import com.waz.service.tracking.LoggedOutEvent
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, EventStream, RefreshingSignal, Signal}
import com.waz.utils.returning
import com.waz.znet.ZNetClient._

import scala.collection.mutable
import scala.concurrent.Future

trait AccountsService {
  import AccountsService._

  def accountState(userId: UserId): Signal[AccountState]

  def activeAccountId: Signal[Option[UserId]]
  def getActiveAccountId: Future[Option[UserId]]

  def loggedInAccounts: Signal[Set[AccountData]]
  def getLoggedInAccounts: Future[Set[AccountData]]

  def loggedInAccountIds: Signal[Set[UserId]] = loggedInAccounts.map(_.map(_.id))
  def getLoggedInAccountIds: Future[Set[UserId]] = getLoggedInAccounts.map(_.map(_.id))

  def activeAccount: Signal[Option[AccountData]]
  def getActiveAccount: Future[Option[AccountData]]

  def zmsInstances: Signal[Set[ZMessaging]]

  def activeZms: Signal[Option[ZMessaging]]
  def getActiveZms: Future[Option[ZMessaging]]

  def zms(userId: UserId): Signal[Option[ZMessaging]]
  def getZms(userId: UserId): Future[Option[ZMessaging]]

  //Set to None in order to go to the login screen without logging out the current users
  def setAccount(userId: Option[UserId]): Future[Unit]

  def logout(userId: UserId): Future[Unit]

  //TODO
  def login(loginCredentials: Credentials): ErrorOr[UserId]
  def register(registerCredentials: Credentials): ErrorOr[UserId]

}

object AccountsService {
  trait AccountState

  case object LoggedOut extends AccountState

  trait Active extends AccountState
  case object InBackground extends Active
  case object InForeground extends Active

  val NoEmailSetWarning = "Account does not have email set - can't request activation code"
}

class AccountsServiceImpl(val global: GlobalModule) extends AccountsService {

  import AccountsService._

  implicit val dispatcher = new SerialDispatchQueue(name = "InstanceService")

  private[waz] implicit val ec: EventContext = EventContext.Global

  private[waz] val accountMap = new mutable.HashMap[UserId, AccountManager]()

  val context       = global.context
  val prefs         = global.prefs
  val storage       = global.accountsStorage
  val storageOld    = global.accountsStorageOld
  val phoneNumbers  = global.phoneNumbers
  val regClient     = global.regClient
  val loginClient   = global.loginClient

  private val firstTimeWithTeamsPref = prefs.preference(FirstTimeWithTeams)
  private val databasesRenamedPref = prefs.preference(DatabasesRenamed)

  private val migrationDone = for {
    first   <- firstTimeWithTeamsPref.signal
    renamed <- databasesRenamedPref.signal
  } yield !first && renamed

  val activeAccountPref = prefs.preference(ActiveAccountPef)

  //TODO can be removed after a (very long) while
  private val migration = databasesRenamedPref().flatMap {
    case true => Future.successful({}) //databases have been renamed - nothing to do.
    case false =>
      for {
        active <- prefs.preference(CurrentAccountPrefOld).apply()
        accs <- storageOld.list()
        _    <- accs.foreach { acc =>
          Future.sequence(acc.userId.map { userId =>
            //migrate the databases
            verbose(s"Renaming database and cryptobox dir: ${acc.id.str} to ${userId.str}")

            val dbFileOld = context.getDatabasePath(acc.id.str)

            val exts = Seq("", "-wal", "-shm", "-journal")

            val toMove = exts.map(ext => s"${dbFileOld.getAbsolutePath}$ext").map(new File(_))

            val dbRenamed = exts.zip(toMove).map { case (ext, f) =>
              f.renameTo(new File(dbFileOld.getParent, s"${userId.str}$ext"))
            }.forall(identity)

            //migrate cryptobox dirs
            val cryptoBoxDirOld = new File(new File(context.getFilesDir, global.metadata.cryptoBoxDirName), acc.id.str)
            val cryptoBoxDirNew = new File(new File(context.getFilesDir, global.metadata.cryptoBoxDirName), userId.str)
            val cryptoBoxRenamed = cryptoBoxDirOld.renameTo(cryptoBoxDirNew)

            verbose(s"DB migration successful?: $dbRenamed, cryptobox migration successful?: $cryptoBoxRenamed")

            //Ensure that the current active account remains active
            if (active.contains(acc.id)) activeAccountPref := Some(userId) else Future.successful({})
          })
        }
        //copy the client ids
        _ <- Future.sequence(accs.collect { case acc if acc.userId.isDefined =>
          import com.waz.service.AccountManager.ClientRegistrationState._
          val state = (acc.clientId, acc.clientRegState) match {
            case (Some(id), _)           => Registered(id)
            case (_, "UNKNOWN")          => Unregistered
            case (_, "PASSWORD_MISSING") => PasswordMissing
            case (_, "LIMIT_REACHED")    => LimitReached
            case _                       =>
              error(s"Unknown client registration state: ${acc.clientId}, ${acc.clientRegState}. Defaulting to unregistered")
              Unregistered
          }

          global.factory.baseStorage(acc.userId.get).userPrefs.preference(UserPreferences.SelfClient) := state
        })
        //delete non-logged in accounts, or every account that's not the current if it's the first installation with teams
        _ <- firstTimeWithTeamsPref().map {
          case false => accs.collect { case acc if acc.cookie.isEmpty => acc.id }
          case true => accs.map(_.id).filterNot(active.contains)
        }.flatMap(storageOld.removeAll)
        //migration done! set the prefs so it doesn't happen again
        _ <- firstTimeWithTeamsPref := false
        _ <- databasesRenamedPref   := true
      } yield {}
  }

  storage.onDeleted(_.foreach { user =>
    global.trackingService.loggedOut(LoggedOutEvent.InvalidCredentials, user)
  })

  override def getLoggedInAccounts = storage.list().map(_.toSet)

  override val loggedInAccounts = migrationDone.flatMap {
    case true =>
      val changes = EventStream.union(
        storage.onChanged.map(_.map(_.id)),
        storage.onDeleted
      ).map(_.toSet)
      new RefreshingSignal[Set[AccountData], Set[UserId]](CancellableFuture.lift(getLoggedInAccounts), changes)
    case false => Signal.const(Set.empty[AccountData])
  }

  @volatile private var accountStateSignals = Map.empty[UserId, Signal[AccountState]]

  override def accountState(userId: UserId) = {

    lazy val newSignal: Signal[AccountState] = for {
      selected <- activeAccountPref.signal.map(_.contains(userId))
      loggedIn <- loggedInAccountIds.map(_.contains(userId))
      uiActive <- global.lifecycle.uiActive
    } yield
      returning(if (!loggedIn) LoggedOut else if (uiActive && selected) InForeground else InBackground) { state =>
        verbose(s"account state changed: $userId -> $state: selected: $selected, loggedIn: $loggedIn, uiActive: $uiActive")
      }

    accountStateSignals.getOrElse(userId, returning(newSignal) { sig =>
      accountStateSignals += userId -> sig
    })
  }

  override lazy val activeAccountId = activeAccountPref.signal

  override def getActiveAccountId = activeAccountPref()

  override lazy val activeAccount = activeAccountId.flatMap {
    case Some(id) => storage.optSignal(id)
    case None     => Signal.const(None)
  }

  lazy val activeAccountManager = activeAccountPref.signal.flatMap {
    case Some(id) => Signal.future(getOrCreateAccountManager(id).map(Some(_)))
    case None     => Signal.const(None)
  }

  lazy val activeZms = activeAccountManager.flatMap {
    case Some(am) => am.zmessaging
    case None     => Signal.const(None)
  }

  override def getActiveAccount = getActiveAccountId.flatMap {
    case Some(id) => storage.get(id)
    case None     => Future.successful(None)
  }

  def getActiveAccountManager = getActiveAccountId.flatMap {
    case Some(id) => getOrCreateAccountManager(id) map (Some(_))
    case _        => Future.successful(None)
  }

  override def getActiveZms = getActiveAccountManager.flatMap {
    case Some(acc) => acc.getZMessaging
    case None      => Future.successful(None)
  }

  private[service] def getOrCreateAccountManager(userId: UserId) = migration.map { _ =>
    verbose(s"getOrCreateAccountManager: $userId")
    accountMap.getOrElseUpdate(userId, new AccountManager(userId, global, this))
  }

  //TODO - why would we ever NOT want to create the account manager if there is a AccountId available for it?
  def getAccountManager(id: UserId, orElse: Option[AccountManager] = None): Future[Option[AccountManager]] = storage.get(id) flatMap {
    case Some(acc) =>
      verbose(s"getAccountManager($acc)")
      getOrCreateAccountManager(id) map (Some(_))
    case _ =>
      Future successful None
  }

  override lazy val zmsInstances = (for {
    ids <- loggedInAccounts.map(_.map(_.id))
    ams <- Signal.future(Future.sequence(ids.map(getOrCreateAccountManager)))
    zs  <- Signal.sequence(ams.map(_.zmessaging).toSeq: _*)
  } yield
    returning(zs.flatten.toSet) { v =>
      verbose(s"Loaded: ${v.size} zms instances for ${ids.size} accounts")
    }).disableAutowiring()

  override def zms(userId: UserId): Signal[Option[ZMessaging]] = zmsInstances.map(_.find(_.selfUserId == userId))

  override def getZms(userId: UserId): Future[Option[ZMessaging]] = getOrCreateAccountManager(userId).flatMap(_.getZMessaging)

  //TODO optional delete history
  def logout(userId: UserId) = {
    for {
      current       <- getActiveAccountId
      otherAccounts <- getLoggedInAccountIds.map(_.filter(userId != _))
      _ <- if (current.contains(userId)) setAccount(otherAccounts.headOption) else Future.successful(())
      _ <- storage.remove(userId) //TODO pass Id to some sort of clean up service before removing
    } yield {}
  }

  /**
    * Logs out of the current account and switches to another specified by the AccountId. If the other cannot be authorized
    * (no cookie) or if anything else goes wrong, we leave the user logged out
    */
  override def setAccount(userId: Option[UserId]) = {
    verbose(s"switchAccount: $userId")
    userId match {
      case Some(id) =>
        for {
          cur      <- getActiveAccountId
          if !cur.contains(id)
          account  <- storage.get(id)
          if account.isDefined
          _        <- activeAccountPref := userId
          _        <- getOrCreateAccountManager(id)
        } yield {}

      case None =>
        activeAccountPref := userId
    }
  }

  def requestVerificationEmail(email: EmailAddress): Unit = loginClient.requestVerificationEmail(email)

  def requestPhoneConfirmationCode(phone: PhoneNumber, kindOfAccess: KindOfAccess): CancellableFuture[ActivateResult] = {
    throw new Exception("Not yet implemented!") //TODO
//    CancellableFuture.lift(phoneNumbers.normalize(phone)) flatMap { normalizedPhone =>
//      regClient.requestPhoneConfirmationCode(normalizedPhone.getOrElse(phone), kindOfAccess)
//    }
  }

  def requestPhoneConfirmationCall(phone: PhoneNumber, kindOfAccess: KindOfAccess): CancellableFuture[ActivateResult] = {
    throw new Exception("Not yet implemented!") //TODO
//    CancellableFuture.lift(phoneNumbers.normalize(phone)) flatMap { normalizedPhone =>
//      regClient.requestPhoneConfirmationCall(normalizedPhone.getOrElse(phone), kindOfAccess)
//    }
  }

  def verifyPhoneNumber(phone: PhoneCredentials, kindOfVerification: KindOfVerification): ErrorOrResponse[Unit] = {
    CancellableFuture.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    CancellableFuture.lift(phoneNumbers.normalize(phone.phone)) flatMap { normalizedPhone =>
//      regClient.verifyPhoneNumber(PhoneCredentials(normalizedPhone.getOrElse(phone.phone), phone.code), kindOfVerification)
//    }
  }

  def loginPhone(number: PhoneNumber): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    def requestCode(): Future[Either[ErrorResponse, Unit]] =
//      requestPhoneConfirmationCode(number, KindOfAccess.LOGIN).future.map {
//        case Failure(error) => Left(error)
//        case PasswordExists => Left(ErrorResponse.PasswordExists)
//        case _ => Right(())
//      }
//
//    for {
//      normalizedPhone <- phoneNumbers.normalize(number).map(_.getOrElse(number))
//      acc <- storageOld.findByPhone(normalizedPhone).map(_.getOrElse(AccountDataOld()))
//      req <- requestCode()
//      updatedAcc  = acc.copy(pendingPhone = Some(normalizedPhone), accessToken = None, cookie = None, password = None, code = None, regWaiting = false)
//      _ <- if (req.isRight) storageOld.updateOrCreate(acc.id, _ => updatedAcc, updatedAcc).map(_ => ()) else Future.successful(())
//      _ <- if (req.isRight) setAccount(Some(updatedAcc.id)) else Future.successful(())
//    } yield req
  }

  def registerPhone(number: PhoneNumber): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    for {
//      normalizedPhone <- phoneNumbers.normalize(number).map(_.getOrElse(number))
//      acc <- storageOld.findByPhone(normalizedPhone).map(_.getOrElse(AccountDataOld()))
//      req <- requestPhoneConfirmationCode(number, KindOfAccess.REGISTRATION).future
//      updatedAcc = acc.copy(pendingPhone = Some(normalizedPhone), code = None, regWaiting = true)
//      _ <- if (req == ActivateResult.Success) storageOld.updateOrCreate(updatedAcc.id, _ => updatedAcc, updatedAcc) else Future.successful(())
//      _ <- if (req == ActivateResult.Success) CancellableFuture.lift(setAccount(Some(acc.id))) else CancellableFuture.successful(())
//    } yield req match {
//      case Failure(error) => Left(error)
//      case PasswordExists => Left(ErrorResponse.PasswordExists)
//      case _ => Right(())
//    }
  }

  def activatePhoneOnRegister(accountId: AccountId, code: ConfirmationCode): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    def verifyCodeRequest(credentials: PhoneCredentials, accountId: AccountId): Future[Either[ErrorResponse, Unit]] = {
//      verifyPhoneNumber(credentials, KindOfVerification.PREVERIFY_ON_REGISTRATION).future.flatMap {
//        case Left(errorResponse) =>
//          Future.successful(Left(errorResponse))
//        case Right(()) =>
//          storageOld.update(accountId, _.copy(phone = Some(credentials.phone), pendingPhone = None, code = Some(code), regWaiting = true)).map(_ => Right(()))
//      }
//    }
//
//    for {
//      Some(acc) <- storageOld.get(accountId)
//      Some(creds) <- acc.pendingPhone.fold2(Future.successful(None), phone => Future.successful(PhoneCredentials(phone, Some(code))).map(Option(_)))
//      req <- verifyCodeRequest(creds.asInstanceOf[PhoneCredentials], acc.id)
//    } yield req
  }

  def registerNameOnPhone(accountId: AccountId, name: String): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    for {
//      acc <- storageOld.get(accountId)
//      req <- acc.fold2(Future.successful(Left(ErrorResponse.InternalError)), accountData => registerOnBackend(accountData, name))
//    } yield req
  }

  def loginPhone(accountId: AccountId, code: ConfirmationCode): Future[Either[ErrorResponse, Unit]] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    for {
//      Some(acc) <- storageOld.get(accountId)
//      req <- loginOnBackend(acc.copy(code = Some(code)))
//      _ <- req match {
//        case Right(_) =>
//          storageOld.update(accountId, _.copy(phone = acc.pendingPhone, pendingPhone = None))
//        case Left(ErrorResponse(Status.Forbidden, _, "pending-activation")) =>
//          storageOld.update(accountId, _.copy(phone = None, pendingPhone = acc.pendingPhone)).map(_ => ())
//        case _ =>
//          Future.successful(())
//      }
//    } yield req
  }

  def loginEmail(emailAddress: EmailAddress, password: String): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    for {
//      acc <- storageOld.findByEmail(emailAddress).map(_.getOrElse(AccountDataOld()))
//      loginAcc = acc.copy(email = Some(emailAddress), password = Some(password))
//      _ <- storageOld.updateOrCreate(loginAcc.id, _ => loginAcc, loginAcc)
//      req <- loginOnBackend(loginAcc)
//      _ <- req match {
//        case Right (_) =>
//          switchAccount(acc.id)
//        case Left(ErrorResponse(Status.Forbidden, _, "pending-activation")) =>
//          storageOld.update(loginAcc.id, _.copy(pendingEmail = Some(emailAddress))).map(_ => ())
//        case _ => Future.successful(())
//      }
//    } yield req
  }

  def registerEmail(emailAddress: EmailAddress, password: String, name: String): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    for {
//      acc <- storageOld.findByEmail(emailAddress).map(_.getOrElse(AccountDataOld()))
//      registerAcc = acc.copy(pendingEmail = Some(emailAddress), password = Some(password), name = Some(name))
//      _ <- storageOld.updateOrCreate(registerAcc.id, _ => registerAcc, registerAcc)
//      req <- registerOnBackend(registerAcc, name)
//      _ <- if (req.isRight) storageOld.update(registerAcc.id, _.copy(email = None, pendingEmail = Some(emailAddress))) else Future.successful(())
//      _ <- if (req.isRight) switchAccount(registerAcc.id) else Future.successful(())
//    } yield req
  }

  def createTeamAccount(teamName: String): Future[AccountId] = {
    throw new Exception("Not yet implemented!") //TODO
//    for {
//      acc <- storageOld.findByPendingTeamName(teamName).flatMap {
//        case Some(a) => Future.successful(a)
//        case None    => storageOld.insert(AccountDataOld(pendingTeamName = Some(teamName)))
//      }
//      _   <- setAccount(Some(acc.id))
//    } yield acc.id
  }

  //For team flow only (for now) - applies to current active account
  def requestActivationCode(email: EmailAddress): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    regClient.requestEmailConfirmationCode(email).future.flatMap {
//      case ActivateResult.Success => updateCurrentAccount(_.copy(pendingEmail = Some(email))).map(_ => Right(()))
//      case ActivateResult.PasswordExists => Future.successful(Left(internalError("password exists for email activation - this shouldn't happen")))
//      case ActivateResult.Failure(err) => Future.successful(Left(err))
//    }
  }

  //For team flow only (for now) - applies to current active account
  def verify(code: ConfirmationCode): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO

//    withActiveAccount { acc =>
//      acc.pendingEmail match {
//        case Some(e) => for {
//          res <- regClient.verifyEmail(e, code).future
//          _   <- res match {
//            case Right(()) => updateCurrentAccount(_.copy(code = Some(code)))
//            case _ => Future.successful(())
//          }
//        } yield res
//        case _ => Future.successful(Left(internalError(s"Current account: ${acc.id} does not have a pending email address. First request an activation code and provide an email address")))
//      }
//    }
  }

  //For team flow only (for now) - applies to current active account
  def register(): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//
//    withActiveAccount { acc =>
//      regClient.registerTeamAccount(acc).future.flatMap {
//        case Right((userInfo, cookie)) =>
//          verbose(s"register($acc) done, id: ${acc.id}, user: $userInfo, cookie: $cookie")
//          storageOld.update(acc.id,
//            _.updated(userInfo).copy(
//              cookie          = cookie,
//              regWaiting      = false,
//              code            = None,
//              firstLogin      = false,
//              email           = acc.pendingEmail,
//              pendingEmail    = None
//            )).map(_ => Right(()))
//        case Left(err@ErrorResponse(Response.Status.NotFound, _, "invalid-code")) =>
//          info(s"register($acc.id) failed: invalid-code")
//          storageOld.update(acc.id, _.copy(code = None, password = None)).map(_ => Left(err))
//        case Left(error) =>
//          info(s"register($acc.id) failed: $error")
//          Future successful Left(error)
//    }}
  }

  private def loginOnBackend(accountData: AccountDataOld): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    loginClient.login(accountData).future.flatMap {
//      case Right((token, cookie)) =>
//        storageOld.update(accountData.id, _.copy(accessToken = Some(token), cookie = cookie, code = None)).map(_ => Right(()))
//      case Left((_, error @ ErrorResponse(Status.Forbidden, _, "pending-activation"))) =>
//        verbose(s"account pending activation: ($accountData), $error")
//        storageOld.update(accountData.id, _.copy(cookie = None, accessToken = None, code = None)).map(_ => Left(error))
//      case Left((_, error)) =>
//        verbose(s"login failed: $error")
//        storageOld.update(accountData.id, _.copy(cookie = None, accessToken = None, code = None)).map(_ => Left(error))
//    }
  }

  private def registerOnBackend(accountData: AccountDataOld, name: String): ErrorOr[Unit] = {
    Future.successful(Left(ErrorResponse.internalError("Not yet implemented!!"))) //TODO
//    regClient.register(accountData, name, None).future.flatMap {
//      case Right((userInfo, Some(cookie))) =>
//        verbose(s"register($accountData) done, id: ${accountData.id}, user: $userInfo, cookie: $cookie")
//        storageOld.update(accountData.id, _.updated(userInfo).copy(cookie = Some(cookie), regWaiting = false, name = Some(name), code = None, firstLogin = false)).map(_ => Right(()))
//      case Right((userInfo, None)) =>
//        verbose(s"register($accountData) done, id: ${accountData.id}, user: $userInfo")
//        storageOld.update(accountData.id, _.updated(userInfo).copy(cookie = None, regWaiting = false,  name = Some(name), code = None, firstLogin = false)).map(_ => Right(()))
//      case Left(error) =>
//        info(s"register($accountData, $name) failed: $error")
//        Future successful Left(error)
//    }
  }

  def setLoggedIn(accountId: AccountId): Future[Unit] = {
    throw new Exception("Not yet implemented!") //TODO
//    storageOld.update(accountId, _.copy(firstLogin = false)).map(_ => ())
  }

}
