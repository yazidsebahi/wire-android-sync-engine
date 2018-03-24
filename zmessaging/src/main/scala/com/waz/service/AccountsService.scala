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
import com.waz.api.{ErrorResponse => _, _}
import com.waz.client.RegistrationClientImpl.ActivateResult
import com.waz.content.GlobalPreferences.{ActiveAccountPef, CurrentAccountPrefOld, DatabasesRenamed, FirstTimeWithTeams}
import com.waz.content.UserPreferences
import com.waz.model._
import com.waz.service.tracking.LoggedOutEvent
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.{EventContext, EventStream, RefreshingSignal, Signal}
import com.waz.utils.{RichOption, returning}
import com.waz.znet.AuthenticationManager.{AccessToken, Cookie}
import com.waz.znet.ZNetClient._

import scala.collection.immutable.HashMap
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

  def requestVerificationEmail(email: EmailAddress): ErrorOr[Unit]

  def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean = false): Future[ActivateResult]
  def requestEmailCode(email: EmailAddress): Future[ActivateResult]

  def verifyPhoneNumber(phone: PhoneNumber, code: ConfirmationCode, dryRun: Boolean): ErrorOr[Unit]
  def verifyEmailAddress(email: EmailAddress, code: ConfirmationCode, dryRun: Boolean = true): ErrorOr[Unit]

  def login(loginCredentials: Credentials): ErrorOr[Unit]
  def register(registerCredentials: Credentials, name: String, teamName: Option[String] = None): ErrorOr[Unit]

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
  implicit val ec: EventContext = EventContext.Global

  @volatile private var accountMap = HashMap[UserId, AccountManager]()

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
    case Some(id) => Signal.future(getOrCreateAccountManager(id))
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
    case Some(id) => getOrCreateAccountManager(id)
    case _        => Future.successful(None)
  }

  override def getActiveZms = getActiveAccountManager.flatMap {
    case Some(acc) => acc.getZMessaging
    case None      => Future.successful(None)
  }

  private def getOrCreateAccountManager(userId: UserId): Future[Option[AccountManager]] = {
    verbose(s"getOrCreateAccountManager: $userId")
    accountMap.get(userId) match {
      case Some(am) => Future.successful(Some(am))
      case _ =>
        storage.get(userId).map {
          case Some(_) =>
            Some(returning(new AccountManager(userId, global, this))(am => accountMap += (userId -> am)))
          case _ =>
            warn(s"No logged in account for user: $userId, not creating account manager")
            None
        }
    }
  }

  override lazy val zmsInstances = (for {
    ids <- loggedInAccounts.map(_.map(_.id))
    ams <- Signal.future(Future.sequence(ids.map(getOrCreateAccountManager)))
    zs  <- Signal.sequence(ams.flatten.map(_.zmessaging).toSeq: _*)
  } yield
    returning(zs.flatten.toSet) { v =>
      verbose(s"Loaded: ${v.size} zms instances for ${ids.size} accounts")
    }).disableAutowiring()

  override def zms(userId: UserId): Signal[Option[ZMessaging]] =
    zmsInstances.map(_.find(_.selfUserId == userId))

  override def getZms(userId: UserId): Future[Option[ZMessaging]] =
    getOrCreateAccountManager(userId).flatMap(_.fold2(Future.successful(Option.empty[ZMessaging]), _.getZMessaging))

  //TODO optional delete history
  def logout(userId: UserId) = {
    verbose(s"logout: $userId")
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
    verbose(s"setAccount: $userId")
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

  def requestVerificationEmail(email: EmailAddress) =
    regClient.requestVerificationEmail(email)

  override def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean = false) = {
    verbose(s"requestPhoneConfirmationCode: $phone, login=$login, call=$call")
    phoneNumbers.normalize(phone).flatMap { normalizedPhone =>
      regClient.requestPhoneCode(normalizedPhone.getOrElse(phone), login, call)
    }
  }

  override def requestEmailCode(email: EmailAddress) = {
    verbose(s"requestEmailConfirmationCode: $email")
    regClient.requestEmailCode(email) //TODO should this email address be normalised?
  }

  override def verifyPhoneNumber(phone: PhoneNumber, code: ConfirmationCode, dryRun: Boolean) = {
    verbose(s"verifyPhoneNumber: $phone, $code, $dryRun")
    phoneNumbers.normalize(phone).flatMap { normalizedPhone =>
      regClient.verifyRegistrationMethod(Left(normalizedPhone.getOrElse(phone)), code, dryRun).map(_.fold(Left(_), _ => Right({}))) //TODO handle label and cookie!
    }
  }

  override def verifyEmailAddress(email: EmailAddress, code: ConfirmationCode, dryRun: Boolean = true) = {
    verbose(s"verifyEmailAddress: $email, $code, $dryRun")
    //TODO normalise email?
    regClient.verifyRegistrationMethod(Right(email), code, dryRun).map(_.fold(Left(_), _ => Right({}))) //TODO handle label and cookie!
  }

  override def login(loginCredentials: Credentials) = {
    verbose(s"login: $loginCredentials")
    loginClient.login(loginCredentials).future.flatMap {
      case Right((token, Some(cookie), _)) => //TODO handle label
        for {
          resp <- loginClient.getSelfUserInfo(token)
          _    <- resp.fold(_ => Future.successful({}), u => createAndEnterAccount(u, cookie, Some(token), loginCredentials))
        } yield resp.fold(Left(_), _ => Right({}))
      case Right(_) =>
        warn("login didn't return with a cookie, aborting")
        Future.successful(Left(ErrorResponse.internalError("No cookie for user after login - can't create account")))
      case Left(error) =>
        verbose(s"login failed: $error")
        Future.successful(Left(error))
    }
  }

  override def register(registerCredentials: Credentials, name: String, teamName: Option[String] = None) = {
    verbose(s"register: $registerCredentials, name: $name, teamName: $teamName")
    regClient.register(registerCredentials, name, teamName).flatMap {
      case Right((user, Some((cookie, _)))) =>
        createAndEnterAccount(user, cookie, None, registerCredentials).map(Right(_))
      case Right(_) =>
        warn("Register didn't return a cookie")
        Future.successful(Left(ErrorResponse.internalError("No cookie for user after registration - can't create account")))
      case Left(error) =>
        verbose(s"register failed: $error")
        Future.successful(Left(error))
    }
  }

  private def createAndEnterAccount(user: UserInfo, cookie: Cookie, token: Option[AccessToken], credentials: Credentials): Future[Unit] = {
    verbose(s"createAndEnterAccount: $user, $cookie, $token, $credentials")
    for {
      _ <- storage.updateOrCreate(user.id, _.copy(cookie = cookie, accessToken = token), AccountData(user.id, cookie, token))
      _ =  accountMap += (user.id -> new AccountManager(user.id, global, this, Some(credentials), Some(user)))
      _ <- setAccount(Some(user.id))
    } yield {}
  }
}

