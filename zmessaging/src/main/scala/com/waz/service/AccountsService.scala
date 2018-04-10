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
import com.waz.model.AccountData.{Label, Password}
import com.waz.model._
import com.waz.service.tracking.LoggedOutEvent
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.{RichOption, Serialized, returning}
import com.waz.znet.AuthenticationManager.{AccessToken, Cookie}
import com.waz.znet.LoginClient
import com.waz.znet.ZNetClient._

import scala.concurrent.Future
import scala.util.Right
import scala.util.control.NonFatal

/**
  * There are a few possible states that an account can progress through for the purposes of log in and registration.
  *
  * No state - an account is not known to sync engine
  *
  * Logged in (global db row)  - the account has a cookie and token and can authenticate requests - it will be persisted,
  *   but logged in accounts alone are not visible externally to this service.
  *
  * With AccountManager - the account has a database as well as being logged in. Here, we can start registering clients
  *
  * With ZMessaging - A ready, working account with database, client and logged in.
  *
  * Active - the current selected account, this state is independent to the other states, except that the account in question
  *   must have an account manager
  *
  */
trait AccountsService {
  import AccountsService._

  def requestVerificationEmail(email: EmailAddress): ErrorOr[Unit]

  def requestPhoneCode(phone: PhoneNumber, login: Boolean, call: Boolean = false): Future[ActivateResult]
  def requestEmailCode(email: EmailAddress): Future[ActivateResult]

  def verifyPhoneNumber(phone: PhoneNumber, code: ConfirmationCode, dryRun: Boolean): ErrorOr[Unit]
  def verifyEmailAddress(email: EmailAddress, code: ConfirmationCode, dryRun: Boolean = true): ErrorOr[Unit]

  def loginEmail(validEmail: String, validPassword: String): ErrorOr[UserId] = login(EmailCredentials(EmailAddress(validEmail), Password(validPassword)))
  def loginPhone(phone: String, code: String) = login(PhoneCredentials(PhoneNumber(phone), ConfirmationCode(code)))
  def login(loginCredentials: Credentials): ErrorOr[UserId]

  def register(registerCredentials: Credentials, name: String, teamName: Option[String] = None): ErrorOr[Unit]

  def hasDatabase(userId: UserId): Future[Boolean] = ???
  def createAccountManager(userId: UserId, dbFile: Option[File]): Future[Option[AccountManager]] //TODO return error codes on failure?

  //Set to None in order to go to the login screen without logging out the current users
  def setAccount(userId: Option[UserId]): Future[Unit]

  def logout(userId: UserId): Future[Unit]


  def accountManagers: Signal[Set[AccountManager]]
  def accountsWithManagers: Signal[Set[UserId]] = accountManagers.map(_.map(_.userId))
  def zmsInstances: Signal[Set[ZMessaging]]
  def getZms(userId: UserId): Future[Option[ZMessaging]]

  def accountState(userId: UserId): Signal[AccountState]

  def activeAccountId:      Signal[Option[UserId]]
  def activeAccount:        Signal[Option[AccountData]]
  def activeAccountManager: Signal[Option[AccountManager]]
  def activeZms:            Signal[Option[ZMessaging]]

  def loginClient: LoginClient
}

object AccountsService {

  val AccountManagersKey = "accounts-map"

  trait AccountState

  case object LoggedOut extends AccountState

  trait Active extends AccountState
  case object InBackground extends Active
  case object InForeground extends Active
}

class AccountsServiceImpl(val global: GlobalModule) extends AccountsService {
  import AccountsService._
  import Threading.Implicits.Background

  implicit val ec: EventContext = EventContext.Global

  val context       = global.context
  val prefs         = global.prefs
  val storageOld    = global.accountsStorageOld
  val phoneNumbers  = global.phoneNumbers
  val regClient     = global.regClient
  val loginClient   = global.loginClient

  private val activeAccountPref      = prefs(ActiveAccountPef)
  private val firstTimeWithTeamsPref = prefs(FirstTimeWithTeams)
  private val databasesRenamedPref   = prefs(DatabasesRenamed)

  private val migrationDone = for {
    first   <- firstTimeWithTeamsPref.signal
    renamed <- databasesRenamedPref.signal
  } yield !first && renamed

  private val storage = migrationDone.head.map(_ => global.accountsStorage)

  //TODO can be removed after a (very long) while
  databasesRenamedPref().flatMap {
    case true => Future.successful({}) //databases have been renamed - nothing to do.
    case false =>
      for {
        active <- prefs.preference(CurrentAccountPrefOld).apply()
        accs <- storageOld.list()
        _ <- Future.sequence(accs.filter(_.userId.isDefined).map { acc =>
          val userId = acc.userId.get
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
        //copy the client ids
        _ <- Future.sequence(accs.collect { case acc if acc.userId.isDefined =>
          import com.waz.service.AccountManager.ClientRegistrationState._
          val state = (acc.clientId, acc.clientRegState) match {
            case (Some(id), _) => Registered(id)
            case (_, "UNKNOWN") => Unregistered
            case (_, "PASSWORD_MISSING") => PasswordMissing
            case (_, "LIMIT_REACHED") => LimitReached
            case _ =>
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
        _ <- markMigrationDone()
      } yield {}
  }.recoverWith {
    case NonFatal(e) =>
      warn("Failed to migrate databases, aborting operation", e)
      markMigrationDone()
  }

  private def markMigrationDone() =
    for {
      _ <- firstTimeWithTeamsPref := false
      _ <- databasesRenamedPref   := true
    } yield {}


  storage.map(_.onDeleted(_.foreach { user =>
    verbose(s"user logged out: $user")
    global.trackingService.loggedOut(LoggedOutEvent.InvalidCredentials, user)
    Serialized.future(AccountManagersKey)(Future[Unit](accountManagers.mutate(_.filterNot(_.userId == user))))
  }))

  override val accountManagers = Signal[Set[AccountManager]]()

  //create account managers for all logged in accounts on app start, or initialise the signal to an empty set
  for {
    ids <- storage.flatMap(_.list().map(_.map(_.id).toSet))
    _   <- Future.sequence(ids.map(createAccountManager(_, None)))
  } yield Serialized.future(AccountManagersKey)(Future[Unit](accountManagers.mutateOrDefault(identity, Set.empty[AccountManager])))

  override def createAccountManager(userId: UserId, importDbFile: Option[File]) = {
    //TODO import dbFile and create ZMessagingDb
    Serialized.future(AccountManagersKey) {
      verbose(s"createAccountManager: $userId")
      accountManagers.orElse(Signal.const(Set.empty[AccountManager])).head.map(_.find(_.userId == userId)).flatMap {
        case Some(am) =>
          warn(s"AccountManager for: $userId already created")
          Future.successful(Some(am))
        case _ =>
          verbose(s"No AccountManager for: $userId, creating new one")
          storage.flatMap(_.get(userId).map {
            case Some(acc) =>
              Some(returning(new AccountManager(userId, acc.teamId, global, this))(am => accountManagers.mutateOrDefault(_ + am, Set(am))))
            case _ =>
              warn(s"No logged in account for user: $userId, not creating account manager")
              None
          })
      }
    }
  }

  @volatile private var accountStateSignals = Map.empty[UserId, Signal[AccountState]]
  override def accountState(userId: UserId) = {

    lazy val newSignal: Signal[AccountState] =
      for {
        selected <- activeAccountPref.signal.map(_.contains(userId))
        loggedIn <- accountsWithManagers.map(_.contains(userId))
        uiActive <- global.lifecycle.uiActive
      } yield {
        returning(if (!loggedIn) LoggedOut else if (uiActive && selected) InForeground else InBackground) { state =>
          verbose(s"account state changed: $userId -> $state: selected: $selected, loggedIn: $loggedIn, uiActive: $uiActive")
        }
      }

    accountStateSignals.getOrElse(userId, returning(newSignal) { sig =>
      accountStateSignals += userId -> sig
    })
  }

  override lazy val activeAccountManager = activeAccountPref.signal.flatMap[Option[AccountManager]] {
    case Some(id) => accountManagers.map(_.find(_.userId == id))
    case None     => Signal.const(None)
  }

  override lazy val activeAccount = activeAccountManager.flatMap[Option[AccountData]] {
    case Some(am) => Signal.future(storage).flatMap(_.optSignal(am.userId))
    case None     => Signal.const(None)
  }

  override lazy val activeAccountId = activeAccount.map(_.map(_.id))

  override lazy val activeZms = activeAccountManager.flatMap[Option[ZMessaging]] {
    case Some(am) => Signal.future(am.zmessaging.map(Some(_)))
    case None     => Signal.const(None)
  }

  override lazy val zmsInstances = (for {
    ams <- accountManagers
    zs  <- Signal.sequence(ams.map(am => Signal.future(am.zmessaging)).toSeq: _*)
  } yield
    returning(zs.toSet) { v =>
      verbose(s"Loaded: ${v.size} zms instances for ${ams.size} accounts")
    }).disableAutowiring()

  override def getZms(userId: UserId): Future[Option[ZMessaging]] = {
    verbose(s"getZms: $userId")
    zmsInstances.head.map(_.find(_.selfUserId == userId))
  }

  //TODO optional delete history
  def logout(userId: UserId) = {
    verbose(s"logout: $userId")
    for {
      current       <- activeAccountId.head
      otherAccounts <- accountsWithManagers.head.map(_.filter(userId != _))
      _ <- if (current.contains(userId)) setAccount(otherAccounts.headOption) else Future.successful(())
      _ <- storage.flatMap(_.remove(userId)) //TODO pass Id to some sort of clean up service before removing
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
        activeAccountId.head.flatMap {
          case Some(cur) if cur == id => Future.successful({})
          case Some(_)   => accountManagers.head.map(_.find(_.userId == id)).flatMap {
            case Some(_) => activeAccountPref := Some(id)
            case _ =>
              warn(s"Tried to set active user who is not logged in: $userId, not changing account")
              Future.successful({})
          }
          case _ => activeAccountPref := Some(id)
        }
      case None => activeAccountPref := None
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
    regClient.requestEmailCode(email)
  }

  override def verifyPhoneNumber(phone: PhoneNumber, code: ConfirmationCode, dryRun: Boolean) = {
    verbose(s"verifyPhoneNumber: $phone, $code, $dryRun")
    phoneNumbers.normalize(phone).flatMap { normalizedPhone =>
      regClient.verifyRegistrationMethod(Left(normalizedPhone.getOrElse(phone)), code, dryRun).map(_.fold(Left(_), _ => Right({}))) //TODO handle label and cookie!
    }
  }

  override def verifyEmailAddress(email: EmailAddress, code: ConfirmationCode, dryRun: Boolean = true) = {
    verbose(s"verifyEmailAddress: $email, $code, $dryRun")
    regClient.verifyRegistrationMethod(Right(email), code, dryRun).map(_.fold(Left(_), _ => Right({}))) //TODO handle label and cookie!
  }

  override def login(loginCredentials: Credentials) = {
    verbose(s"login: $loginCredentials")
    loginClient.login(loginCredentials).future.flatMap {
      case Right((token, Some(cookie), _)) => //TODO handle label
        loginClient.getSelfUserInfo(token).flatMap {
          case Right(user) =>
            addAccountEntry(user, cookie, Some(token), loginCredentials)
              .map(_.right.map(_ => user.id))
          case Left(err) => Future.successful(Left(err))
        }
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
        for {
          resp <- addAccountEntry(user, cookie, None, registerCredentials)
          _    <- resp.fold(_ => Future.successful({}), _ => createAccountManager(user.id, None).flatMap(_ => setAccount(Some(user.id))))
        } yield resp
      case Right(_) =>
        warn("Register didn't return a cookie")
        Future.successful(Left(ErrorResponse.internalError("No cookie for user after registration - can't create account")))
      case Left(error) =>
        verbose(s"register failed: $error")
        Future.successful(Left(error))
    }
  }

  private def addAccountEntry(user: UserInfo, cookie: Cookie, token: Option[AccessToken], credentials: Credentials): ErrorOr[Unit] = {
    verbose(s"addAccountEntry: $user, $cookie, $token, $credentials")

    def getTeam(accessToken: AccessToken): ErrorOr[Option[TeamId]] = {
      loginClient.findSelfTeam(accessToken).flatMap {
        case Right(teamOpt) =>
          (teamOpt match {
            case Some(t) => global.teamsStorage.updateOrCreate(t.id, _ => t, t)
            case _ => Future.successful({})
          }).map(_ => Right(teamOpt.map(_.id)))
        case Left(err) =>
          warn(s"Failed to update team information: ${err.message}")
          Future.successful(Left(err))
      }
    }

    for {
      cur  <- storage.flatMap(_.get(user.id))
      resp <- cur match {
        case Some(_) =>
          verbose(s"There is already an account entry for ${user.id}")
          storage
            .flatMap(_.update(user.id, _.copy(cookie = cookie, accessToken = token, password = credentials.maybePassword)))
            .map(_ => Right({}))
        case None =>
          verbose("No account entry, getting access token if necessary and checking team")
          for {
            tokenResp <- token.fold2(loginClient.access(cookie, None).future, at => Future.successful(Right((at, Some(cookie), Option.empty[Label])))).map(_.right.map(_._1))
            teamResp <- tokenResp match {
              case Right(access) => getTeam(access).flatMap {
                case Right(tId) =>
                  storage
                    .flatMap(_.insert(AccountData(user.id, tId, cookie, Some(access), password = credentials.maybePassword)))
                    .map(_ => Right({}))
                case Left(err) => Future.successful(Left(err))
              }
              case Left(err) => Future.successful(Left(err))
            }
          } yield teamResp
      }
    } yield resp
  }
}

