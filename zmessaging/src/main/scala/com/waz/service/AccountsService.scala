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
import com.waz.api.impl._
import com.waz.api.{KindOfAccess, KindOfVerification}
import com.waz.client.RegistrationClient.ActivateResult
import com.waz.content.GlobalPreferences.{CurrentAccountPref, FirstTimeWithTeams}
import com.waz.model._
import com.waz.service.AccountsService.SwapAccountCallback
import com.waz.threading.{CancellableFuture, SerialDispatchQueue, Threading}
import com.waz.utils.events.{EventContext, EventStream, RefreshingSignal, Signal}
import com.waz.znet.Response.Status
import com.waz.znet.ZNetClient._

import scala.collection.mutable
import scala.concurrent.Future

class AccountsService(val global: GlobalModule) {

  implicit val dispatcher = new SerialDispatchQueue(name = "InstanceService")

  private[waz] implicit val ec: EventContext = EventContext.Global

  private[waz] val accountMap = new mutable.HashMap[AccountId, AccountManager]()

  val context       = global.context
  val prefs         = global.prefs
  val storage       = global.accountsStorage
  val phoneNumbers  = global.phoneNumbers
  val regClient     = global.regClient
  val loginClient   = global.loginClient

  private val firstTimePref = prefs.preference(FirstTimeWithTeams)

  val loggedInAccounts = firstTimePref.signal.flatMap {
    case false =>
      val changes = EventStream.union(
        storage.onChanged.map(_.map(_.id)),
        storage.onDeleted
      )
      new RefreshingSignal[Seq[AccountData], Seq[AccountId]](CancellableFuture.lift(storage.list()), changes)
    case true => Signal.const(Seq.empty[AccountData])
  }.map(_.filter(acc => acc.cookie.isDefined && acc.clientId.isDefined))

  // XXX Temporary stuff to handle team account in signup/signin - start
  private var _loggedInAccounts = Seq.empty[AccountData]

  loggedInAccounts { accs =>
    verbose(s"Logged in accounts: ${accs.map(_.id)}")
    _loggedInAccounts = accs
  }

  def getLoggedInAccounts = _loggedInAccounts

  def hasLoggedInAccount = _loggedInAccounts.nonEmpty

  def fallbackToLastAccount(callback: SwapAccountCallback) =
    if (_loggedInAccounts.nonEmpty)
      switchAccount(_loggedInAccounts.head.id).map(_ => callback.onSwapComplete())(Threading.Ui)
    else callback.onSwapFailed()
  // XXX Temporary stuff to handle team account in signup/signin - end

  val activeAccountPref = prefs.preference(CurrentAccountPref)
  activeAccountPref.signal(ac => verbose(s"Active account: $ac"))

  lazy val activeAccount = activeAccountPref.signal.flatMap[Option[AccountData]] {
    case None     => Signal.const(None)
    case Some(id) => storage.optSignal(id)
  }

  lazy val activeAccountManager = activeAccountPref.signal.flatMap[Option[AccountManager]] {
    case None     => Signal.const(None)
    case Some(id) => Signal.future(getOrCreateAccountManager(id).map(Some(_)))
  }

  lazy val activeZms = activeAccountManager.flatMap[Option[ZMessaging]] {
    case Some(service) => service.zmessaging
    case None          => Signal.const(None)
  }

  def getActiveAccount = activeAccountPref() flatMap {
    case None     => Future successful None
    case Some(id) => storage.get(id)
  }

  def getActiveAccountManager = activeAccountPref() flatMap {
    case Some(id) => getOrCreateAccountManager(id) map (Some(_))
    case _        => Future successful None
  }

  def getActiveZms = getActiveAccountManager.flatMap {
    case Some(acc) => acc.getZMessaging
    case None      => Future successful None
  }

  private[service] def getOrCreateAccountManager(accountId: AccountId) = flushOtherCredentials.map { _ =>
    verbose(s"getOrCreateAccountManager: $accountId")
    accountMap.getOrElseUpdate(accountId, new AccountManager(accountId, global, this))
  }

  def getAccountManager(id: AccountId, orElse: Option[AccountManager] = None): Future[Option[AccountManager]] = storage.get(id) flatMap {
    case Some(acc) =>
      verbose(s"getAccountManager($acc)")
      getOrCreateAccountManager(id) map (Some(_))
    case _ =>
      Future successful None
  }

  def logout(flushCredentials: Boolean) = activeAccountManager.head flatMap {
    case Some(account) => account.logout(flushCredentials)
    case None          => Future.successful(())
  }

  def logout(account: AccountId, flushCredentials: Boolean) = {
    activeAccountPref() flatMap { id =>
        for {
          otherAccounts <- loggedInAccounts.map(_.map(_.id).filter(!id.contains(_))).head
          _ <- if (flushCredentials) storage.update(account, _.copy(accessToken = None, cookie = None, password = None, registeredPush = None)) else storage.update(account, _.copy(registeredPush = None))
          _ <- if (id.contains(account)) setAccount(if (flushCredentials) otherAccounts.headOption else None) else Future.successful(())
        } yield {}
    }
  }

  private def setAccount(acc: Option[AccountId]) = {
    verbose(s"setAccount($acc)")
    activeAccountPref := acc
  }

  /**
    * Logs out of the current account and switches to another specified by the AccountId. If the other cannot be authorized
    * (no cookie) or if anything else goes wrong, we leave the user logged out
    */
  def switchAccount(accountId: AccountId) = {
    verbose(s"switchAccount: $accountId")
    for {
      cur      <- getActiveAccountManager.map(_.map(_.id))
      if !cur.contains(accountId)
      _        <- logout(flushCredentials = false)
      account  <- storage.get(accountId)
      if account.exists { acc =>
        acc.cookie.exists(_.isValid) || (acc.email.isDefined && acc.password.isDefined)
      }
      _        <- setAccount(Some(accountId))
      _        <- getOrCreateAccountManager(accountId)
    } yield {}
  }

  private def switchAccount(credentials: Credentials) = {
    verbose(s"switchAccount($credentials)")
    for {
      _          <- logout(flushCredentials = false)
      normalized <- normalizeCredentials(credentials)
      matching   <- storage.find(normalized)
      accountId  =  matching.flatMap(_.authorized(normalized)).map(_.id)
      _          <- setAccount(accountId)
      manager    <- accountId.fold(Future successful Option.empty[AccountManager]) { id => getOrCreateAccountManager(id).map(Some(_)) }
    } yield
      (normalized, matching, manager)
  }


  def login(credentials: Credentials): Future[Either[ErrorResponse, AccountData]] =
    switchAccount(credentials) flatMap {
      case (normalized, _, Some(manager)) => manager.login(normalized)
      case (normalized, Some(account), None) => // found matching account, but is not authorized (wrong password)
        verbose(s"found matching account: $account, trying to authorize with backend")
        login(account, normalized)
      case (normalized, None, None) =>
        verbose(s"matching account not found, creating new account")
        login(AccountData(), normalized)
    }

  private def login(account: AccountData, normalized: Credentials) = {
    def loginOnBackend() =
      loginClient.login(account.id, normalized).future map {
        case Right((token, c)) =>
          Right(account.updated(normalized).copy(cookie = c, verified = true, accessToken = Some(token)))
        case Left((_, error @ ErrorResponse(Status.Forbidden, _, "pending-activation"))) =>
          verbose(s"account pending activation: $normalized, $error")
          Right(account.updated(normalized).copy(verified = false, cookie = None, accessToken = None))
        case Left((_, error)) =>
          verbose(s"login failed: $error")
          Left(error)
      }

    loginOnBackend() flatMap {
      case Right(a) =>
        for {
          acc     <- storage.updateOrCreate(a.id, _.updated(normalized).copy(cookie = a.cookie, verified = true, accessToken = a.accessToken), a)
          manager <- getOrCreateAccountManager(a.id)
          _       <- setAccount(Some(a.id))
          res     <- manager.login(normalized)
        } yield res
      case Left(err) =>
        Future successful Left(err)
    }
  }

  def requestVerificationEmail(email: EmailAddress): Unit = loginClient.requestVerificationEmail(email)

  def requestPhoneConfirmationCode(phone: PhoneNumber, kindOfAccess: KindOfAccess): CancellableFuture[ActivateResult] =
    CancellableFuture.lift(phoneNumbers.normalize(phone)) flatMap { normalizedPhone =>
      regClient.requestPhoneConfirmationCode(normalizedPhone.getOrElse(phone), kindOfAccess)
    }

  def requestPhoneConfirmationCall(phone: PhoneNumber, kindOfAccess: KindOfAccess): CancellableFuture[ActivateResult] =
    CancellableFuture.lift(phoneNumbers.normalize(phone)) flatMap { normalizedPhone =>
      regClient.requestPhoneConfirmationCall(normalizedPhone.getOrElse(phone), kindOfAccess)
    }

  def verifyPhoneNumber(phone: PhoneCredentials, kindOfVerification: KindOfVerification): ErrorOrResponse[Unit] =
    CancellableFuture.lift(phoneNumbers.normalize(phone.phone)) flatMap { normalizedPhone =>
      regClient.verifyPhoneNumber(PhoneCredentials(normalizedPhone.getOrElse(phone.phone), phone.code), kindOfVerification)
    }

  private def normalizeCredentials(credentials: Credentials): Future[Credentials] = credentials match {
    case cs @ PhoneCredentials(p, _, _) =>
      phoneNumbers.normalize(p) map { normalized => cs.copy(phone = normalized.getOrElse(p)) }
    case other =>
      Future successful other
  }

  def register(credentials: Credentials, name: String, accent: AccentColor): Future[Either[ErrorResponse, AccountData]] = {
    debug(s"register($credentials, $name, $accent")

    def register(accountId: AccountId, normalized: Credentials) =
      regClient.register(accountId, normalized, name, Some(accent.id)).future flatMap {
        case Right((userInfo, cookie)) =>
          verbose(s"register($credentials) done, id: $accountId, user: $userInfo, cookie: $cookie")
          for {
            acc     <- storage.insert(AccountData(accountId, normalized).copy(cookie = cookie, userId = Some(userInfo.id), verified = normalized.autoLoginOnRegistration))
            _       = verbose(s"created account: $acc")
            manager <- getOrCreateAccountManager(accountId)
            _       <- setAccount(Some(accountId))
            res     <- manager.login(normalized)
          } yield res
        case Left(error) =>
          info(s"register($credentials, $name) failed: $error")
          Future successful Left(error)
      }

    switchAccount(credentials) flatMap {
      case (normalized, _, Some(manager)) =>
        verbose(s"register($credentials), found matching account: $manager, will just sign in")
        manager.login(normalized)
      case (normalized, Some(account), None) =>
        verbose(s"register($credentials), found matching account: $account, will try signing in")
        login(account, normalized) flatMap {
          case Right(acc) => Future successful Right(acc)
          case Left(_) =>
            // login failed, maybe this account has been removed on backend, let's try registering
            register(account.id, normalized)
        }
      case (normalized, None, None) =>
        register(AccountId(), normalized)
    }
  }

  //TODO can be removed after a while
  private val flushOtherCredentials = {
    firstTimePref().flatMap {
      case false => Future.successful({})
      case true  =>
        for {
          cur <- activeAccountPref()
          accs <- storage.list()
          _ <- {
            val withoutCurrent = accs.map(_.id).filterNot(cur.contains)
            verbose(s"Flushing accounts: curr: $cur, others: $withoutCurrent")
            storage.updateAll2(withoutCurrent, _.copy(cookie = None, accessToken = None, password = None, registeredPush = None))
          }
          _ <- firstTimePref.update(false)
        } yield {}
    }
  }
}

object AccountsService {
  trait SwapAccountCallback {
    def onSwapComplete(): Unit
    def onSwapFailed(): Unit
  }
}
