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
package com.waz.api.impl

import android.content.Context
import android.net.Uri
import com.waz.ZLog._
import com.waz.api
import com.waz.api.PermissionProvider
import com.waz.api.ZMessagingApi.{PhoneConfirmationCodeRequestListener, PhoneNumberVerificationListener, RegistrationListener}
import com.waz.api.impl.search.Search
import com.waz.client.RegistrationClient.ActivateResult
import com.waz.content.Uris
import com.waz.media.manager.MediaManager
import com.waz.model._
import com.waz.service.AccountService
import com.waz.threading.Threading
import com.waz.ui.UiModule
import com.waz.utils.events.EventContext

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class ZMessagingApi(implicit val ui: UiModule) extends com.waz.api.ZMessagingApi {

  import Threading.Implicits.Ui
  private implicit val logTag: LogTag = logTagFor[ZMessagingApi]

  private[waz] var account: Option[AccountService] = None
  private[waz] def zmessaging = account match {
    case Some(acc) => acc.getZMessaging
    case None => Future successful None
  }

  private var context: Context = _
  private var resumeCount = 0
  private var createCount = 0

  private val startTime = System.nanoTime()

  private val accounts = ui.accounts

  lazy val cache = new ZCache(ui.global.cache)

  accounts.current.on(Threading.Ui) { setAccount } (EventContext.Global)

  override def onCreate(context: Context): Unit = {
    verbose(s"onCreate $context, count: $createCount")
    createCount += 1
    if (this.context == null) {
      this.context = context.getApplicationContext
    }
    ui.onCreate(context)
  }

  private def setAccount(zms: Option[AccountService]): Unit = if (account != zms) {
    if (resumeCount > 0) {
      account.foreach(_.global.lifecycle.releaseUi(s"replace api: $this"))
      zms.foreach(_.global.lifecycle.acquireUi(s"replace api: $this"))
    }
    account = zms
  }

  override def onResume(): Unit = {
    debug("onResume")
    resumeCount += 1
    account.foreach(_.global.lifecycle.acquireUi(s"resume api: $this"))
    ui.onResume()
  }

  override def onPause(): Unit = {
    debug("onPause")
    assert(resumeCount > 0, "onPause should be called exactly once for every onResume")
    resumeCount -= 1
    ui.onPause()
    account.foreach(_.global.lifecycle.releaseUi(s"pause api: $this"))
  }

  override def onDestroy(): Unit = {
    debug("onDestroy")
    assert(createCount > 0, "onDestroy should be called exactly once for every onCreate")
    assert(resumeCount == 0, s"onDestroy() was called before onPause(), this means there is some error in lifecycle callbacks")
    ui.onDestroy()
    account = None
    createCount -= 1
  }

  override def onInit(listener: api.InitListener): Unit = accounts.getCurrentAccountInfo onComplete {
    case Success(accountData) =>
      debug(s"initFuture completed, loggedUser: $accountData")
      // FIXME: this ensures that self is loaded, but it's pretty ugly
      ui.users.selfUser.update(accountData)
      ui.convs.convsList // force convs list loading
      debug(s"Time needed for startup: ${(System.nanoTime - startTime) / 1000 / 1000f} ms" )
      listener.onInitialized(ui.users.selfUser)
    case res =>
      error(s"initFuture failed: $res")
  }

  override def setPermissionProvider(p: PermissionProvider): Unit = ui.global.permissions.setProvider(p)

  override def removePermissionProvider(p: PermissionProvider): Unit = ui.global.permissions.clearProvider(p)

  override def getSelf: Self = ui.users.selfUser

  override def login(credentials: com.waz.api.Credentials, listener: api.LoginListener): Unit = credentials match {
    case credentials: Credentials =>
      verbose(s"login $credentials")
      accounts.login(credentials) onComplete {
        case Success(Left(err @ ErrorResponse(code, message, label))) =>
          error(s"Login for credentials: $credentials failed: $err")
          listener.onFailed(code, message, label)
        case Success(Right(acc)) =>
          listener.onSuccess(updateSelfUser(acc))
        case Failure(ex) =>
          error(s"Login for credentials: $credentials failed", ex)
          listener.onFailed(499, ex.getMessage, "internal-error")
      }
    case _ => error("Use the Credentials factory to create credentials")
  }

  private def updateSelfUser(acc: AccountData) = {
    // FIXME: this ensures that self is loaded, but it's pretty ugly
    ui.users.selfUser.update(Some(acc))
    ui.users.selfUser
  }


  override def register(credentials: com.waz.api.Credentials, name: String, accent: com.waz.api.AccentColor, listener: RegistrationListener): Unit = credentials match {
    case credentials: Credentials =>
      verbose(s"register($credentials, $name, $accent)")
      require(accent.isInstanceOf[AccentColor])
      accounts.register(credentials, name, accent.asInstanceOf[AccentColor]) onComplete {
        case Success(Right(acc)) =>
          verbose(s"registration complete: $acc")
          listener.onRegistered(updateSelfUser(acc))
        case Success(Left(ErrorResponse(code, msg, label))) =>
          verbose(s"registration failed: $code, $msg, $label")
          listener.onRegistrationFailed(code, msg, label)
        case Failure(ex) =>
          error(s"register($credentials, $name) failed", ex)
          listener.onRegistrationFailed(499, ex.getMessage, "")
      }
    case _ => error("Use the Credentials factory to create credentials")
  }

  private def activateResultHandler(kindOfAccess: api.KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Try[ActivateResult] => Unit = {
    case Success(ActivateResult.Success) => listener.onConfirmationCodeSent(kindOfAccess)
    case Success(ActivateResult.PasswordExists) => listener.onPasswordExists(kindOfAccess)
    case Success(ActivateResult.Failure(ErrorResponse(status, msg, label))) => listener.onConfirmationCodeSendingFailed(kindOfAccess, status, msg, label)
    case Failure(ex) => listener.onConfirmationCodeSendingFailed(kindOfAccess, 499, ex.getMessage, "")
  }

  override def requestPhoneConfirmationCode(phoneNumber: String, kindOfAccess: api.KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Unit =
    accounts.requestPhoneConfirmationCode(PhoneNumber(phoneNumber), kindOfAccess) onComplete activateResultHandler(kindOfAccess, listener)

  override def requestPhoneConfirmationCall(phoneNumber: String, kindOfAccess: api.KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Unit =
    accounts.requestPhoneConfirmationCall(PhoneNumber(phoneNumber), kindOfAccess) onComplete activateResultHandler(kindOfAccess, listener)

  override def verifyPhoneNumber(phoneNumber: String, confirmationCode: String, kindOfVerification: api.KindOfVerification, listener: PhoneNumberVerificationListener): Unit =
    accounts.verifyPhoneNumber(PhoneCredentials(PhoneNumber(phoneNumber), Option(confirmationCode) map ConfirmationCode), kindOfVerification) onComplete {
      case Success(Right(())) => listener.onVerified(kindOfVerification)
      case Success(Left(ErrorResponse(status, msg, label))) => listener.onVerificationFailed(kindOfVerification, status, msg, label)
      case Failure(ex) => listener.onVerificationFailed(kindOfVerification, 499, ex.getMessage, "")
    }

  override def logout() = ui.currentAccount.head flatMap {
    case Some(acc) =>
      verbose(s"logout $acc")
      acc.logout()
    case None => Future.successful(())
  }

  override def getConversations = ui.convs.convsList

  override def search() = new Search

  override def getCache: ZCache = cache

  override def getIncomingMessages = ui.cached(Uris.IncomingMsgsUri, new IncomingMessages)

  override def getActiveVoiceChannels = ui.cached(Uris.VoiceChannelsUri, new ActiveVoiceChannels)

  override def getUser(id: String): User = ui.users.getUser(UserId(id))

  override def getMediaManager: MediaManager = ui.global.mediaManager.mediaManager.orNull

  override def getMediaResourceUri(name: String): Uri = ui.global.mediaManager.getSoundUri(name).orNull

  override def getErrors: ErrorsList = ui.cached(Uris.ErrorsUri, new ErrorsList)

  override def getInvitations: Invitations = ui.invitations

  override def getAvs: Avs = ui.cached(Uris.AvsUri, new Avs)

  override def getContacts: Contacts = ui.cached(Uris.ContactsUri, new Contacts(SearchKeyFiltering()))

  override def getGiphy: Giphy = new Giphy

  override def getSpotify: api.Spotify = ui.cached(Uris.SpotifyUri, new Spotify)

  override def getConnectionIndicator = new ConnectionIndicator()

  override def getUsernames = new Usernames
}
