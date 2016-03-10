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
import com.waz.api.{ClientRegistrationState, PermissionProvider}
import com.waz.api.ZMessagingApi.{PhoneConfirmationCodeRequestListener, PhoneNumberVerificationListener, RegistrationListener}
import com.waz.api.impl.search.Search
import com.waz.content.Uris
import com.waz.media.manager.MediaManager
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.ui.UiModule

import scala.concurrent.Future
import scala.util.{Failure, Success}

class ZMessagingApi(implicit val ui: UiModule) extends com.waz.api.ZMessagingApi {

  import ClientRegistrationState._
  import Threading.Implicits.Ui
  import ui.eventContext
  private implicit val logTag: LogTag = logTagFor[ZMessagingApi]

  private[waz] var zmessaging: Option[ZMessaging] = None

  private var context: Context = _
  private var resumeCount = 0
  private var createCount = 0

  private val startTime = System.nanoTime()

  private val instance = ui.instance

  lazy val cache = new ZCache(ui.global.cache)

  override def onCreate(context: Context): Unit = {
    verbose(s"onCreate $context, count: $createCount")
    createCount += 1
    if (this.context == null) {
      this.context = context.getApplicationContext
      ui.current.on(Threading.Ui) { (zms: Option[ZMessaging]) => setZmessaging(zms) }
    }
    ui.onCreate(context)
  }

  private def initFuture = instance.getCurrent flatMap {
    case Some(zms) =>
      debug("getCurrent returned zmessaging instance")
      setZmessaging(Some(zms))
      initUserInfo(zms, true)
    case None =>
      debug(s"getCurrent returned None")
      Future.successful((None, UNKNOWN))
  }

  private def initUserInfo(zms: ZMessaging, assumeLoggedIn: Boolean) =
    for {
      user <- zms.users.getSelfUser
      st <- zms.otrContent.registrationState
      state <- st match {
        case UNKNOWN if assumeLoggedIn => zms.otrClientsService.awaitClientRegistered() flatMap { _ => zms.otrContent.registrationState }
        case _ => Future successful st
      }
    } yield (user, state)

  override def onResume(): Unit = {
    debug("onResume")
    resumeCount += 1
    zmessaging.foreach(_.lifecycle.acquireUi(s"resume api: $this"))
    ui.onResume()
  }

  override def onPause(): Unit = {
    debug("onPause")
    assert(resumeCount > 0, "onPause should be called exactly once for every onResume")
    resumeCount -= 1
    ui.onPause()
    zmessaging.foreach(_.lifecycle.releaseUi(s"pause api: $this"))
  }

  override def onDestroy(): Unit = {
    debug("onDestroy")
    assert(createCount > 0, "onDestroy should be called exactly once for every onCreate")
    assert(resumeCount == 0, s"onDestroy() was called before onPause(), this means there is some error in lifecycle callbacks")
    ui.onDestroy()
    createCount -= 1
  }

  override def onInit(listener: api.InitListener): Unit = initFuture onComplete {
    case Success((loggedUser, otrRegState)) =>
      debug(s"initFuture completed, loggedUser: $loggedUser")
      // FIXME: this ensures that self is loaded, but it's pretty ugly
      ui.users.selfUser.zuser = zmessaging.map(_.user.user)
      loggedUser foreach { u => ui.users.selfUser.updateUser(u, otrRegState) }
      ui.convs.convsList // force convs list loading
      debug(s"Time needed for startup: ${(System.nanoTime - startTime) / 1000 / 1000f} ms" )
      listener.onInitialized(ui.users.selfUser)
    case res =>
      error(s"initFuture failed: $res")
  }

  override def setPermissionProvider(p: PermissionProvider): Unit = ui.global.permissions.setProvider(p)

  override def removePermissionProvider: Unit = ui.global.permissions.clearProvider()

  override def getSelf: Self = ui.users.selfUser

  override def login(credentials: com.waz.api.Credentials, listener: api.LoginListener): Unit = credentials match {
    case credentials: Credentials =>
      instance.login(credentials) onComplete {
        case Success(Left(err @ ErrorResponse(code, message, label))) =>
          error(s"Login for credentials: $credentials failed: $err")
          listener.onFailed(code, message, label)
        case Success(Right(zms)) =>
          setZmessaging(Some(zms))
          updateSelfUser(zms, true) onComplete {
            case Success(Some(user)) => listener.onSuccess(user)
            case Success(None) =>
              error(s"updateSelfUser returned None")
              listener.onFailed(499, "Couldn't load self user", "internal-error")
            case Failure(ex) =>
              error(s"updateSelfUser failed", ex)
              listener.onFailed(499, "Couldn't load self user", "internal-error")
          }
        case Failure(ex) =>
          error(s"Login for credentials: $credentials failed", ex)
          listener.onFailed(499, ex.getMessage, "internal-error")
      }
    case _ => error("Use the Credentials factory to create credentials")
  }

  private def setZmessaging(zms: Option[ZMessaging]): Unit = if (zmessaging != zms) {
    if (resumeCount > 0) {
      zmessaging.foreach(_.lifecycle.releaseUi(s"replace api: $this"))
      zms.foreach(_.lifecycle.acquireUi(s"replace api: $this"))
    }
    zmessaging = zms
  }

  private def updateSelfUser(zms: ZMessaging, assumeLoggedIn: Boolean) =
    initUserInfo(zms, assumeLoggedIn) map {
      case (user, otrRegState) if zmessaging.contains(zms) =>
        // FIXME: this ensures that self is loaded, but it's pretty ugly
        ui.users.selfUser.zuser = zmessaging.map(_.user.user)
        user foreach { u => ui.users.selfUser.updateUser(u, otrRegState) }
        Some(ui.users.selfUser)
      case _ =>
        info(s"updateSelfUser - zmessaging instance already changed")
        None
    }


  override def register(credentials: com.waz.api.Credentials, name: String, accent: com.waz.api.AccentColor, listener: RegistrationListener): Unit = credentials match {
    case credentials: Credentials =>
      require(accent.isInstanceOf[AccentColor])
      instance.register(credentials, name, accent.asInstanceOf[AccentColor]) onComplete {
        case Success(Right(zms)) =>
          setZmessaging(Some(zms))
          updateSelfUser(zms, credentials.autoLoginOnRegistration) onComplete {
            case Success(Some(user)) => listener.onRegistered(user)
            case _ => listener.onRegistrationFailed(499, "Internal error", "")
          }
        case Success(Left(ErrorResponse(code, msg, label))) =>
          listener.onRegistrationFailed(code, msg, label)
        case Failure(ex) =>
          error(s"register($credentials, $name) failed", ex)
          listener.onRegistrationFailed(499, ex.getMessage, "")
      }
    case _ => error("Use the Credentials factory to create credentials")
  }

  override def requestPhoneConfirmationCode(phoneNumber: String, kindOfAccess: api.KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Unit = instance.requestPhoneConfirmationCode(PhoneNumber(phoneNumber), kindOfAccess) onComplete {
    case Success(Right(())) => listener.onConfirmationCodeSent(kindOfAccess)
    case Success(Left(ErrorResponse(status, msg, label))) => listener.onConfirmationCodeSendingFailed(kindOfAccess, status, msg, label)
    case Failure(ex) => listener.onConfirmationCodeSendingFailed(kindOfAccess, 499, ex.getMessage, "")
  }

  override def verifyPhoneNumber(phoneNumber: String, confirmationCode: String, kindOfVerification: api.KindOfVerification, listener: PhoneNumberVerificationListener): Unit =
    instance.verifyPhoneNumber(PhoneCredentials(PhoneNumber(phoneNumber), Option(confirmationCode) map ConfirmationCode), kindOfVerification) onComplete {
      case Success(Right(())) => listener.onVerified(kindOfVerification)
      case Success(Left(ErrorResponse(status, msg, label))) => listener.onVerificationFailed(kindOfVerification, status, msg, label)
      case Failure(ex) => listener.onVerificationFailed(kindOfVerification, 499, ex.getMessage, "")
    }

  override def logout() = zmessaging.foreach { zms => instance.logout(zms.userId) }

  override def getConversations = ui.convs.convsList

  @deprecated("user ZMessagingApi.search() instead", "21.0")
  override def searchQuery() = new SearchQuery

  override def search() = new Search

  override def getCache: ZCache = cache

  override def getIncomingMessages = ui.cached(Uris.IncomingMsgsUri, new IncomingMessages)

  override def getActiveVoiceChannels = ui.cached(Uris.VoiceChannelsUri, new ActiveVoiceChannels)

  override def getUser(id: String): User = ui.users.getUser(UserId(id))

  override def getMediaManager: MediaManager = MediaManager.getInstance(context.getApplicationContext) // XXX: this assumes that getInstance returns the same instance every time (at least for the same context)

  override def getMediaResourceUri(name: String): Uri = zmessaging.flatMap(_.mediamanager.getSoundUri(name)).orNull

  override def getErrors: ErrorsList = ui.cached(Uris.ErrorsUri, new ErrorsList)

  override def getTrackingData: TrackingData = ui.cached(Uris.TrackingUri, new TrackingData)

  override def getInvitations: Invitations = ui.invitations

  override def getAvs: Avs = ui.cached(Uris.AvsUri, new Avs)

  override def getAudioLink: AudioLink = ui.cached(Uris.AudioLinkUri, new AudioLink)

  override def getContacts: Contacts = ui.cached(Uris.ContactsUri, new Contacts(SearchKeyFiltering()))

  override def getGiphy: Giphy = new Giphy

  override def getSpotify: api.Spotify = ui.cached(Uris.SpotifyUri, new Spotify)

  override def getLogging: Logging = ui.cached(Uris.LoggingUri, new Logging)

  override def getConnectionIndicator = new ConnectionIndicator()
}
