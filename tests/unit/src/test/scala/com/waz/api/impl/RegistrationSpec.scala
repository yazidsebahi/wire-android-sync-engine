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

import android.database.sqlite.SQLiteDatabase
import android.net.Uri
import com.waz.RobolectricUtils
import com.waz.ZLog._
import com.waz.api.ZMessagingApi.RegistrationListener
import com.waz.api.{CredentialsFactory, InitListener, LoginListener}
import com.waz.client.RegistrationClient
import com.waz.model._
import com.waz.service.InstanceService
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils.{EmptySyncService, MockGlobalModule, MockInstance, MockUiModule, MockZMessaging}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.UiModule
import com.waz.utils._
import com.waz.utils.events.EventContext
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import com.waz.znet.ContentEncoder.{BinaryRequestContent, RequestContent}
import com.waz.znet.Request._
import com.waz.znet.Response.{Headers, HttpStatus, ResponseBodyDecoder}
import com.waz.znet._
import org.json.JSONObject
import org.robolectric.Robolectric
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class RegistrationSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>

  import Threading.Implicits.Background

  lazy val context = Robolectric.application

  implicit def db: SQLiteDatabase = api.zmessaging.value.storage.dbHelper.getWritableDatabase

  implicit val ec = EventContext.Global
  val timeout = 5.seconds

  lazy val selfUser = UserData("Self User")
  lazy val otherUser = UserData("Other User")

  var loginResponse: Either[ErrorResponse, (Token, Cookie)] = _
  var registerResponse: Either[ErrorResponse, (UserInfo, Cookie)] = _
  var response: ((Uri, RequestContent)) => Response = _
  var request: Option[(Uri, RequestContent)] = _
  var selfUserSyncRequested = false

  class MockGlobal extends MockGlobalModule {

    override lazy val client: AsyncClient = new AsyncClient(wrapper = TestClientWrapper) {
      override def apply(uri: Uri, method: String, body: RequestContent, headers: Map[String, String], followRedirect: Boolean, timeout: FiniteDuration, decoder: Option[ResponseBodyDecoder], downloadProgressCallback: Option[ProgressCallback] = None): CancellableFuture[Response] = {
        println(s"uri: $uri, body: $body")
        request = Some((uri, body))
        CancellableFuture.successful(response(request.value))
      }
    }

    override lazy val loginClient: LoginClient = new LoginClient(client, backend) {
      override def login(user: ZUserId, credentials: Credentials) = CancellableFuture.successful(loginResponse)
      override def access(cookie: Option[String], token: Option[Token]) = CancellableFuture.successful(loginResponse)
    }
    override lazy val regClient: RegistrationClient = new RegistrationClient(client, backend) {
      override def register(user: ZUserId, credentials: Credentials, name: String, accentId: Option[Int]) = CancellableFuture.successful(registerResponse)
    }
  }

  lazy val global = new MockGlobal
  lazy val instance = new MockInstance(global, new MockZMessaging(_, _, _) {
    override lazy val sync = new EmptySyncService {
      override def syncSelfUser(): Future[SyncId] = {
        selfUserSyncRequested = true
        super.syncSelfUser()
      }
    }
  })

  implicit lazy val ui: UiModule = MockUiModule(instance)

  lazy val api = new ZMessagingApi()(ui)


  before {
    loginResponse = Left(ErrorResponse(0, "", ""))
    registerResponse = Left(ErrorResponse(0, "", ""))
    response = { _ => Response(Response.Cancelled) }
    request = None
    selfUserSyncRequested = true

    api.onCreate(context)
    api.onResume()

    var initialized = false
    api.onInit(new InitListener {
      override def onInitialized(s: com.waz.api.Self): Unit = {
        initialized = true
      }
    })

    withDelay(initialized shouldEqual true)
  }

  after {
    if (api.zmessaging.exists(_.lifecycle.isUiActive)) {
      api.onPause()
      api.zmessaging foreach { zms =>
        Thread.sleep(1000)
        val dbName = zms.storage.dbHelper.getDatabaseName
        Await.result(zms.storage.close().flatMap(_ => zms.global.storage.close()), 5.seconds)
        context.getDatabasePath(dbName).getParentFile.listFiles.foreach(_.delete())
      }
      api.onDestroy()
    }
  }

  def loggedIn(api: ZMessagingApi = test.api) = api.zmessaging.get.user.onVerifiedLogin.currentValue.flatten.isDefined

  feature("New user registration") {

    scenario("Register new user, set image, and verify email right away") {
      val selfUserId = UserId()
      registerResponse = Right((UserInfo(selfUserId, Some("name"), Some(0), Some(EmailAddress("email"))), None))
      response = { _ => Response(HttpStatus(403), JsonObjectResponse(Json("code" -> 403, "message" -> "invalid credentials", "label" -> ""))) }

      var self: com.waz.api.Self = null
      api.register(CredentialsFactory.emailCredentials("email", "passwd"), "name", AccentColor(0), new RegistrationListener {
        override def onRegistrationFailed(i: Int, s: String, s1: String): Unit = {}
        override def onRegistered(s: com.waz.api.Self): Unit = self = s
      })

      withDelay {
        self should not be null
        self.isLoggedIn shouldEqual true
        self.getUser.getId shouldEqual selfUserId.str
        self.getName shouldEqual "name"
        self.getEmail shouldEqual "email"
        self.isEmailVerified shouldEqual false
        loggedIn() shouldEqual false
      }

      self.setPicture(ui.images.getOrCreateImageAssetFrom(IoUtils.toByteArray(getClass.getResourceAsStream("/images/penguin.png"))))

      var imageAsset: com.waz.api.ImageAsset = ImageAsset.Empty
      soon {
        imageAsset = self.getUser.getPicture
        imageAsset should not be empty
      }

      soon {
        imageAsset.data should not be empty
      }

      val asset = api.zmessaging.get.imageAssets.getImageAsset(imageAsset.data.id).await()
      asset should be('defined)
      asset.map(_.convId.str) shouldEqual Some(selfUserId.str)

      idle(500.millis)
      self.isLoggedIn shouldEqual true
      selfUserSyncRequested shouldEqual true

      response = _ => Response(HttpStatus(200), JsonObjectResponse(Json("access_token" -> "token", "expires_in" -> 36000, "token_type" -> "Bearer")), Headers(LoginClient.SetCookie -> "zuid=asd;asd"))
      loginResponse = Right((Token("", "Bearer", System.currentTimeMillis() + 10.minutes.toMillis), Some("sd-zuid=asd;asd")))

      api.zmessaging.get.usersClient.loadSelf()

      within (10.seconds) {
        self.isEmailVerified shouldEqual true
        loggedIn() shouldEqual true
      }
    }

    scenario("Register new user, restart the app, verify email, login again") {
      val selfUserId = UserId()
      registerResponse = Right((UserInfo(selfUserId, Some("name"), Some(0), Some(EmailAddress("email"))), None))
      response = _ => Response(HttpStatus(403), JsonObjectResponse(Json("code" -> 403, "message" -> "invalid credentials", "label" -> "")))

      var self: com.waz.api.Self = null
      var res = ""
      api.register(CredentialsFactory.emailCredentials("email", "passwd"), "name", AccentColor(0), new RegistrationListener {
        override def onRegistrationFailed(i: Int, s: String, s1: String): Unit = { res = s }
        override def onRegistered(s: com.waz.api.Self): Unit = {
          self = s
          res = "done"
        }
      })

      withDelay {
        res shouldEqual "done"
        self should not be null
        self.isLoggedIn shouldEqual true
        self.getUser.getId shouldEqual selfUserId.str
        self.getName shouldEqual "name"
        self.getEmail shouldEqual "email"
        self.isEmailVerified shouldEqual false
        loggedIn() shouldEqual false
      }

      withDelay(global.prefs.preferences.getString(InstanceService.CurrentUserPref, "") should not be "")

      self = null

      idle(1.second) // wait for storage to sync

      api.onPause()
      api.onDestroy()
      idle(1.second)
      api.zmessaging.foreach { zms =>
        zms.storage.close().flatMap(_ => zms.global.storage.close()).await()
      }

      request = None
      response = {
        case req @ (uri, BinaryRequestContent(content, "application/json")) if uri.getLastPathSegment == "login" && new JSONObject(new String(content)).getString("password") == "passwd" =>
          Response(HttpStatus(200), JsonObjectResponse(Json("access_token" -> "token", "expires_in" -> 36000, "token_type" -> "Bearer")), Headers(LoginClient.SetCookie -> "sd-zuid=asd;asd"))
        case req =>
          Response(HttpStatus(403), JsonObjectResponse(Json("code" -> 403, "message" -> "invalid credentials", "label" -> "")))
      }

      debug("##### starting new api")(logTagFor[RegistrationSpec])

      val api2 = new ZMessagingApi()(MockUiModule(new MockInstance(global)))
      api2.onCreate(context)
      api2.onResume()

      global.prefs.preferences.getString(InstanceService.CurrentUserPref, "") should not be ""

      api2.onInit(new InitListener {
        override def onInitialized(s: com.waz.api.Self): Unit = {
          self = s
        }
      })

      withDelay {
        self should not be null
        self.isLoggedIn shouldEqual true
        self.getUser should not be null
        self.getEmail shouldEqual "email"
        self.getName shouldEqual "name"
        self.isEmailVerified shouldEqual false
        loggedIn(api2) shouldEqual false
      }

      api2.zmessaging.value.user.user.password shouldEqual None

      loginResponse = Right((Token("", "Bearer", System.currentTimeMillis() + 10.minutes.toMillis), Some("sd-zuid=asd;asd")))
      res = ""
      api2.login(CredentialsFactory.emailCredentials("email", "passwd"), new LoginListener {
        override def onSuccess(user: com.waz.api.Self): Unit = res = "done"
        override def onFailed(code: Int, message: String, label: String): Unit = res = message
      })

      withDelay {
        res shouldEqual "done"
        self.isLoggedIn shouldEqual true
        self.getEmail shouldEqual "email"
        self.getName shouldEqual "name"
        self.isEmailVerified shouldEqual true
        self.getUser should not be null
        self.getUser.getId shouldEqual selfUserId.str
        loggedIn(api2) shouldEqual true
      }
    }
  }
}
