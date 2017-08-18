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
package com.waz.service.push

import com.waz.content.{AccountsStorage, GlobalPreferences}
import com.waz.content.GlobalPreferences.{CurrentAccountPref, PushEnabledKey}
import com.waz.model._
import com.waz.service.ZmsLifecycle
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.testutils.TestGlobalPreferences
import com.waz.threading.Threading
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.utils.wrappers.GoogleApi

import scala.concurrent.Future
import scala.concurrent.duration._

class PushTokenServiceSpec extends AndroidFreeSpec {

  val google    = mock[GoogleApi]
  val lifecycle = mock[ZmsLifecycle]
  val accounts  = mock[AccountsStorage]
  val sync      = mock[SyncServiceHandle]
  val prefs     = new TestGlobalPreferences()
  val accountId = AccountId()

  val pushEnabled         = prefs.preference(PushEnabledKey)
  val currentAccount      = prefs.preference(CurrentAccountPref)
  val currentToken        = prefs.preference(GlobalPreferences.PushToken)

  val googlePlayAvailable = Signal(false)
  val lifecycleActive     = Signal(false)
  val accountSignal       = Signal[AccountData]()

  val defaultDuration = 5.seconds

  def accountData(token: PushToken): AccountData = AccountData(accountId, Left({}), None, "", None, None, Some(token))
  def accountData(token: Option[PushToken]): AccountData = AccountData(accountId, Left({}), None, "", None, None, token)

  feature("Token generation") {
    scenario("Fetches token on init if GCM available") {
      val token = PushToken("token")
      (google.getPushToken _).expects().returning(Some(token))
      val service = initTokenService()

      pushEnabled := true
      googlePlayAvailable ! true
      result(service.currentToken.signal.filter(_.contains(token)).head)
    }

    scenario("Remove Push Token event should create new token and delete all previous tokens") {

      val oldToken = PushToken("oldToken")
      val newToken = PushToken("newToken")
      pushEnabled := true
      currentToken := Some(oldToken)
      googlePlayAvailable ! true

      (google.getPushToken _).expects().once().returning(Some(newToken))

      //This needs to be called
      (google.deleteAllPushTokens _).expects().once()

      val service = initTokenService()

      //wait for first token to be set
      result(service.currentToken.signal.filter(_.contains(oldToken)).head)
      //delete first token in response to BE event
      service.eventProcessingStage(RConvId(), Vector(PushTokenRemoveEvent(oldToken, "sender", Some("client"))))
      //new token should be set
      result(service.currentToken.signal.filter(_.contains(newToken)).head)
    }
  }

  feature("Token registration") {
    scenario("If current user does not have matching registeredPush token, remove the old token and register the new one with our BE") {

      val oldToken = PushToken("oldToken")
      val newToken = PushToken("token")

      accountSignal ! accountData(oldToken)
      currentAccount := Some(accountId)

      currentToken := Some(newToken)
      pushEnabled := true
      googlePlayAvailable ! true

      lazy val service = initTokenService()

      (sync.deletePushToken _).expects(oldToken).once().returning(Future.successful(SyncId()))

      (sync.registerPush _).expects(newToken).once().onCall { _: PushToken =>
        Future {
          service.onTokenRegistered(newToken)
          SyncId()
        } (Threading.Background)
      }

      result(service.currentToken.signal.filter(_.contains(newToken)).head)
      result(accountSignal.filter(_.registeredPush.contains(newToken)).head)
    }

    scenario("Instance Id token refresh should trigger re-registration for current user") {
      val oldToken = PushToken("oldToken")
      val newToken = PushToken("newToken")

      accountSignal ! accountData(oldToken)
      currentAccount := Some(accountId)
      pushEnabled := true
      googlePlayAvailable ! true
      currentToken := Some(oldToken)

      lazy val service = initTokenService()

      (sync.deletePushToken _).expects(oldToken).once().returning(Future.successful(SyncId()))
      (sync.registerPush _).expects(newToken).once().onCall { _: PushToken =>
        Future {
          service.onTokenRegistered(newToken)
          SyncId()
        } (Threading.Background)
      }

      result(service.currentToken.signal.filter(_.contains(oldToken)).head)
      result(accountSignal.filter(_.registeredPush.contains(oldToken)).head)

      service.onTokenRefresh ! Some(newToken) //InstanceIDService triggers new token

      result(service.currentToken.signal.filter(_.contains(newToken)).head)
      result(accountSignal.filter(_.registeredPush.contains(newToken)).head)
    }

    scenario("After user is logged out, clearing their current push token should NOT trigger new registration") {
      val token = PushToken("token")

      accountSignal ! accountData(token)
      currentAccount := Some(accountId)
      pushEnabled := true
      googlePlayAvailable ! true
      currentToken := Some(token)

      (google.getPushToken _).expects().never()
      (sync.registerPush _).expects(*).never()

      lazy val service = initTokenService()

      result(service.pushActive.filter(_ == true).head)

      currentAccount := None //user is logged out
      accountSignal ! accountData(None)

      result(service.pushActive.filter(_ == false).head)

      /**
        * It seems as though there can be a couple of instances of zms (and therefore the push token service) available.
        * So lets make sure we only register the account assigned to our instance. Logging in then with another account
        * shouldn't change the state of this instance.
        */
      currentAccount := Some(AccountId())

      awaitAllTasks
      result(service.pushActive.filter(_ == false).head)
    }
  }

  feature("Push active") {

    scenario("Push should be active if push is enabled and inactive if not") {
      val token = PushToken("token")
      accountSignal ! accountData(token)
      currentAccount := Some(accountId)
      pushEnabled := true //set active
      googlePlayAvailable ! true
      currentToken := Some(token)

      val service = initTokenService()

      result(service.pushActive.filter(_ == true).head)

      pushEnabled := false //set inactive
      result(service.pushActive.filter(_ == false).head)
    }

    scenario("Push should be active if app is in background and inactive if not") {
      val token = PushToken("token")
      accountSignal ! accountData(token)
      currentAccount := Some(accountId)
      pushEnabled := true
      googlePlayAvailable ! true
      lifecycleActive ! true //websocket should be open
      currentToken := Some(token)

      val service = initTokenService()

      result(service.pushActive.filter(_ == false).head)

      lifecycleActive ! false //websocket should be off - use push again
      result(service.pushActive.filter(_ == true).head)
    }

    scenario("Push should be inactive if play services are unavailable") {
      val token = PushToken("token")
      accountSignal ! accountData(token)
      currentAccount := Some(accountId)
      pushEnabled := true
      googlePlayAvailable ! true
      currentToken := Some(token)

      val service = initTokenService()

      result(service.pushActive.filter(_ == true).head)

      googlePlayAvailable ! false //Set true and then false again to cause the signal to change. Maybe the user disables play services?
      result(service.pushActive.filter(_ == false).head)
    }

    scenario("Push should be inactive if user is not currently registered") {

      val token = PushToken("token")
      accountSignal ! accountData(None)
      currentAccount := Some(accountId)
      pushEnabled := true
      googlePlayAvailable ! true
      currentToken := Some(token)

      (sync.registerPush _).expects(*).anyNumberOfTimes().returning(Future.successful(SyncId()))
      val service = initTokenService()
      result(service.pushActive.filter(_ == false).head)
    }
  }

  def initTokenService() = {
    (google.isGooglePlayServicesAvailable _).expects().anyNumberOfTimes().returning(googlePlayAvailable)
    (accounts.signal _).expects(*).anyNumberOfTimes().returning(accountSignal)
    (lifecycle.active _).expects().anyNumberOfTimes().returning(lifecycleActive)
    (accounts.update _).expects(accountId, *).anyNumberOfTimes().onCall { (_, f) =>
      Future {
        returning(accountSignal.currentValue("").fold(Option.empty[(AccountData, AccountData)])(p => Some((p, f(p))))) {
          case Some((_, updated)) => accountSignal ! updated
          case _ =>
        }
      }(Threading.Background)
    }

    new PushTokenService(google, prefs, lifecycle, accountId, accounts, sync)
  }
}
