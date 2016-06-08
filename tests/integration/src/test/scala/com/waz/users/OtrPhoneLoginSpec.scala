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
package com.waz.users

import java.util.UUID

import akka.pattern.ask
import com.waz.api._
import com.waz.login.RegistrationUtils
import com.waz.model.ZUserId
import com.waz.provision.ActorMessage.{RegisterPhone, Successful}
import com.waz.testutils.Matchers._
import org.robolectric.annotation.Config
import org.scalatest.{FeatureSpec, OptionValues}

import scala.concurrent.duration._
import scala.util.Success

@Config(manifest = "tests/OtrAndroidManifest.xml")
class OtrPhoneLoginSpec extends FeatureSpec with OptionValues with ApiSpec with ProcessActorSpec with RegistrationUtils { test =>
  override val autoLogin = false

  lazy val userId = ZUserId()

  lazy val phone = randomPhoneNumber
  override lazy val email = s"android.test+${UUID.randomUUID}@wearezeta.com"

  lazy val remote = registerDevice("second_device_remote")

  feature("Register by phone") {

    scenario("Register on remote device") {
      val code = awaitUiFuture(fetchPhoneConfirmationCode(phone, KindOfAccess.REGISTRATION)).toOption
      code should not be empty

      awaitUiFuture(verifyPhoneNumber(phone, code.value, KindOfVerification.PREVERIFY_ON_REGISTRATION)) shouldEqual Success(true)

      remote ? RegisterPhone(phone.str, code.value.str, "name") should eventually(be(Successful))

      awaitUi(5.seconds)
    }

    scenario("Sign in on second device") {
      val code = awaitUiFuture(fetchPhoneConfirmationCode(phone, KindOfAccess.LOGIN)).toOption
      code should not be empty

      val self = awaitUiFuture(login(CredentialsFactory.phoneCredentials(phone.str, code.value.str))).toOption.value

      lazy val otrClient = self.getOtrClient
      lazy val otherClients = self.getOtherOtrClients

      withDelay {
        self.isLoggedIn shouldEqual true
        self.getClientRegistrationState shouldEqual ClientRegistrationState.REGISTERED

        otrClient should not be empty
        otherClients should have size 1
      }

      self.setPassword("password", new CredentialsUpdateListener {
        override def onUpdated(): Unit = ()
        override def onUpdateFailed(code: Int, message: String, label: String): Unit =
          info(s"unable to update credentials: $code, $message, $label")
      })

      withDelay {
        self.getClientRegistrationState shouldEqual ClientRegistrationState.REGISTERED
      }
    }
  }
}
