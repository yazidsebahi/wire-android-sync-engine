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

import com.waz.api.ZMessagingApi.{RegistrationListener, PhoneNumberVerificationListener, PhoneConfirmationCodeRequestListener}
import com.waz.api.impl.AccentColor
import com.waz.api._
import com.waz.model.{EmailAddress, ConfirmationCode, PhoneNumber, ZUserId}
import com.waz.provision.InternalBackendClient
import org.scalatest.{OptionValues, FeatureSpec}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class PhoneRegistrationAndLoginSpec extends FeatureSpec with OptionValues with ApiSpec { test =>
  override val autoLogin = false

  lazy val userId = ZUserId()

  lazy val phone = randomPhoneNumber
  override lazy val email = s"android.test+${UUID.randomUUID}@wearezeta.com"

  lazy val internalBackendClient = new InternalBackendClient(globalModule.client, testBackend)

  def randomPhoneNumber: PhoneNumber = PhoneNumber("+0" + Array.fill(14)(Random.nextInt(10)).mkString)

  var confirmationCodeSent: Boolean = false
  var confirmationCode = Option.empty[ConfirmationCode]

  lazy val confirmationCodeListener = new PhoneConfirmationCodeRequestListener {
    override def onConfirmationCodeSent(kindOfAccess: KindOfAccess): Unit = confirmationCodeSent = true
    override def onConfirmationCodeSendingFailed(kindOfAccess: KindOfAccess, code: Int, message: String, label: String): Unit = {
      info(s"unable to request confirmation code: $kindOfAccess, $code, $message, $label")
      confirmationCodeSent = false
    }
  }

  var credentialsUpdated: Boolean = false
  lazy val credentialsListener = new CredentialsUpdateListener {
    override def onUpdated(): Unit = credentialsUpdated = true
    override def onUpdateFailed(code: Int, message: String, label: String): Unit = {
      info(s"unable to update credentials: $code, $message, $label")
      credentialsUpdated = false
    }
  }

  var loggedInSelf = Option.empty[Self]
  lazy val loginListener = new LoginListener {
    override def onSuccess(user: Self): Unit = loggedInSelf = Some(user)
    override def onFailed(code: Int, message: String, label: String): Unit = {
      info(s"unable to login: $code, $message, $label")
      loggedInSelf = None
    }
  }

  feature("Register by phone") {
    scenario("Request confirmation code") {
      api.requestPhoneConfirmationCode(phone.str, KindOfAccess.REGISTRATION, confirmationCodeListener)
      withDelay {
        confirmationCodeSent shouldEqual true
      }
    }

    scenario("Verify confirmation code") {
      confirmationCode = Await.result(internalBackendClient.getPhoneActivationCode(phone), 10.seconds).right.toOption

      var verified = false
      api.verifyPhoneNumber(phone.str, confirmationCode.value.str, KindOfVerification.PREVERIFY_ON_REGISTRATION, new PhoneNumberVerificationListener {
        override def onVerified(kindOfVerification: KindOfVerification): Unit = verified = true
        override def onVerificationFailed(kindOfVerification: KindOfVerification, code: Int, message: String, label: String): Unit = {
          info(s"unable to verify phone number: $kindOfVerification, $code, $message, $label")
          verified = false
        }
      })

      withDelay {
        verified shouldEqual true
      }
    }

    scenario("Register") {
      var self = Option.empty[Self]
      api.register(CredentialsFactory.phoneCredentials(phone.str, confirmationCode.value.str), "name", AccentColor(1), new RegistrationListener {
        override def onRegistered(user: Self): Unit = self = Some(user)
        override def onRegistrationFailed(code: Int, message: String, label: String): Unit = {
          info(s"unable to register: $code, $message, $label")
          self = None
        }
      })

      withDelay {
        self should be(defined)
        self.value.getName shouldEqual "name"
        self.value.getPhone shouldEqual phone.str
        self.value.isLoggedIn shouldEqual true
        self.value.isEmailVerified shouldEqual false
        self.value.getEmail should be(empty)
      }
    }

  }
  feature("Add email address and password afterwards") {
    scenario("Add a password") {
      val self = api.getSelf
      self.setPassword("initial", credentialsListener)
      withDelay {
        credentialsUpdated shouldEqual true
      }
    }

    scenario("Add an email address") {
      val self = api.getSelf
      credentialsUpdated = false
      self.setEmail(email, credentialsListener)
      withDelay {
        credentialsUpdated shouldEqual true
        self.getEmail should be (empty)
        self.getPhone shouldEqual phone.str
        self.isEmailVerified shouldEqual false
      }
    }

    scenario("Activate the added email address") {
      Await.result(internalBackendClient.activateEmail(EmailAddress(email)), 10.seconds) should be ('right)
      val self = api.getSelf
      withDelay {
        self.getEmail shouldEqual email
        self.isEmailVerified shouldEqual true
      }
    }

    scenario("Logout") {
      api.logout()
      withDelay {
        api.getSelf.isLoggedIn shouldEqual false
      }
    }
  }

  feature("Sign in by phone") {
    scenario("Request login confirmation code") {
      confirmationCodeSent = false
      api.requestPhoneConfirmationCode(phone.str, KindOfAccess.LOGIN, confirmationCodeListener)
      withDelay {
        confirmationCodeSent shouldEqual true
      }
      confirmationCode = Await.result(internalBackendClient.getPhoneLoginCode(phone), 10.seconds).right.toOption
    }

    scenario("Login") {
      api.login(CredentialsFactory.phoneCredentials(phone.str, confirmationCode.value.str), loginListener)
      withDelay {
        val self = loggedInSelf
        self should be (defined)
        self.value.isLoggedIn shouldEqual true
        self.value.getPhone shouldEqual phone.str
        self.value.getEmail shouldEqual email
        self.value.getUser.getEmail shouldEqual email
      }
    }

    scenario("Logout") {
      api.logout()
      withDelay {
        api.getSelf.isLoggedIn shouldEqual false
      }
    }
  }

  feature("Sign in with the email address that was added") {
    scenario("Login") {
      loggedInSelf = None
      api.login(CredentialsFactory.emailCredentials(email, "initial"), loginListener)
      withDelay {
        val self = loggedInSelf
        self should be (defined)
        self.value.isLoggedIn shouldEqual true
        self.value.getEmail shouldEqual email
        self.value.getPhone shouldEqual phone.str
      }
    }

    scenario("Logout") {
      api.logout()
      withDelay {
        api.getSelf.isLoggedIn shouldEqual false
      }
    }
  }
}
