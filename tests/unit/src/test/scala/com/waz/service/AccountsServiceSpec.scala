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

import com.waz.api.impl.{EmailCredentials, PhoneCredentials}
import com.waz.api.{KindOfAccess, KindOfVerification}
import com.waz.client.RegistrationClient
import com.waz.client.RegistrationClientImpl.ActivateResult
import com.waz.content.AccountsStorage
import com.waz.model._
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.TestGlobalPreferences
import com.waz.threading.CancellableFuture
import com.waz.utils.events.{EventStream, Signal}
import com.waz.utils.returning
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import com.waz.znet.LoginClient

import scala.concurrent.Future

class AccountsServiceSpec extends AndroidFreeSpec {

  private val globalModule = mock[GlobalModule]
  private val storage = mock[AccountsStorage]
  private val phoneNumbers = mock[PhoneNumberService]
  private val regClient = mock[RegistrationClient]
  private val loginClient = mock[LoginClient]

  feature("Phone registration") {

    val phoneNumber = PhoneNumber("+0918273465")
    val confirmationCode = ConfirmationCode("123")

    scenario("Request for a new phone registration should create a pending account and request a code from backend") {
      val service = getAccountService

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Option.empty[AccountData]))
      (regClient.requestPhoneConfirmationCode _).expects(*, KindOfAccess.REGISTRATION).once().returning(CancellableFuture.successful[ActivateResult](ActivateResult.Success))
      (storage.updateOrCreate _).expects(*, *, *).once().onCall{ (_, _, creator) =>

          creator.phone.shouldBe(None)
          creator.pendingPhone.shouldBe(Some(phoneNumber))
          creator.regWaiting.shouldBe(true)
          creator.code.shouldBe(None)
          creator.cookie.shouldBe(None)

          Future.successful(creator)
      }

      result(service.registerPhone(phoneNumber)).shouldBe(Right(()))
    }

    scenario("Request for a phone registration that exists in the db should update the account to pending and request a code from backend") {
      val service = getAccountService
      val prevAccount = AccountData()

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Some(prevAccount)))
      (regClient.requestPhoneConfirmationCode _).expects(*, KindOfAccess.REGISTRATION).once().returning(CancellableFuture.successful[ActivateResult](ActivateResult.Success))
      (storage.updateOrCreate _).expects(*, *, *).once().onCall{ (_, updater, _) =>
        val updated = updater(prevAccount)

        updated.phone.shouldBe(None)
        updated.pendingPhone.shouldBe(Some(phoneNumber))
        updated.regWaiting.shouldBe(true)
        updated.code.shouldBe(None)
        updated.cookie.shouldBe(None)

        Future.successful(updated)
      }

      result(service.registerPhone(phoneNumber)).shouldBe(Right(()))
    }

    scenario("Validating a code for registration should update the pending phone to normal and save the confirmation code") {
      val service = getAccountService
      var account = AccountData(pendingPhone = Some(phoneNumber))

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Some(account)))
      (regClient.verifyPhoneNumber _).expects(*, KindOfVerification.PREVERIFY_ON_REGISTRATION).once().returning(CancellableFuture.successful(Right(())))
      (storage.updateOrCreate _).expects(*, *, *).once().onCall{ (_, updater, _) => returning(Future.successful(updater(account))){_ => account = updater(account)} }
      (storage.update _).expects(*, *).once().onCall{ (_, updater) =>
        account = updater(account)

        account.phone.shouldBe(Some(phoneNumber))
        account.pendingPhone.shouldBe(None)
        account.regWaiting.shouldBe(true)
        account.code.shouldBe(Some(confirmationCode.str))
        account.cookie.shouldBe(None)

        Future.successful(Some(account, account))
      }

      result(service.activatePhoneOnRegister(phoneNumber, confirmationCode)).shouldBe(Right(()))
    }

    scenario("Registering a name to a phone account should finish the registration") {
      val service = getAccountService
      var account = AccountData(phone = Some(phoneNumber))

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Some(account)))
      (regClient.register _).expects(account.id, *, *, *).once().returning(CancellableFuture.successful(Right(UserInfo(UserId()), Some(Cookie("")))))
      (storage.update _).expects(*, *).once().onCall{ (_, updater) =>
        account = updater(account)

        account.phone.shouldBe(Some(phoneNumber))
        account.pendingPhone.shouldBe(None)
        account.regWaiting.shouldBe(false)
        account.code.shouldBe(None)
        account.cookie.shouldBe(Some(Cookie("")))

        Future.successful(Some(account, account))
      }
      (storage.get _).expects(account.id).anyNumberOfTimes().returning(Future.successful(Some(account)))

      result(service.registerNameOnPhone(phoneNumber, confirmationCode, "Whisker Pants")).shouldBe(Right(()))
    }

  }

  feature("Phone login") {

    val phoneNumber = PhoneNumber("+0918273465")
    val confirmationCode = ConfirmationCode("123")

    scenario("Request a login with a new phone number should create an account and set the phone to pending"){
      val service = getAccountService

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Option.empty[AccountData]))
      (regClient.requestPhoneConfirmationCode _).expects(*, KindOfAccess.LOGIN_IF_NO_PASSWD).once().returning(CancellableFuture.successful(ActivateResult.Success))
      (storage.updateOrCreate _).expects(*, *, *).once().onCall{ (_, _, creator) =>

        creator.phone.shouldBe(None)
        creator.pendingPhone.shouldBe(Some(phoneNumber))
        creator.regWaiting.shouldBe(false)
        creator.code.shouldBe(None)
        creator.cookie.shouldBe(None)

        Future.successful(creator)
      }

      result(service.loginPhone(phoneNumber)).shouldBe(Right(()))
    }

    scenario("Request a login with an existing phone number in db should update the account and set the phone to pending"){
      val service = getAccountService
      var account = AccountData().copy(phone = Some(phoneNumber))

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Some(account)))
      (regClient.requestPhoneConfirmationCode _).expects(*, KindOfAccess.LOGIN_IF_NO_PASSWD).once().returning(CancellableFuture.successful(ActivateResult.Success))
      (storage.updateOrCreate _).expects(*, *, *).once().onCall{ (_, updater, _) =>
        account = updater(account)

        account.phone.shouldBe(None)
        account.pendingPhone.shouldBe(Some(phoneNumber))
        account.regWaiting.shouldBe(false)
        account.code.shouldBe(None)
        account.cookie.shouldBe(None)

        Future.successful(account)
      }

      result(service.loginPhone(phoneNumber)).shouldBe(Right(()))
    }

    scenario("Activate the phone on login should set the pending phone to normal and set the cookie and token"){
      val service = getAccountService
      var account = AccountData().copy(phone = Some(phoneNumber))

      val cookie = Cookie("123")
      val token = Token("1", "2", 3)

      (storage.findByPhone _).expects(*).once().returning(Future.successful(Some(account)))
      (loginClient.login _).expects(*, PhoneCredentials(phoneNumber, Some(confirmationCode))).once().returning(CancellableFuture.successful(Right(token, Some(cookie))))
      (storage.update _).expects(*, *).once().onCall{ (_, updater) =>
        account = updater(account)

        account.phone.shouldBe(Some(phoneNumber))
        account.pendingPhone.shouldBe(None)
        account.regWaiting.shouldBe(false)
        account.code.shouldBe(None)
        account.cookie.shouldBe(Some(cookie))
        account.accessToken.shouldBe(Some(token))

        Future.successful(Some((account, account)))
      }
      (storage.get _).expects(account.id).anyNumberOfTimes().returning(Future.successful(Some(account)))

      result(service.loginPhone(phoneNumber, confirmationCode)).shouldBe(Right(()))
    }
  }

  feature("Email registration") {

    val name = "Whisker Pants"
    val email = EmailAddress("whisker.pants@wire.com")
    val password = "12345678"

    scenario("Attempting registration should send a request with the data and create the appropriate account data") {
      val service = getAccountService

      val cookie = Cookie("cookie")
      var account = AccountData()

      (storage.findByEmail _).expects(email).once().returning(Future.successful(None))
      (regClient.register _).expects(*, EmailCredentials(email, Some(password)), name, *).once().returning(CancellableFuture.successful(Right(UserInfo(UserId()), Some(cookie))))
      (storage.updateOrCreate _).expects(*, *, *).once.onCall{ (id, updater, creator) =>

        updater(AccountData(id)).shouldBe(creator)

        creator.pendingEmail.shouldBe(Some(email))
        creator.email.shouldBe(None)
        creator.hash.nonEmpty.shouldBe(true)
        creator.regWaiting.shouldBe(true)

        account = creator
        Future.successful(creator)
      }

      (storage.update _).expects(*, *).once.onCall{ (_, updater) =>
        account = updater(account)

        account.email.shouldBe(Some(email))
        account.pendingEmail.shouldBe(None)
        account.hash.nonEmpty.shouldBe(true)
        account.regWaiting.shouldBe(false)
        account.cookie.shouldBe(Some(cookie))

        Future.successful(Some((account, account)))
      }
      (storage.get _).expects(*).anyNumberOfTimes().onCall{ (id: AccountId) =>
        if (id == account.id)
          Future.successful(Some(account))
        else
          Future.successful(Some(AccountData(id)))
      }

      result(service.registerEmail(email, password, name)).shouldBe(Right(()))
    }
  }

  feature("Email login") {

    val email = EmailAddress("whisker.pants@wire.com")
    val password = "12345678"

    scenario("Login with a new email should create a the appropriate account data"){
      val service = getAccountService

      val cookie = Cookie("cookie")
      val token = Token("1", "2", 3)
      var account = AccountData()

      (storage.findByEmail _).expects(email).once().returning(Future.successful(None))
      (loginClient.login _).expects(*, EmailCredentials(email, Some(password))).returning(CancellableFuture.successful(Right((token, Some(cookie)))))
      (storage.updateOrCreate _).expects(*, *, *).once.onCall{ (id, updater, creator) =>

        updater(AccountData(id)).shouldBe(creator)

        creator.email.shouldBe(Some(email))
        creator.pendingEmail.shouldBe(None)
        creator.hash.nonEmpty.shouldBe(true)
        creator.regWaiting.shouldBe(false)
        account.cookie.shouldBe(None)
        account.accessToken.shouldBe(None)

        account = creator
        Future.successful(creator)
      }

      (storage.update _).expects(*, *).once.onCall{ (_, updater) =>
        account = updater(account)

        account.email.shouldBe(Some(email))
        account.pendingEmail.shouldBe(None)
        account.hash.nonEmpty.shouldBe(true)
        account.regWaiting.shouldBe(false)
        account.cookie.shouldBe(Some(cookie))
        account.accessToken.shouldBe(Some(token))

        Future.successful(Some((account, account)))
      }
      (storage.get _).expects(*).anyNumberOfTimes().onCall{ (id: AccountId) =>
        if (id == account.id)
          Future.successful(Some(account))
        else
          Future.successful(Some(AccountData(id)))
      }

      result(service.loginEmail(email, password)).shouldBe(Right(()))
    }

  }

  def getAccountService: AccountsService = {
    val prefs = new TestGlobalPreferences()

    (globalModule.accountsStorage _).expects().anyNumberOfTimes.returning(storage)
    (globalModule.phoneNumbers _).expects().anyNumberOfTimes.returning(phoneNumbers)
    (globalModule.regClient _).expects().anyNumberOfTimes.returning(regClient)
    (globalModule.loginClient _).expects().anyNumberOfTimes.returning(loginClient)
    (globalModule.prefs _).expects().anyNumberOfTimes.returning(prefs)
    (globalModule.factory _).expects().anyNumberOfTimes.returning(new ZMessagingFactory(globalModule))
    (globalModule.context _).expects().anyNumberOfTimes().returning(null)
    (globalModule.lifecycle _).expects().anyNumberOfTimes().returning(new ZmsLifecycle)

    (phoneNumbers.normalize _).expects(*).anyNumberOfTimes().onCall { p: PhoneNumber => Future.successful(Some(p)) }

    (storage.list _).expects().anyNumberOfTimes().returning(Future.successful(Seq.empty[AccountData]))
    (storage.updateAll2 _).expects(*, *).anyNumberOfTimes().returning(Future.successful(Seq()))
    (storage.onChanged _).expects().anyNumberOfTimes().returning(new EventStream[Seq[AccountData]]())
    (storage.onDeleted _).expects().anyNumberOfTimes().returning(new EventStream[Seq[AccountId]]())
    (storage.signal _).expects(*).anyNumberOfTimes().returning(Signal.empty[AccountData])
    (storage.optSignal _).expects(*).anyNumberOfTimes().returning(Signal.empty[Option[AccountData]])

    new AccountsService(globalModule)
  }
}
