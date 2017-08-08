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

import com.waz.api.impl.PhoneCredentials
import com.waz.api.{KindOfAccess, KindOfVerification}
import com.waz.client.RegistrationClient
import com.waz.client.RegistrationClientImpl.ActivateResult
import com.waz.content.Preferences.{PrefKey, Preference}
import com.waz.content.Preferences.Preference.PrefCodec
import com.waz.content.{AccountsStorage, GlobalPreferences, Preferences}
import com.waz.model._
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.CancellableFuture
import com.waz.utils.returning
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import com.waz.znet.LoginClient

import scala.concurrent.{ExecutionContext, Future}

class AccountsServiceSpec extends AndroidFreeSpec {

  private val globalModule = mock[GlobalModule]
  private val storage = mock[AccountsStorage]
  private val phoneNumbers = mock[PhoneNumberService]
  private val regClient = mock[RegistrationClient]
  private val loginClient = mock[LoginClient]
  private val prefs = mock[GlobalPreferences]

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

  def getAccountService: AccountsService = {

    val mockPrefs = new Preferences {

      private var map = Map[AnyRef, AnyRef]()

      override protected def getValue[A: PrefCodec](key: PrefKey[A]): Future[A] = {
        Future.successful(map.getOrElse(key, key.default).asInstanceOf[A])
      }

      override protected def setValue[A: PrefCodec](key: PrefKey[A], value: A): Future[Unit] ={
        map = map ++ Map(key -> value.asInstanceOf[AnyRef])
        Future.successful(())
      }

      override implicit protected val dispatcher: ExecutionContext = ExecutionContext.global
    }

    (globalModule.accountsStorage _).expects().anyNumberOfTimes.returning(storage)
    (globalModule.phoneNumbers _).expects().anyNumberOfTimes.returning(phoneNumbers)
    (globalModule.regClient _).expects().anyNumberOfTimes.returning(regClient)
    (globalModule.loginClient _).expects().anyNumberOfTimes.returning(loginClient)
    (globalModule.prefs _).expects().anyNumberOfTimes.returning(prefs)
/*
    (prefs.preference[Boolean] (_: PrefKey[Boolean])(_: PrefCodec[Boolean])).expects(*, *).anyNumberOfTimes().onCall {
      (key: PrefKey[Boolean], _ : PrefCodec[Boolean]) =>
        new Preference[Boolean](mockPrefs, key)
    }
*/
    (phoneNumbers.normalize _).expects(*).anyNumberOfTimes().onCall { p: PhoneNumber => Future.successful(Some(p)) }

    new AccountsService(globalModule)
  }
}
