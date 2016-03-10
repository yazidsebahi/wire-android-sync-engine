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

import android.content.Context
import com.waz.ZLog._
import com.waz.api.impl.{AccentColor, Credentials, ErrorResponse, PhoneCredentials}
import com.waz.api.{KindOfAccess, KindOfVerification}
import com.waz.model.{EmailAddress, PhoneNumber, ZUser, ZUserId}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.Signal
import com.waz.znet.AuthenticationManager.Token
import com.waz.znet.ZNetClient._

import scala.collection.mutable
import scala.concurrent.Future


class InstanceService(context: Context, val global: GlobalModule, val factory: ZMessaging.Factory) extends UserDataCallbacks {
  import InstanceService._
  implicit val dispatcher = new SerialDispatchQueue(name = "InstanceService")
  import global._

  private[waz] val instanceMap = new mutable.HashMap[ZUserId, ZMessaging]()

  val currentUserPref = prefs.preferenceStringSignal(CurrentUserPref)

  lazy val currentUser = currentUserPref.signal flatMap {
    case "" => Signal.const(Option.empty[ZUser])
    case id => Signal.future(users.getById(ZUserId(id)))
  }

  lazy val current = currentUser flatMap {
    case Some(user) =>
      debug(s"Loaded current user: ${user.email}, future: $currentUser")
      Signal.future(getInstance(user) map (Option(_)))
    case None =>
      info(s"No current user found in ZUsers db")
      Signal.const(Option.empty[ZMessaging])
  }

  def getCurrent: Future[Option[ZMessaging]] = currentUserPref() flatMap {
    case "" => Future.successful(None)
    case id => users.getById(ZUserId(id)).future flatMap {
      case Some(user) => getInstance(user) map (Option(_))
      case None => Future.successful(None)
    }
  }

  def getInstance(user: ZUser, token: Option[Token] = None) = Future {
    verbose(s"getInstance($user, $token)")
    val zms = instanceMap.getOrElseUpdate(user.id, factory(this, user, token))
    zms.user.update(user)
    zms
  }

  def login(credentials: Credentials): Future[Either[ErrorResponse, ZMessaging]] = for {
    _ <- currentUserPref := ""
    res <- users.login(credentials).future flatMap {
      case Left(error) => Future.successful(Left(error))
      case Right((user, token)) =>
        getInstance(user, token) flatMap { zms =>
          currentUserPref := zms.userId.str
          if (zms.user.user.emailVerified) {
            zms.user.onVerifiedLogin ! Some(zms.userId)
            zms.otrClientsService.awaitClientRegistered() map { _ => Right(zms) }
          } else
            Future successful Right(zms)
        }
    }
  } yield res

  def register(credentials: Credentials, name: String, accent: AccentColor): Future[Either[ErrorResponse, ZMessaging]] = {
    debug(s"register($credentials, $name, $accent")

    users.register(credentials, name, accent).future flatMap {
      case Left(error) =>
        info(s"register($credentials) failed: $error")
        Future.successful(Left(error))
      case Right((zuser, userInfo)) =>
        for {
          zms <- getInstance(zuser)
          _ <- zms.users.updateSelf(userInfo, emailVerified = false, phoneVerified = credentials.maybePhone.isDefined)
          _ <- if (credentials.autoLoginOnRegistration) {
                zms.user.onVerifiedLogin ! Some(zuser.id)
                zms.otrClientsService.awaitClientRegistered()
              } else Future.successful(())
          _ <- currentUserPref := zuser.id.str
        } yield Right(zms)
    }
  }

  def requestPhoneConfirmationCode(phone: PhoneNumber, kindOfAccess: KindOfAccess): ErrorOrResponse[Unit] = {
    CancellableFuture.lift(phoneNumbers.normalize(phone)) flatMap { normalizedPhone =>
      regClient.requestPhoneConfirmationCode(normalizedPhone.getOrElse(phone), kindOfAccess)
    }
  }

  def verifyPhoneNumber(phone: PhoneCredentials, kindOfVerification: KindOfVerification): ErrorOrResponse[Unit] = {
    CancellableFuture.lift(phoneNumbers.normalize(phone.phone)) flatMap { normalizedPhone =>
      regClient.verifyPhoneNumber(PhoneCredentials(normalizedPhone.getOrElse(phone.phone), phone.code), kindOfVerification)
    }
  }

  def updateZUserEmail(user: ZUser, email: EmailAddress, verified: Boolean): Unit = {
    debug(s"updateZUserEmail($user, $email, $verified")
    if (!user.email.contains(email) || user.emailVerified != verified) {
      users.updateEmail(user.id, email, verified)
      current.currentValue.foreach(_.foreach { _.user.update(user.copy(email = Some(email), emailVerified = verified)) })
    }
  }

  def updateZUserPhone(user: ZUser, phone: PhoneNumber, verified: Boolean): Unit = {
    debug(s"updateZUserPhone($user, $phone, $verified)")
    if (!user.phone.contains(phone) || user.phoneVerified != verified) {
      users.updatePhone(user.id, phone, verified)
      current.currentValue.foreach(_.foreach { _.user.update(user.copy(phone = Some(phone), phoneVerified = verified)) })
    }
  }

  def logout(user: ZUserId) = currentUserPref() flatMap {
    case id if id == user.str =>
      info(s"logout $user")
      current.currentValue.foreach(_.foreach(_.user.onLogout ! {()} ))
      currentUserPref := ""
    case _ =>
      Future.successful(())
  }
}

object InstanceService {
  private implicit val logTag: LogTag = logTagFor[InstanceService]

  val CurrentUserPref = "CurrentUserPref"
}
