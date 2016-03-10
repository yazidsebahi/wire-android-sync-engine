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
import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog._
import com.waz.api.impl.{AccentColor, Credentials, EmailCredentials, ErrorResponse, PhoneCredentials}
import com.waz.client.RegistrationClient
import com.waz.content.Database
import com.waz.model.ZUser.ZUserDao
import com.waz.model._
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.AuthenticationManager._
import com.waz.znet.LoginClient
import com.waz.znet.ZNetClient.ErrorOrResponse

class ZUsers(context: Context, storage: Database, loginClient: LoginClient, regClient: RegistrationClient, prefs: PreferenceService, phoneNumbers: PhoneNumberService) {
  import Threading.Implicits.Background

  private implicit val logTag: LogTag = logTagFor[ZUsers]

  def getById(id: ZUserId): CancellableFuture[Option[ZUser]] = storage { ZUserDao.getById(id)(_) } ("ZUsers.getById")

  def find(credentials: Credentials): CancellableFuture[Option[ZUser]] = storage (implicit db => credentials match {
    case EmailCredentials(email, Some(password), _) =>
      ZUserDao.findByEmail(email) filter { u =>
        verbose(s"found $u")
        u.hash == ZUser.computeHash(u.id, password)
      } map (_.copy(password = Some(password)))
    case EmailCredentials(_, None, _) => None
    case PhoneCredentials(_, _, _) => None // always force backend login for phone-based credentials
  })

  def create(id: ZUserId, credentials: Credentials, cookie: Option[String], verified: Boolean): CancellableFuture[ZUser] = (credentials match {
    case EmailCredentials(email, Some(password), _) =>
      storage { ZUserDao.findByEmail(email)(_) } map {
        case Some(user) => user.copy(hash = ZUser.computeHash(user.id, password), emailVerified = verified, cookie = cookie.orElse(user.cookie), password = Some(password))
        case None =>
          ZUser(id, Some(email), ZUser.computeHash(id, password), phone = None, verified, phoneVerified = false, cookie, Some(password))
      }
    case EmailCredentials(email, None, _) =>
      error("creating zusers should always include passwords")
      CancellableFuture.successful(ZUser(ZUserId(), Some(email), "", phone = None, verified))
    case PhoneCredentials(phone, _, _) =>
      storage { ZUserDao.findByPhone(phone)(_) } map {
        case Some(user) => user.copy(phoneVerified = verified, cookie = cookie.orElse(user.cookie))
        case None =>
          val id = ZUserId()
          ZUser(id, email = None, hash = "", phone = Some(phone), emailVerified = false, phoneVerified = verified, cookie, password = None)
      }
    }) flatMap (user => storage { ZUserDao.insertOrReplace(user)(_) })

  def createNew(id: ZUserId, credentials: Credentials, cookie: Cookie): ErrorOrResponse[ZUser] = credentials match {
    case EmailCredentials(email, Some(password), _) => addOrReplace(ZUser(email, password).copy(id = id, cookie = cookie), ZUserDao.findByEmail(email)(_)) map (Right(_))
    case EmailCredentials(email, None, _) =>
      error("creating zusers with email should always include passwords")
      CancellableFuture.successful(Left(ErrorResponse.internalError("Password missing for email registration")))
    case PhoneCredentials(phone, _, _) => addOrReplace(ZUser(id, None, "", Some(phone), cookie = cookie), ZUserDao.findByPhone(phone)(_)) map (Right(_))
  }

  private def addOrReplace(zuser: => ZUser, finder: SQLiteDatabase => Option[ZUser]): CancellableFuture[ZUser] = Threading.Background(zuser) flatMap { newUser =>
    storage.withTransaction { implicit db =>
      finder(db) foreach { user =>
        // TODO: we should somehow delete zmessaging db for this user, this should be done in some cleanup service, since it could be used by sync service
        ZUserDao.delete(user.id)
      }
      ZUserDao.insertOrReplace(newUser)
    }
  }

  def listUsers() = storage { ZUserDao.list(_) }

  def updateCookie(user: ZUser, cookie: Option[String]) = storage { implicit db =>
    verbose(s"updateCookie($user, cookie.isDefined = ${cookie.isDefined}")
    ZUserDao.update(user)(_.copy(cookie = cookie, emailVerified = user.emailVerified || cookie.isDefined))
  }

  def updateEmail(id: ZUserId, email: EmailAddress, verified: Boolean) = storage { implicit db =>
    debug(s"updateEmail($id, $email, $verified)")
    ZUserDao.updateById(id)(_.copy(email = Some(email), emailVerified = verified))
  } ("ZUsers.updateEmail")

  def updatePhone(id: ZUserId, phone: PhoneNumber, verified: Boolean) = storage { implicit db =>
    debug(s"updatePhone($id, $phone, $verified)")
    ZUserDao.updateById(id)(_.copy(phone = Some(phone), phoneVerified = verified))
  } ("ZUsers.updatePhone")

  def resetCredentials(user: ZUser) = storage.withTransaction { implicit db =>
    ZUserDao.update(user)(_.copy(hash = "", cookie = None))
  }

  def login(credentials: Credentials): ErrorOrResponse[(ZUser, Option[Token])] = {
    debug(s"login($credentials})")

    normalizeCredentials(credentials) flatMap { normalized =>
      find(normalized) flatMap {
        case Some(user) =>
          CancellableFuture.successful(Right((user, None: Option[Token])))
        case None =>
          val id = ZUserId()
          loginClient.login(id, normalized) flatMap {
            case Right((token, cookie)) =>
              create(id, normalized, cookie, verified = true) map { user =>
                Right((user, Some(token)))
              }
            case Left(err@ErrorResponse(403, _, "pending-activation")) =>
              create(id, normalized, None, verified = false) map { user =>
                Right((user, None: Option[Token]))
              }
            case Left(error) =>
              CancellableFuture.successful(Left(error))
          }
      }
    }
  }

  def register(credentials: Credentials, name: String, accent: AccentColor): ErrorOrResponse[(ZUser, UserInfo)] = {
    debug(s"register($credentials, $name, $accent")
    val userId = ZUserId()
    normalizeCredentials(credentials) flatMap { normalized =>
      regClient.register(userId, normalized, name, Some(accent.id)) flatMap {
        case Right((userInfo, cookie)) =>
          createNew(userId, normalized, cookie) map {
            case Right(zuser) => Right((zuser, userInfo))
            case Left(err) => Left(err)
          }
        case Left(error) =>
          info(s"register($normalized, $name) failed: $error")
          CancellableFuture.successful(Left(error))
      }
    }
  }

  private def normalizeCredentials(credentials: Credentials): CancellableFuture[Credentials] = credentials match {
    case cs @ PhoneCredentials(p, _, _) =>
      CancellableFuture.lift(phoneNumbers.normalize(p) map { normalized => cs.copy(phone = normalized.getOrElse(p)) })
    case other => CancellableFuture.successful(other)
  }
}
