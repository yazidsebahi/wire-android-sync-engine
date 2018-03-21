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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api._
import com.waz.model._
import com.waz.service.AccountManager
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.Signal

class Self()(implicit ui: UiModule) extends com.waz.api.Self with UiObservable with SignalLoading {

  var data = Option.empty[AccountData]

  private def users = ui.users

  private var user = Option.empty[User]

  private val userUpdateListener = new UpdateListener {
    override def updated(): Unit = {
      verbose(s"self user changed: $user")
      notifyChanged()
    }
  }

  def signal(acc: Option[AccountManager]): Signal[Option[AccountData]] = acc match {
    case Some(a) => a.accountData.map(Option(_))
    case None    => Signal.const(Option.empty[AccountData])
  }

  accountLoaderOpt(signal) { acc =>
    verbose(s"update($acc)")
    val previousState = (data, user)
    this.data = acc
    if (user.map(_.data.id) != userId) {
      user.foreach(_.removeUpdateListener(userUpdateListener))
      user = userId.map(users.getUser)
      user.foreach(_.addUpdateListener(userUpdateListener))
    }
    if (previousState != (data, user)) notifyChanged()
  }

  def userId = data.flatMap(_.userId)

  override def getClientRegistrationState = data.map(_.clientRegState).getOrElse(ClientRegistrationState.UNKNOWN)

  override def resendVerificationEmail(email: String): Unit = users.requestVerificationEmail(EmailAddress(email))

}
