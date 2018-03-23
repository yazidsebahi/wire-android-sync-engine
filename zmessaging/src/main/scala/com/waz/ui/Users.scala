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
package com.waz.ui

import com.waz.Control.getOrUpdate
import com.waz.api.impl._
import com.waz.model._

class Users(implicit ui: UiModule) {

  val users = new UiCache[UserId, User](lruSize = 50)

  def getUser(id: UserId): User = getOrUpdate(users)(id, new User(id))

  def getUser(data: UserData): User = getOrUpdate(users)(data.id, new User(data.id, data))

  def requestVerificationEmail(email: EmailAddress): Unit = ui.accounts.requestVerificationEmail(email)
}
