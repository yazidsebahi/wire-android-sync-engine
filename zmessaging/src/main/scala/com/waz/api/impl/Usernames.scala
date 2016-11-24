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

import com.waz.api
import com.waz.api.{UsernameValidation, UsernameValidationError, UsernamesRequestCallback}

object Usernames {
  val MAX_LENGTH = 21
  val MIN_LENGTH = 2
}

class Usernames extends api.Usernames{
  override def isUsernameAvailable(username: String, callback: UsernamesRequestCallback) = {
    //TODO: STUB
    callback.onUsernameRequestResult(username, UsernameValidation(isValid = true, UsernameValidationError.NONE))
  }
  override def isUsernameValid(username: String): UsernameValidation = {
    val usernameRegex = s"""^([a-z]|[0-9]|_|\\.){${Usernames.MIN_LENGTH},${Usernames.MAX_LENGTH}}$$""".r

    if (username.length  > Usernames.MAX_LENGTH) {
      return UsernameValidation(isValid = false, UsernameValidationError.TOO_LONG)
    }
    if (username.length  < Usernames.MIN_LENGTH) {
      return UsernameValidation(isValid = false, UsernameValidationError.TOO_SHORT)
    }
    username match {
      case usernameRegex(_) => UsernameValidation(isValid = true, UsernameValidationError.NONE)
      case _ => UsernameValidation(isValid = false, UsernameValidationError.INVALID_CHARACTERS)
    }
  }
}
