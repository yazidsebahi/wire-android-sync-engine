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

import java.text.Normalizer

import android.content.Context
import com.waz.ZLog._
import com.waz.api
import com.waz.api.{UsernameValidation, UsernameValidationError, UsernamesRequestCallback, ValidatedUsernames}
import com.waz.model.sync.SyncRequest.ValidateHandles
import com.waz.model.{Handle, UserData}
import com.waz.threading.Threading
import com.waz.ui.UiModule
import com.waz.utils.JsonDecoder
import com.waz.zms.R
import com.waz.znet.Response.{HttpStatus, Status, SuccessHttpStatus}
import com.waz.znet._

import scala.util.{Random, Try}
import scala.util.control.NonFatal

object Usernames {
  val MAX_LENGTH = 21
  val MIN_LENGTH = 2
  val checkMultipleAvailabilityPath = "/users"
  val checkSingleAvailabilityPath = "/users/handles/"
  val handlesQuery = "handles"
}

class Usernames()(implicit ui: UiModule) extends api.Usernames{
  private implicit val tag: LogTag = s"Usernames"

  override def isUsernameAvailable(username: String, callback: UsernamesRequestCallback) = {
    ui.zms(_.zNetClient.withErrorHandling("isUsernameAvailable", Request.Head(Usernames.checkSingleAvailabilityPath + username)) {
      case Response(SuccessHttpStatus(), _, _) => callback.onUsernameRequestResult(Array(username).map(u => UsernameValidation(u, UsernameValidationError.ALREADY_TAKEN)))
      case Response(HttpStatus(Status.NotFound, _), _, _) => callback.onUsernameRequestResult(Array(username).map(u => UsernameValidation(u, UsernameValidationError.NONE)))
      case Response(HttpStatus(status, _), _, _) => callback.onRequestFailed(status)
      case _ => callback.onRequestFailed(499)
    }(Threading.Ui))
  }

  override def isUsernameValid(username: String): UsernameValidation = {
    val usernameRegex = s"""^([a-z]|[0-9]|_){${Usernames.MIN_LENGTH},${Usernames.MAX_LENGTH}}$$""".r

    if (username.length  > Usernames.MAX_LENGTH) {
      return UsernameValidation(username = username, UsernameValidationError.TOO_LONG)
    }
    if (username.length  < Usernames.MIN_LENGTH) {
      return UsernameValidation(username = username, UsernameValidationError.TOO_SHORT)
    }
    username match {
      case usernameRegex(_) => UsernameValidation(username = username, UsernameValidationError.NONE)
      case _ => UsernameValidation(username = username, UsernameValidationError.INVALID_CHARACTERS)
    }
  }

  override def generateUsernameFromName(name: String, context: Context): String = {
    var cleanName: String = replaceOtherCases(name.toLowerCase)
    cleanName = Normalizer.normalize(cleanName, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    cleanName = Normalizer.normalize(cleanName, Normalizer.Form.NFD).replaceAll("\\W+", "")
    if (cleanName.isEmpty) {
      cleanName = generateFromDictionary(context)
    }
    cleanName
  }

  override def validateUsernames(usernames: Array[String]): Unit = {
    ui.zms(_.sync.postValidateHandles(usernames.map(Handle(_))))
  }

  override def getValidatedUsernames: ValidatedUsernames = new ValidatedHandles()

  private def replaceOtherCases(input: String): String = {
    var output: String = input
    output = output.replace("ł", "l")
    output = output.replace("æ", "a")
    output = output.replace("ø", "o")
    output
  }

  private def generateFromDictionary(context: Context): String = {
    if (context == null) { return "" }
    val names: Array[String] = context.getResources.getStringArray(R.array.random_names)
    val adjectives: Array[String] = context.getResources.getStringArray(R.array.random_adjectives)
    val namesIndex: Int = Random.nextInt(names.length)
    val adjectivesIndex: Int = Random.nextInt(adjectives.length)
    (adjectives(adjectivesIndex) + names(namesIndex)).toLowerCase
  }
}

object UsersHandleResponseContent {
  def unapply(response: ResponseContent): Option[Seq[String]] = {
    try {
      response match {
        case JsonArrayResponse(js) => Try(JsonDecoder.array[UserData](js).map(user => user.handle.getOrElse(Handle("")).toString)).toOption
        case _ => None
      }
    } catch {
      case NonFatal(_) =>  None
    }
  }
}
