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
package com.waz.sync.client

import com.waz.api.{UsernameValidation, UsernameValidationError}
import com.waz.model.{Handle, UserData}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonDecoder
import com.waz.znet.Response.{HttpStatus, Status, SuccessHttpStatus}
import com.waz.znet._
import com.waz.znet.ZNetClient._

import scala.util.Try
import scala.util.control.NonFatal

object HandlesClient {
  val checkMultipleAvailabilityPath = "/users"
  val checkSingleAvailabilityPath = "/users/handles/"
  val handlesQuery = "handles"
}

class HandlesClient(netClient: ZNetClient)  {
  def getHandlesValidation(handles: Seq[Handle]): ErrorOrResponse[Option[Seq[UsernameValidation]]] = {
    val uri = Request.query(HandlesClient.checkMultipleAvailabilityPath, (HandlesClient.handlesQuery, handles.map(_.toString).reduce(_ + "," + _)))
    netClient.withErrorHandling("getHandlesValidation", Request.Get(uri)) {
      case Response(SuccessHttpStatus(), UsersHandleResponseContent(Seq(unavailableHandles)), headers) =>
        Some(handles map (u => UsernameValidation(u.toString, if (unavailableHandles.contains(u)) UsernameValidationError.ALREADY_TAKEN else UsernameValidationError.NONE)))
      case Response(HttpStatus(Status.NotFound, _), _, _) => Some(handles.map(u => UsernameValidation(u.toString, UsernameValidationError.NONE)))
    }(Threading.Background)
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
