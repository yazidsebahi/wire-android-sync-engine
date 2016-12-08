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
package com.waz.service.call


import com.sun.jna.Pointer
import com.waz.ZLog
import com.waz.ZLog.ImplicitTag._
import com.waz.model.UserId
import com.waz.service.call.Calling.{ReadyHandler, SendHandler}
import com.waz.sync.client.OAuth2Client.ClientId
import com.waz.utils.jna.Size_t

import scala.concurrent.Promise

class CallingService(selfUserId: UserId, clientId: ClientId, callEventClient: CallEventClient) {

  private lazy val init = {
    val callingReady = Promise[Unit]()
    Calling.wcall_init(selfUserId.str, clientId.str,
      new ReadyHandler {
        override def invoke(version: Int, arg: Pointer): Unit = {
          ZLog.verbose(s"Calling ready: avs version??: $version")
          callingReady.success(())
        }
      },
      new SendHandler {
        override def invoke(ctx: Pointer, convid: String, userid: String, clientid: String, data: Pointer, len: Size_t, arg: Pointer): Int = {
//          callEventClient.messageToSend !
          0 //TODO Dean - needs to be an http response code - there is a mapping to succes code ints
        }
      },
      null, null, null, null, null
    )
    callingReady.future
  }



}
