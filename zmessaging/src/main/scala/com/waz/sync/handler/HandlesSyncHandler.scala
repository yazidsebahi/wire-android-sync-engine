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
package com.waz.sync.handler

import com.waz.api.UsernameValidation
import com.waz.model.Handle
import com.waz.service.HandlesService
import com.waz.sync.SyncResult
import com.waz.sync.client.HandlesClient
import com.waz.threading.Threading
import com.waz.utils.events.Signal

import scala.concurrent.Future

class HandlesSyncHandler(handlesClient: HandlesClient, handlesService: HandlesService) {
  import Threading.Implicits.Background

  val responseSignal = Signal(Seq.empty[UsernameValidation])

  def validateHandles(handles: Seq[Handle]): Future[SyncResult] = {
    handlesClient.getHandlesValidation(handles).future map {
      case Right(data) =>
        responseSignal ! data.getOrElse(Seq())
        handlesService.updateValidatedHandles(data.getOrElse(Seq()))
        SyncResult.Success
      case Left(error) => SyncResult.Failure(Some(error), shouldRetry = true)
    }
  }
}
