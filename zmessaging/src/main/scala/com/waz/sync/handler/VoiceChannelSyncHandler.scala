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

import com.waz.ZLog._
import com.waz.content.DefaultConversationStorage
import com.waz.model.ConvId
import com.waz.service.call.VoiceChannelService
import com.waz.sync.SyncResult
import com.waz.sync.client.VoiceChannelClient
import com.waz.threading.Threading

import scala.concurrent.Future

class VoiceChannelSyncHandler(client: VoiceChannelClient, callsService: VoiceChannelService, conversations: DefaultConversationStorage) {

  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[VoiceChannelSyncHandler]

  def syncCallState(id: ConvId, fromFreshNotification: Boolean): Future[SyncResult] = conversations.get(id) flatMap {
    case Some(conv) =>
      client.loadCallState(conv.remoteId).future flatMap {
        case Right(event) => callsService.handleCallStateEvent(if (fromFreshNotification) event.withCurrentLocalTime() else event) map (_ => SyncResult.Success)
        case Left(errorResp) =>
          error(s"loadCallState($id) failed")
          Future.successful(SyncResult(errorResp))
      }
    case None =>
      error(s"syncCallState failed - no conversation data found for id: $id")
      Future.successful(SyncResult.failed())
  }
}
