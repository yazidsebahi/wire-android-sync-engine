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

import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.model._
import com.waz.model.messages.LikingMessage
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.messages.LikingsService
import com.waz.sync.SyncResult
import com.waz.sync.client.MessagesClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.utils._

import scala.concurrent.Future

class LikingsSyncHandler(client: MessagesClient, convs: ConversationsContentUpdater, service: LikingsService, otrSync: OtrSyncHandler) {

  private implicit val logTag: LogTag = logTagFor[LikingsSyncHandler]
  import com.waz.threading.Threading.Implicits.Background

  def postLiking(id: ConvId, liking: Liking): Future[SyncResult] =
    convs.convById(id) flatMap {
      case Some(conv) =>
        otrSync.postOtrMessage(conv, LikingMessage(liking)) flatMap {
          case Right(time) =>
            service.processLiking(Seq(liking.copy(timestamp = time.instant))) map (_ => SyncResult.Success)
          case Left(error) =>
            Future.successful(SyncResult(error))
        }
      case None =>
        HockeyApp.saveException(new Exception("postLiking failed, couldn't find conversation"), s"convId: $id, liking: $liking")
        Future.successful(SyncResult.failed())
    }
}
