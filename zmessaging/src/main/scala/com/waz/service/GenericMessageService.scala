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

import com.waz.ZLog._
import com.waz.model.GenericContent._
import com.waz.model._
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater}
import com.waz.service.messages.{LikingsService, MessagesContentUpdater}
import com.waz.utils._
import org.threeten.bp.Instant

import scala.concurrent.Future.traverse

class GenericMessageService(messages: MessagesContentUpdater, convs: ConversationsContentUpdater, convEvents: ConversationEventsService, images: AssetService, likings: LikingsService) {
  private implicit val tag: LogTag = logTagFor[GenericMessageService]
  import com.waz.threading.Threading.Implicits.Background

  val eventProcessingStage = EventScheduler.Stage[GenericMessageEvent] { (convId, events) =>

    def lastForConv(items: Seq[(RConvId, Instant)]) = items.groupBy(_._1).map { case (conv, times) => times.maxBy(_._2.toEpochMilli) }

    val likes = events collect {
      case GenericMessageEvent(_, _, time, from, GenericMessage(_, Reaction(action, msg))) => Liking(msg, from, time.instant, action)
    }

    val lastRead = lastForConv(events collect {
      case GenericMessageEvent(_, _, _, _, GenericMessage(_, LastRead(conv, time))) => (conv, time)
    })

    val cleared = lastForConv(events collect {
      case GenericMessageEvent(_, _, _, _, GenericMessage(_, Cleared(conv, time))) => (conv, time)
    })

    val deleted = events collect {
      case GenericMessageEvent(_, _, _, _, GenericMessage(_, MsgDeleted(_, msg))) => msg
    }

    for {
      _ <- messages.deleteOnUserRequest(deleted)
      _ <- traverse(lastRead) { case (remoteId, timestamp) =>
        convs.processConvWithRemoteId(remoteId, retryAsync = true) { conv => convs.updateConversationLastRead(conv.id, timestamp) }
      }
      _ <- likings.processLiking(likes)
      _ <- traverse(cleared) { case (remoteId, timestamp) =>
        convs.processConvWithRemoteId(remoteId, retryAsync = true) { conv => convs.updateConversationCleared(conv.id, timestamp) }
      }
    } yield ()
  }
}
