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
package com.waz.content

import java.util.concurrent.TimeUnit

import android.content.Context
import com.waz.ZLog._
import com.waz.api.ContentSearchQuery
import com.waz.model._
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class MessageIndexStorage(context: Context, storage: ZmsDatabase, messagesStorage: MessagesStorage, loader: MessageAndLikesStorage) {
  import MessageIndexStorage._
  import MessageContentIndex._
  import com.waz.utils.events.EventContext.Implicits.global

  private implicit val logTag: LogTag = logTagFor[MessageIndexStorage]
  private implicit val dispatcher = new SerialDispatchQueue(name = "MessageIndexStorage")

  def updateOutdated(): CancellableFuture[Boolean] = {
    storage{ MessageContentIndexDao.updateOldMessages()(_)}.flatMap{
      case false => CancellableFuture.delay(UpdateOldMessagesThrottle).flatMap(_ => updateOutdated())
      case _ =>  CancellableFuture.successful(true)
    }
  }

  val finishedIndexing = Signal.future(updateOutdated()).orElse(Signal(true))

  messagesStorage.onAdded{ added =>
    storage(MessageContentIndexDao.addMessages(added.filter(m => TextMessageTypes.contains(m.msgType) && !m.isEphemeral))(_))
  }

  messagesStorage.onUpdated{ updated =>
    storage(MessageContentIndexDao.updateMessages(updated.filter(m => TextMessageTypes.contains(m._2.msgType) && !m._2.isEphemeral))(_))
  }

  messagesStorage.onDeleted{ removed =>
    storage(MessageContentIndexDao.removeMessages(removed)(_))
  }

  def searchText(contentSearchQuery: ContentSearchQuery, convId: Option[ConvId]): Future[MessagesCursor] =
    storage(MessageContentIndexDao.findContent(contentSearchQuery, convId)(_)).map(c => new MessagesCursor(c, 0, Instant.now, loader))

  def getNormalizedContentForMessage(messageId: MessageId): Future[Option[String]] ={
    storage(MessageContentIndexDao.getById(messageId)(_).map(_.content))
  }
}

object MessageIndexStorage{
  val UpdateOldMessagesThrottle = FiniteDuration(1, TimeUnit.SECONDS)
}
