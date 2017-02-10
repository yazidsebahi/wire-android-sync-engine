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
import com.waz.api.{ContentSearchQuery, Message}
import com.waz.model._
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.Locales
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class MessageIndexStorage(context: Context, storage: ZmsDatabase, messagesStorage: MessagesStorage, loader: MessageAndLikesStorage) {
  import MessageIndexStorage._
  import com.waz.utils.events.EventContext.Implicits.global

  private implicit val logTag: LogTag = logTagFor[MessageIndexStorage]
  private implicit val dispatcher = new SerialDispatchQueue(name = "MessageIndexStorage")

  def updateOutdated(): CancellableFuture[Boolean] = {
    storage{ MessageContentIndexDao.updateOldMessages()(_)}.flatMap{
      case false => CancellableFuture.delay(UpdateOldMessagesThrottle).flatMap(_ => updateOutdated())
      case _ =>  CancellableFuture.successful(true)
    }
  }

  val finishedIndexing = Signal.future(false, updateOutdated())

  messagesStorage.onAdded{ added =>
    storage(MessageContentIndexDao.addMessages(added.filter(m => m.msgType == Message.Type.TEXT || m.msgType == Message.Type.TEXT_EMOJI_ONLY || m.msgType == Message.Type.RICH_MEDIA))(_))
  }

  messagesStorage.onUpdated{ updated =>
    storage(MessageContentIndexDao.updateMessages(updated.filter(m => m._2.msgType == Message.Type.TEXT || m._2.msgType == Message.Type.TEXT_EMOJI_ONLY || m._2.msgType == Message.Type.RICH_MEDIA))(_))
  }

  messagesStorage.onDeleted{ removed =>
    storage(MessageContentIndexDao.removeMessages(removed)(_))
  }

  def searchText(contentSearchQuery: ContentSearchQuery, convId: Option[ConvId]): Future[MessagesCursor] ={
    convId match {
      case Some(conv) => storage(MessageContentIndexDao.findContent(contentSearchQuery.toFtsQuery, convId)(_)).map(c => new MessagesCursor(conv, c, 0, Instant.now, loader))
      //TODO: global search
    }
  }

  def getNormalizedContentForMessage(messageId: MessageId): Future[Option[String]] ={
    storage(MessageContentIndexDao.getById(messageId)(_).map(_.content))
  }
}

object MessageIndexStorage{
  val UpdateOldMessagesThrottle = FiniteDuration(1, TimeUnit.SECONDS)
}
