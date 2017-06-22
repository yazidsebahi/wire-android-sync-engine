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

import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api
import com.waz.content.Uris.SyncIndicatorUri
import com.waz.content.{MessagesCursor, Uris}
import com.waz.model._
import com.waz.model.sync.SyncCommand
import com.waz.service.ZMessaging
import com.waz.service.messages.MessageAndLikes
import com.waz.sync.SyncRequestService.SyncMatcher
import com.waz.threading.Threading
import com.waz.ui._
import com.waz.utils._
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.Future

class MessagesList(convId: ConvId)(implicit ui: UiModule) extends com.waz.api.MessagesList with CoreList[api.Message] with SignalLoading { list =>

  var lastRead = Instant.EPOCH

  private var unreadCount = 0
  private var lastMessage = Option.empty[MessageAndLikes]
  private[impl] var msgs = MessagesCursor.Empty

  private var lastAccessed = Option.empty[MessageId]

  private def signal(zms: ZMessaging) =
    Signal (
      zms.messagesStorage.getEntries(convId) flatMap { cursor =>
        Signal.future(Future(lastAccessed.fold(cursor.prefetch(cursor.lastReadIndex))(cursor.prefetchById))(Threading.Ui).flatMap(_.map(_ => cursor)(Threading.Background))(Threading.Background))
      },
      zms.messagesStorage.lastRead(convId)
    )

  addLoader(_.messagesStorage.unreadCount(convId)) { this.unreadCount = _ }

  addLoader { zms =>
    zms.messagesStorage.lastMessage(convId).flatMap(m => Signal.future(m.mapFuture(zms.msgAndLikes.combineWithLikes)(Threading.Background)))
  } { m =>
    verbose(s"lastMessage changed: $m")
    this.lastMessage = m
  }

  addLoader(signal _) { case (cursor, lastReadTime) =>
    verbose(s"onLoaded($cursor, $lastReadTime")

    debug(s"changing last read from ${this.lastRead} to ${this.lastRead max lastReadTime}")
    this.lastRead = this.lastRead max lastReadTime

    if (cursor ne msgs) {
      msgs.close()
      msgs = cursor
      notifyChanged()
    }
  }

  override def size: Int = msgs.size

  override def get(index: Int): api.Message = {
    val data = msgs(index)
    lastAccessed = Some(data.message.id)
    updateLastRead(data.message)
    ui.messages.cachedOrUpdated(data, userAction = true)
  }

  private[waz] def updateLastRead(msg: MessageData) = if (lastRead.isBefore(msg.time)) {
    debug(s"updateLastRead from $lastRead to $msg")
    lastRead = msg.time
    ui.messages.updateLastRead(convId, msg)
  }

  override def getUnreadCount: Int = unreadCount

  override def getLastMessage: Message = lastMessage.map(ui.messages.cachedOrUpdated(_)).orNull

  override def getLastReadIndex: Int = {
    val ind = if (lastRead == Instant.EPOCH) msgs.lastReadIndex else msgs.indexOf(lastRead)
    verbose(s"getLastReadIndex [$convId] - $ind, count: $size")
    if (ind < 0) msgs.lastReadIndex else ind
  }

  override def getMessageIndex(m: api.Message): Int = {
    require(m != null, "null messages don't have an index")
    val msg = m.asInstanceOf[Message]
    val index = msgs.indexOf(msg.data.time)
    verbose(s"getMessageIndex: $index, msg: $msg")
    index
  }

  override def getSyncIndicator = ui.cached(SyncIndicatorUri(Uris.MessagesUri(convId)), new SyncIndicator(SyncMatcher(SyncCommand.SyncConversation, Some(convId))))
}
