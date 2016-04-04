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
import com.waz.api
import com.waz.api.IncomingMessagesList.{KnockListener, MessageListener}
import com.waz.model.MessageData
import com.waz.service.conversation.ConversationsService
import com.waz.service.messages.MessageAndLikes
import com.waz.ui._
import com.waz.utils._
import org.threeten.bp.Instant

class IncomingMessages(implicit context: UiModule) extends com.waz.api.IncomingMessagesList with CoreList[api.Message] with SignalLoading {

  private implicit val logTag: LogTag = logTagFor[IncomingMessages]

  private var listeners = List[MessageListener]()
  private var knockListeners = List[KnockListener]()
  private var messages = Array[(MessageData, Int)]()
  private var lastMessageTime = Instant.EPOCH

  addLoader(_.messages.getIncomingMessages) { onLoaded }

  private[impl] def onLoaded(data: List[MessageData]): Unit = {
    verbose(s"onLoaded($data)")
    messages = data.flatMap(m => (0 until math.max(1, m.content.size)).map((m, _))).toArray
    notifyChanged()

    lastMessageTime = lastMessageTime max (Instant.now - ConversationsService.KnockTimeout)
    messages.filter(_._1.localTime isAfter lastMessageTime) foreach {
      case (msg, seq) =>
        verbose(s"calling listeners for new message: $msg")
        val message = context.messages.cachedOrUpdated(MessageAndLikes(msg, Vector.empty, likedBySelf = false))
        listeners.foreach(_.onIncomingMessage(message))
        if (msg.msgType == api.Message.Type.KNOCK) {
          knockListeners.foreach(_.onKnock(message))
        }
    }
    lastMessageTime = data.lastOption.fold(lastMessageTime)(_.localTime)
  }

  override def addKnockListener(listener: KnockListener): Unit = knockListeners ::= listener

  override def removeKnockListener(listener: KnockListener): Unit = knockListeners = knockListeners.filter(_ != listener)

  override def addMessageListener(listener: MessageListener): Unit = listeners ::= listener

  override def removeMessageListener(listener: MessageListener): Unit = listeners = listeners.filter(_ != listener)

  override def get(index: Int): Message = {
    val (msg, seq) = messages(index)
    context.messages.cachedOrUpdated(MessageAndLikes(msg, Vector.empty, likedBySelf = false))
  }

  override def size(): Int = messages.length
}
