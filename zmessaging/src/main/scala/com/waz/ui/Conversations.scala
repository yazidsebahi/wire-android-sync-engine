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
package com.waz.ui

import android.os.Parcel
import com.waz.Control.getOrUpdate
import com.waz.ZLog._
import com.waz.api.ConversationsList.VerificationStateCallback
import com.waz.api.impl.{Conversation, ConversationsList}
import com.waz.api.{IConversation, MessageContent, User}
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.EventContext
import com.waz.utils.{JsonDecoder, Serialized}

class Conversations(implicit ui: UiModule, ec: EventContext) {
  import Threading.Implicits.Ui
  import ui.zms

  private val conversations = new UiCache[ConvId, Conversation](lruSize = 10) // these objects are pretty lightweight now, we can easily keep several

  UiCacheUpdater(conversations, _.convsNotifier.onConversationChanged)

  lazy val convsList = new ConversationsList()

  def getConversation(id: ConvId): CancellableFuture[Option[Conversation]] =
    conversations.get(id) match {
      case Some(conv) => CancellableFuture.successful(Some(conv))
      case None => CancellableFuture.lift(zms.flatMapFuture(_.convsContent.convById(id))) map { _ map getConversation }
    }

  private[waz] def convById(id: ConvId): Conversation = getOrUpdate(conversations)(id, new Conversation(id))

  def getConversation(data: ConversationData) = getOrUpdate(conversations)(data.id, new Conversation(data))

  def getConversation(p: Parcel): IConversation = getConversation(JsonDecoder.decode[ConversationData](p.readString()))

  def sendMessage[A](id: ConvId, content: MessageContent[A]): Unit = zms(_.convsUi.sendMessage(id, content))

  def setName(id: ConvId, name: String): Unit = zms(_.convsUi.setConversationName(id, name))
  
  def addMembers(id: ConvId, users: Seq[User]): Unit = zms(_.convsUi.addConversationMembers(id, users.map(u => UserId(u.getId))))

  def removeMember(id: ConvId, user: User): Unit = zms(_.convsUi.removeConversationMember(id, UserId(user.getId)))

  def leave(id: ConvId): Unit = Serialized("Conversations", id) { zms(_.convsUi.leaveConversation(id)) }

  def setArchived(id: ConvId, archived: Boolean): Unit = zms(_.convsUi.setConversationArchived(id, archived))

  def setMuted(id: ConvId, muted: Boolean): Unit = zms(_.convsUi.setConversationMuted(id, muted))

  def clear(id: ConvId): Unit = Serialized("Conversations", id) { zms(_.convsUi.clearConversation(id)) }

  def createGroupConversation(users: Seq[User], localId: ConvId = ConvId()) =
    zms.flatMapFuture(_.convsUi.createGroupConversation(localId, users.map(u => UserId(u.getId))))

  def knock(id: ConvId): Unit = zms(_.convsUi.knock(id))

  def onVerificationStateChange(callback: VerificationStateCallback) = {

    def changeStream(zMessaging: ZMessaging) =
      zMessaging.convsStorage.onUpdated.map {
        _.filter { case (prev, conv) => prev.verified != conv.verified }
      }.filter(_.nonEmpty)

    zms {
      changeStream(_).on(Threading.Ui) { _ foreach {
          case (prev, conv) => callback.onVerificationStateChanged(conv.id.str, prev.verified, conv.verified)
        }
      }
    }
  }
}

object Conversations {
  implicit val tag: LogTag = logTagFor[Conversations]
}
