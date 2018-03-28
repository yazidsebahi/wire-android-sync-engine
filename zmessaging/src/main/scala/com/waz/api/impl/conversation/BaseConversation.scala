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
package com.waz.api.impl.conversation

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.IConversation
import com.waz.api.impl._
import com.waz.model.ConversationData
import com.waz.service.ZMessaging
import com.waz.ui.{SignalLoading, UiModule}

import scala.util.Try

abstract class BaseConversation(implicit ui: UiModule) extends IConversation with UiObservable with SignalLoading {
  import BaseConversation._

  var name: String = ""
  var data: ConversationData = ConversationData.Empty

  protected def id = data.id

  def set(data: ConversationData): Unit = {
    verbose(s"set($data)")
    if (this.data != data) {
      this.data = data
      name = conversationName(data)

      notifyChanged()
    }
  }

  def getName = name

  override def toString: String = s"Conversation($id, $name, $data)"

  override def hashCode = id.hashCode

  override def getId = id.str

}

object BaseConversation {

  lazy val UnknownName = Try(ZMessaging.context.getResources.getString(com.waz.zms.R.string.zms_unknown_conversation_name)).getOrElse("…")

  private def conversationName(data: ConversationData) = {
    val name = if (data.convType == IConversation.Type.GROUP) data.name.filter(!_.isEmpty).getOrElse(data.generatedName) else data.generatedName
    if (name.isEmpty) {
      warn(s"Name is empty for: $data")
      UnknownName
    } else name
  }
}
