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
package com.waz.api.impl.search

import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api
import com.waz.api.IConversation
import com.waz.api.impl.CoreList
import com.waz.model.ConvId
import com.waz.service.SearchKey
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.Signal

class ConversationSearchResult(prefix: String, limit: Int, handleOnly: Boolean)(implicit ui: UiModule) extends api.ConversationSearchResult with CoreList[IConversation] with SignalLoading {
  import com.waz.threading.Threading.Implicits.Background

  @volatile private var convs = Option.empty[Vector[ConvId]]

  addLoader { zms =>
    Signal.future(zms.convsUi.findGroupConversations(SearchKey(prefix), limit, handleOnly).map(_.map(_.id).toVector))
  } { cs =>
    verbose(s"loaded conversations ($prefix, $limit): $cs")
    if (convs.forall(_ != cs)) {
      convs = Some(cs)
      notifyChanged()
    }
  }

  private[this] def currentConvs = convs.getOrElse(Vector.empty)

  def getAll: Array[IConversation] = currentConvs.map(ui.convs.convById)(collection.breakOut)
  override def get(position: Int): IConversation = ui.convs.convById(currentConvs(position))
  override def size(): Int = currentConvs.length
}
