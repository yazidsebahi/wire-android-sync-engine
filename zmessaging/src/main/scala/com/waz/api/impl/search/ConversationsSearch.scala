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
import com.waz.api
import com.waz.api.IConversation
import com.waz.api.impl.CoreList
import com.waz.model.ConvId
import com.waz.service.SearchKey
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.Signal

class ConversationsSearch(initialPrefix: String = "", initialLimit: Int = 0)(implicit ui: UiModule) extends api.ConversationsSearch with CoreList[IConversation] with SignalLoading {
  import com.waz.threading.Threading.Implicits.Background
  private implicit val tag = logTagFor[ConversationsSearch]

  @volatile private var convs = IndexedSeq.empty[ConvId]
  private val searchParams = Signal((initialPrefix, initialLimit))

  addLoader({ zms =>
    searchParams flatMap { case (prefix, limit) =>
      Signal.future(zms.convsUi.findGroupConversations(SearchKey(prefix), limit).map(_.map(_.id).toVector))
    }
  }, IndexedSeq.empty) { cs =>
    verbose(s"loaded conversations (${searchParams.currentValue}): $cs")
    if (convs != cs) {
      convs = cs
      notifyChanged()
    }
  }

  override def query(prefix: String, limit: Int): Unit = {
    verbose(s"query($prefix, $limit)")
    searchParams ! (prefix, limit)
  }

  def getAll: Array[IConversation] = convs.map(ui.convs.convById)(collection.breakOut)

  override def get(position: Int): IConversation = ui.convs.convById(convs(position))

  override def size(): Int = convs.length
}
