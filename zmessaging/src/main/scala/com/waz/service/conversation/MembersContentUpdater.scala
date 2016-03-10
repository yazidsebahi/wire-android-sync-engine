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
package com.waz.service.conversation

import android.content.Context
import com.waz.content.MembersStorage
import com.waz.model.{ConvId, UserId}

class MembersContentUpdater(context: Context, val membersStorage: MembersStorage) {

  private[service] def addUsersToConversation(conv: ConvId, users: Seq[UserId]) =
    membersStorage.add(conv, users: _*)

  private[service] def removeUsersFromConversation(conv: ConvId, users: Seq[UserId]) =
    membersStorage.remove(conv, users: _*)
}
