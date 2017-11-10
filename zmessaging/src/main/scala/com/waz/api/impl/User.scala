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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.model._
import com.waz.ui._

class User(val id: UserId, var data: UserData)(implicit ui: UiModule) extends com.waz.api.User with UiObservable with SignalLoading {

  def this(id: UserId)(implicit ui: UiModule) = this(id, UserData(id, ""))
  def this(data: UserData)(implicit ui: UiModule) = this(data.id, data)

  require(id == data.id)

  // XXX: listen to storage directly if no ZMessaging is available
  // this is needed for Self.getUser to work, UI accesses it before zms is fully logged in
  accountLoader { acc =>
    acc.zmessaging flatMap {
      case None => acc.storage.flatMap(_.usersStorage.signal(id))
      case Some(zms) => zms.users.userSignal(id)
    }
  } { set }

  def set(d: UserData): Unit = {
    require(this.id == d.id)
    verbose(s"set($d)")

    if (data != d) {
      data = d
      notifyChanged()
    }
  }

  def getName = data.name

  override def equals(other: Any): Boolean = other match {
    case other: User => other.id == id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = s"User(id = $id, data = $data)"
}
