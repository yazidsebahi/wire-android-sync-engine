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
import com.waz.api.User
import com.waz.api.impl.SearchQuery.{DbQuery, RecommendedPeople, TopPeople}
import com.waz.api.impl.{CoreList, SearchQuery}
import com.waz.model.UserData.ConnectionStatus
import com.waz.model.{UserData, UserId}
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.ui.{SignalLoading, UiEventListener, UiModule}
import com.waz.utils.events.Signal

import scala.collection.{breakOut, mutable}
import scala.concurrent.Future

class UsersSearch(initialQuery: SearchQuery.DbQuery = TopPeople, initialLimit: Int = UsersSearch.DefaultLimit, initialFilter: Set[String] = Set.empty)(implicit val ui: UiModule) extends api.UsersSearch with CoreList[api.User] with SignalLoading with UiEventListener[Seq[UserData]] {
  import UsersSearch._

  private var userIds = IndexedSeq.empty[UserId]
  private var contactsOnWire = Set.empty[UserId]
  private val userData = new mutable.HashMap[UserId, UserData]
  // volatile, because this is also accessed from signal to get default value, and we don't yet have any way to restrict flatMap to ui thread
  @volatile private var users = EmptyResult

  private val searchParams = Signal[(DbQuery, Int, Set[String])]((initialQuery, initialLimit, initialFilter))

  addLoader({ zms =>
    searchParams flatMap { case (query, limit, filter) =>
      zms.usersearch.searchQueryChangeSignal(query) flatMap { _ =>
        def shouldExclude(user: UserData) = filter(user.id.str)

        import com.waz.threading.Threading.Implicits.Background
        Signal.future(zms.usersearch.searchUserData(query, limit + filter.size).map(_.filterNot(shouldExclude).take(limit).toIndexedSeq))
      }
    }
  }, EmptyResult) { us =>
    verbose(s"users[${searchParams.currentValue}] loaded: $us")
    val ids = us.map(_.id)
    if (userIds != ids) {
      userIds = ids
      userData.clear()
      us.foreach { u => userData(u.id) = u }
      updateUsers()
      notifyChanged()
    }
  }

  // user data could have been changed since last time we fetched user ids, so we need to add another filtering,
  // top people should only return connected (and not blocked) users, while recommended only shows unconnected users
  private def updateUsers() = searchParams.currentValue.foreach { p =>
    users = p._1 match {
      case TopPeople => userIds map userData filter { _.connection == ConnectionStatus.Accepted }
      case RecommendedPeople => userIds filterNot contactsOnWire.contains map userData filterNot { _.isConnected }
      case _ => userIds map userData
    }
  }

  override protected def publisher(zms: ZMessaging) = zms.usersStorage.onChanged

  override protected def onReset: Future[Unit] = Future.successful(())
  override protected def onResume: Future[Unit] = Future.successful(())

  // Listens to all user update events to check for user state changes
  override protected def process(events: Seq[Seq[UserData]]): Future[Unit] = Future {
    var changed = false
    var notify = false
    events.flatten foreach { user =>
      userData.get(user.id) foreach { prev =>
        changed = true
        if (prev.connection != user.connection) notify = true
        userData.put(user.id, user)
      }
    }
    if (changed) updateUsers()
    if (notify) notifyChanged() // only need to notify UI if resulting list ordering was changed, and this could only happen if user connection state is changed
  } (Threading.Ui)

  def query(query: SearchQuery.DbQuery, limit: Int, filter: Set[String]): Unit = {
    verbose(s"query($query, $limit, $filter)")
    searchParams ! (query, limit, filter)
  }

  override def query(name: String, limit: Int, filter: Array[String]): Unit =
    query(DbQuery(name.trim), limit, filter.toSet)

  override def get(position: Int): User = ui.users.getUser(users(position))

  override def size(): Int = users.length

  def getAll: Array[User] = users.map(ui.users.getUser)(breakOut)
  def getContacts: Array[User] = users.filter(_.connection == ConnectionStatus.Accepted).map(ui.users.getUser)(breakOut)
  def getUnconnected: Array[User] = users.filter(_.connection != ConnectionStatus.Accepted).map(ui.users.getUser)(breakOut)
}

object UsersSearch {
  private implicit val tag: LogTag = logTagFor[UsersSearch]
  val EmptyResult = IndexedSeq.empty[UserData]
  val DefaultLimit = 9
}
