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

import android.content.Context
import com.waz.content.ContentChange.{Removed, Updated}
import com.waz.model.TeamData.TeamDataDoa
import com.waz.model.TeamMemberData.TeamMemberDataDoa
import com.waz.model._
import com.waz.service.SearchKey
import com.waz.threading.Threading
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.EventStream
import com.waz.utils.{CachedStorage, CachedStorageImpl, TrimmingLruCache}

import scala.collection.Seq
import scala.concurrent.Future

trait TeamsStorage extends CachedStorage[TeamId, TeamData]
class TeamsStorageImpl(context: Context, storage: Database) extends CachedStorageImpl[TeamId, TeamData](new TrimmingLruCache(context, Fixed(1024)), storage)(TeamDataDoa, "TeamStorage_Cached") with TeamsStorage


trait TeamMemberStorage extends CachedStorage[TeamMemberData.Key, TeamMemberData] {
  def getByUser(user: Set[UserId]): Future[Set[TeamMemberData]]
  def getByTeam(team: Set[TeamId]): Future[Set[TeamMemberData]]
  def searchByTeam(team: TeamId, prefix: SearchKey, handleOnly: Boolean): Future[Set[TeamMemberData]]
  def removeByTeam(teams: Set[TeamId]): Future[Set[UserId]]
}

class TeamMemberStorageImpl(context: Context, storage: Database) extends CachedStorageImpl[TeamMemberData.Key, TeamMemberData](new TrimmingLruCache(context, Fixed(1024)), storage)(TeamMemberDataDoa, "TeamMemberStorage_Cached") with TeamMemberStorage {

  import Threading.Implicits.Background

  override def getByUser(users: Set[UserId]) = find(data => users.contains(data.userId), TeamMemberDataDoa.findForUsers(users)(_), identity)

  override def getByTeam(teams: Set[TeamId]) = find(data => teams.contains(data.teamId), TeamMemberDataDoa.findForTeams(teams)(_), identity)

  override def removeByTeam(teams: Set[TeamId]) = for {
    members <- getByTeam(teams)
    _       <- remove(members.map(data => data.userId -> data.teamId))
  } yield members.map(_.userId)

  override def searchByTeam(team: TeamId, prefix: SearchKey, handleOnly: Boolean) = storage(TeamMemberDataDoa.search(prefix, team, handleOnly)(_)).future
}
