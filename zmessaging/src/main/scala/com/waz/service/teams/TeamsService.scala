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
package com.waz.service.teams

import com.waz.ZLog.ImplicitTag._
import com.waz.content.UserPreferences.ShouldSyncTeams
import com.waz.content._
import com.waz.model.ConversationData.ConversationDataDao
import com.waz.model.UserData.ConnectionStatus.Unconnected
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.service.{EventScheduler, SearchKey}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.RichFuture

import scala.concurrent.Future

//TODO - return Signals of the search results for UI??
trait TeamsService {

  def getTeams(userId: UserId): Future[Set[TeamData]]

  def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[UserData]]

  def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[ConversationData]]

  def getSelfTeams: Future[Set[TeamData]]

  def getPermissions(userId: UserId, teamId: TeamId): Future[Option[Set[TeamMemberData.Permission]]]

  def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData]): Future[Unit]

}

class TeamsServiceImpl(selfUser:          UserId,
                       teamStorage:       TeamsStorage,
                       userStorage:       UsersStorage,
                       convsStorage:      ConversationStorage,
                       teamMemberStorage: TeamMemberStorage,
                       sync:              SyncServiceHandle,
                       userPrefs:         UserPreferences) extends TeamsService {

  private implicit val dispatcher = SerialDispatchQueue()

  val shouldSyncTeams = userPrefs.preference(ShouldSyncTeams)

  shouldSyncTeams.mutate {
    case true =>
      sync.syncTeams()
      false
    case v => v
  }

  val eventsProcessingStage = EventScheduler.Stage[TeamEvent] { (_, events) =>
    import TeamEvent._
    for {
      _ <- onTeamsAdded(events.collect { case Create(id) => id }.toSet)
      _ <- onTeamsRemoved(events.collect { case Delete(id) => id}.toSet)
      _ <- RichFuture.processSequential(events)(handleTeamEvent)
    } yield {}
  }

  private def handleTeamEvent(ev: TeamEvent) = {
    import TeamEvent._
    ev match {
      case Update(id, name, icon, iconKey)    => onUpdated(id, name, icon, iconKey)
      case MemberJoin(teamId, userId)         => onMemberJoin(teamId, userId)
      case MemberLeave(teamId, userId)        => onMemberLeave(teamId, userId)
      case ConversationCreate(teamId, convId) => onConversationCreated(teamId, convId)
      case ConversationDelete(teamId, convId) => onConversationDeleted(teamId, convId)
      case _ => Future.successful({})
    }
  }

  //TODO - maybe include user permissions for supplied team
  override def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = {
    for {
      userIds  <- (query match {
        case Some(q) => teamMemberStorage.searchByTeam(teamId, q, handleOnly)
        case None =>    teamMemberStorage.getByTeam(Set(teamId))
      }).map(_.map(_.userId) - selfUser)
      userData <- userStorage.getAll(userIds)
    } yield userData.collect { case Some(data) => data }.toSet
  }

  override def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = {
    import ConversationDataDao._
    (query match {
      case Some(q) => convsStorage.search(q, selfUser, handleOnly, Some(teamId))
      case None    => convsStorage.find(_.team.contains(teamId), db => iterating(find(Team, Some(teamId))(db)), identity)
    }).map(_.toSet)
  }

  override def getTeams(userId: UserId) = for {
    teamIds  <- teamMemberStorage.getByUser(Set(userId)).map(_.map(_.teamId))
    teamData <- teamStorage.getAll(teamIds)
  } yield teamData.collect { case Some(data) => data }.toSet

  override def getSelfTeams = getTeams(selfUser)

  override def getPermissions(userId: UserId, teamId: TeamId) =
    teamMemberStorage.get((userId, teamId)).map(_.map(_.permissions))

  //TODO we should clear team/teamMember storage on full sync. There's no way of knowing what teams we're not a part of...
  override def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData]) = {
    for {
      _ <- teamStorage.insert(teams)
      _ <- teamMemberStorage.insert(members)
    //TODO should we check first if we already have these users in the database?
      _ <- sync.syncUsers(members.map(_.userId).toSeq: _* )
    } yield {}
  }

  private def onTeamsAdded(ids: Set[TeamId]) = sync.syncTeams(ids)

  private def onTeamsRemoved(id: Set[TeamId]) = {
    for {
      _       <- teamStorage.remove(id)
      removed <- teamMemberStorage.removeByTeam(id)
      _       <- removeUnconnectedUsers(removed)
    } yield {}
  }

  private def onUpdated(id: TeamId, name: Option[String], icon: Option[RAssetId], iconKey: Option[AESKey]) = {
    //TODO handle processing of icon
    teamStorage.update(id, team => team.copy(
      name = name.getOrElse(team.name)
    ))
  }

  private def onMemberJoin(teamId: TeamId, userId: UserId) = {
    for {
      _ <- Future.successful({}) //userStorage.insert() TODO need to sync users?
      memberData <- Future.successful(TeamMemberData(userId, teamId, Set.empty)) //TODO after syncing user, sync his team permissions
      _ <- teamMemberStorage.insert(memberData)
    } yield memberData
  }

  private def onMemberLeave(teamId: TeamId, userId: UserId) =
    for {
      _ <- teamMemberStorage.remove((userId, teamId))
      _ <- removeUnconnectedUsers(Set(userId))
    } yield {}


  private def removeUnconnectedUsers(users: Set[UserId]): Future[Unit] = {
    (for {
      stillTeamMembers <- teamMemberStorage.getByUser(users).map(_.map(_.userId).toSet)
      stillConnected   <- userStorage.find(u => users.contains(u.id), db => UserDataDao.findAll(users)(db), identity).map(_.filter(_.connection != Unconnected).map(_.id).toSet)
    } yield {
       val toRemove = users -- stillTeamMembers -- stillConnected
      userStorage.remove(toRemove)
    }).flatten
  }

  private def onConversationCreated(teamId: TeamId, convId: RConvId) = {
    //TODO
    Future.successful({})
  }

  private def onConversationDeleted(teamId: TeamId, convId: RConvId) = {
    //TODO
    Future.successful({})
  }

}
