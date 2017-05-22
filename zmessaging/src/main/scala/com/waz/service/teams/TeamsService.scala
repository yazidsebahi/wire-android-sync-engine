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
import com.waz.content._
import com.waz.model.ConversationData.ConversationDataDao
import com.waz.model._
import com.waz.service.EventScheduler
import com.waz.sync.SyncServiceHandle
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.RichFuture

import scala.concurrent.Future

trait TeamsService {

  def getMembers(teamId: TeamId): Future[Set[UserData]]

  def getTeams(userId: UserId): Future[Set[TeamData]]

  def getSelfTeams: Future[Set[TeamData]]

  def getPermissions(userId: UserId, teamId: TeamId): Future[Option[Set[TeamMemberData.Permission]]]

  def getTeamConversations(teamId: TeamId): Future[Set[ConversationData]]

  def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData]): Future[Unit]

  def onTeamMember

}

class TeamsServiceImpl(selfUser:          UserId,
                      teamStorage:       TeamsStorage,
                      userStorage:       UsersStorage,
                      convsStorage:      ConversationStorage,
                      teamMemberStorage: TeamMemberStorage,
                      sync:              SyncServiceHandle,
                      prefs:             UserPreferences) extends TeamsService {

  private implicit val dispatcher = SerialDispatchQueue()


  val eventsProcessingStage = EventScheduler.Stage[TeamEvent] { (_, events) =>
    RichFuture.processSequential(events)(handleTeamEvent)
  }

  private def handleTeamEvent(ev: TeamEvent) = {
    import TeamEvent._
    ev match {
      case Create(id)                         => onAdded(id)
      case Delete(id)                         => onRemoved(id)
      case Update(id, name, icon, iconKey)    => onUpdated(id, icon, iconKey)
      case MemberJoin(teamId, userId)         => onMemberJoin(teamId, userId)
      case MemberLeave(teamId, userId)        => onMemberLeave(teamId, userId)
      case ConversationCreate(teamId, convId) => onConversationCreated(teamId, convId)
      case ConversationDelete(teamId, convId) => onConversationDeleted(teamId, convId)
      case _ => Future.successful({})
    }
  }

  override def getMembers(teamId: TeamId) = for {
    userIds  <- teamMemberStorage.getByTeam(teamId).map(_.map(_.userId))
    userData <- userStorage.getAll(userIds)
  } yield userData.collect { case Some(data) => data }.toSet

  override def getTeams(userId: UserId) = for {
    teamIds  <- teamMemberStorage.getByUser(userId).map(_.map(_.teamId))
    teamData <- teamStorage.getAll(teamIds)
  } yield teamData.collect { case Some(data) => data }.toSet

  override def getSelfTeams = getTeams(selfUser)

  override def getPermissions(userId: UserId, teamId: TeamId) =
    teamMemberStorage.get((userId, teamId)).map(_.map(_.permissions))

  override def getTeamConversations(teamId: TeamId) = {
    import ConversationDataDao._
    convsStorage.find(_.team.contains(teamId), db => iterating(find(Team, Some(teamId))(db)), identity)
  }

  //TODO we should clear team/teamMember storage on full sync. There's no way of knowing what teams we're not a part of...
  override def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData]) = {
    for {
      _ <- teamStorage.insert(teams)
      _ <- teamMemberStorage.insert(members)
    //TODO should we check first if we already have these users in the database?
      _ <- sync.syncUsers(members.map(_.userId).toSeq: _* )
    } yield {}
  }

  private def onAdded(teamId: TeamId) = {
    //TODO
    Future.successful(TeamData(teamId, "", None))
  }

  //TODO what happens if we miss a remove event?
  private def onRemoved(id: TeamId) = {
    for {
      _ <- teamStorage.remove(id)
      _ <- teamMemberStorage.removeByTeam(id)
    } yield {}
  }

  private def onUpdated(id: TeamId, icon: Option[RAssetId], iconKey: Option[AESKey]) = {
    //TODO
    Future.successful(TeamData(id, "", None))
  }

  private def onMemberJoin(teamId: TeamId, userId: UserId) = {
    for {
      _ <- Future.successful({}) //userStorage.insert() TODO need to sync users?
      memberData <- Future.successful(TeamMemberData(userId, teamId, Set.empty)) //TODO after syncing user, sync his team permissions
      _ <- teamMemberStorage.insert(memberData)
    } yield memberData
  }

  private def onMemberLeave(teamId: TeamId, userId: UserId) = {
    //TODO
    Future.successful({})
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
