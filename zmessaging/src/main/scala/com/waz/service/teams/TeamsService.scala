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

import com.waz.ZLog
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.{verbose, _}
import com.waz.content.UserPreferences.ShouldSyncTeams
import com.waz.content._
import com.waz.model.ConversationData.ConversationDataDao
import com.waz.model.UserData.ConnectionStatus.Unconnected
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.service.conversation.{ConversationsContentUpdater, ConversationsService, ConversationsUiService}
import com.waz.service.{EventScheduler, SearchKey}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.RichFuture

import scala.concurrent.Future
import scala.concurrent.Future.traverse

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
                       convsContent:      ConversationsContentUpdater,
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
    verbose(s"Handling events: $events")
    import TeamEvent._

    def memberEvents(evs: Iterable[MemberEvent]): Map[TeamId, Set[UserId]] =
      evs.groupBy { _.teamId }.map { case (t, ev) => t -> ev.map(_.userId).toSet}

    def convEvents(evs: Iterable[ConversationEvent]): Map[TeamId, Set[RConvId]] =
      evs.groupBy { _.teamId }.map { case (t, ev) => t -> ev.map(_.convId).toSet}

    for {
      _ <- onTeamsAdded(events.collect { case Create(id) => id }.toSet)
      _ <- onTeamsRemoved(events.collect { case Delete(id) => id}.toSet)
      _ <- RichFuture.processSequential(events.collect { case e:Update => e}) { case Update(id, name, icon, iconKey) => onTeamUpdated(id, name, icon, iconKey) }
      _ <- traverse(memberEvents(events.collect { case e:MemberJoin  => e})){ case (team, joined) => onMembersJoined(team, joined)}
      _ <- traverse(memberEvents(events.collect { case e:MemberLeave => e})){ case (team, left)   => onMembersLeft(team, left)}
      _ <- traverse(convEvents(events.collect { case e:ConversationCreate => e})) { case (team, created)   => onConversationsCreated(team, created)}
      _ <- traverse(convEvents(events.collect { case e:ConversationDelete => e})) { case (team, deleted)   => onConversationsDeleted(team, deleted)}
    } yield {}
  }

  //TODO - maybe include user permissions for supplied team
  override def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = {
    verbose(s"searchTeamMembers: team: $teamId, query: $query, handlOnly?: $handleOnly")
    for {
      userIds  <- (query match {
        case Some(q) => teamMemberStorage.searchByTeam(teamId, q, handleOnly)
        case None =>    teamMemberStorage.getByTeam(Set(teamId))
      }).map(_.map(_.userId) - selfUser)
      userData <- userStorage.getAll(userIds)
    } yield userData.collect { case Some(data) => data }.toSet
  }

  override def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = {
    verbose(s"searchTeamConversations: team: $teamId, query: $query, handlOnly?: $handleOnly")
    import ConversationDataDao._
    (query match {
      case Some(q) => convsStorage.search(q, selfUser, handleOnly, Some(teamId))
      case None    => convsStorage.find(_.team.contains(teamId), db => iterating(find(Team, Some(teamId))(db)), identity)
    }).map(_.toSet)
  }

  override def getTeams(userId: UserId) = {
    verbose(s"getTeams: user: $userId")
    for {
      teamIds  <- teamMemberStorage.getByUser(Set(userId)).map(_.map(_.teamId))
      teamData <- teamStorage.getAll(teamIds)
    } yield teamData.collect { case Some(data) => data }.toSet
  }

  override def getSelfTeams = getTeams(selfUser)

  override def getPermissions(userId: UserId, teamId: TeamId) =
    teamMemberStorage.get((userId, teamId)).map(_.map(_.permissions))

  //TODO we should clear team/teamMember storage on full sync. There's no way of knowing what teams we're not a part of...
  override def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData]) = {
    verbose(s"onTeamsSynced: teams: $teams \nmembers: $members")
    for {
      _ <- teamStorage.insert(teams)
      _ <- teamMemberStorage.insert(members)
    //TODO should we check first if we already have these users in the database?
      _ <- sync.syncUsers(members.map(_.userId).toSeq: _* )
    } yield {}
  }

  private def onTeamsAdded(ids: Set[TeamId]) = {
    verbose(s"onTeamsAdded: $ids")
    sync.syncTeams(ids)
  }

  private def onTeamsRemoved(ids: Set[TeamId]) = {
    verbose(s"onTeamsRemoved: $ids")
    for {
      _       <- teamStorage.remove(ids)
      removed <- teamMemberStorage.removeByTeam(ids)
      _       <- removeUnconnectedUsers(removed)
    } yield {}
  }

  private def onTeamUpdated(id: TeamId, name: Option[String], icon: Option[RAssetId], iconKey: Option[AESKey]) = {
    verbose(s"onTeamUpdated: $id, name: $name, icon: $icon, iconKey: $iconKey")
    //TODO handle processing of icon
    teamStorage.update(id, team => team.copy(
      name = name.getOrElse(team.name)
    ))
  }

  //TODO follow up on: https://github.com/wireapp/architecture/issues/13
  //At the moment, we need to re-fetch the entire list of team members as a workaround
  private def onMembersJoined(teamId: TeamId, users: Set[UserId]) = {
    verbose(s"onTeamMembsJoined: $teamId, users: $users")
    sync.syncTeams(Set(teamId))
  }

  private def onMembersLeft(teamId: TeamId, userIds: Set[UserId]) = {
    verbose(s"onMembersLeft: team: $teamId, users: $userIds")
    for {
      _ <- teamMemberStorage.remove(userIds.map(u => u -> teamId))
      _ <- removeUnconnectedUsers(userIds)
    } yield {}
  }


  private def removeUnconnectedUsers(users: Set[UserId]): Future[Unit] = {
    (for {
      stillTeamMembers <- teamMemberStorage.getByUser(users).map(_.map(_.userId).toSet)
      stillConnected   <- userStorage.find(u => users.contains(u.id), db => UserDataDao.findAll(users)(db), identity).map(_.filter(_.connection != Unconnected).map(_.id).toSet)
    } yield {
      val toRemove = users -- stillTeamMembers -- stillConnected
      verbose(s"Removing users from database: $toRemove")
      userStorage.remove(toRemove)
    }).flatten
  }

  private def onConversationsCreated(teamId: TeamId, convs: Set[RConvId]) = {
    verbose(s"onConversationsCreated: $teamId, convs: $convs")
    for {
      convs <- Future.traverse(convs)(convsContent.convByRemoteId).map(_.collect { case Some(c) => c.id })
      _     <- sync.syncConversations(convs)
    } yield {}
  }

  private def onConversationsDeleted(teamId: TeamId, convs: Set[RConvId]) = {
    verbose(s"onConversationsDeleted: $teamId, convs: $convs")
    //TODO
    Future.successful({})
  }

}
