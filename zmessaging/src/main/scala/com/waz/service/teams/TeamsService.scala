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
import com.waz.ZLog.verbose
import com.waz.content.UserPreferences.ShouldSyncTeams
import com.waz.content._
import com.waz.model.ConversationData.ConversationDataDao
import com.waz.model.UserData.ConnectionStatus.Unconnected
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.{EventScheduler, SearchKey}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.RichFuture
import com.waz.utils.events.{EventStream, RefreshingSignal, Signal}

import scala.collection.{Map, Seq}
import scala.concurrent.Future
import scala.concurrent.Future.traverse

//TODO - return Signals of the search results for UI??
trait TeamsService {

  def getTeams(userId: UserId): Signal[Set[TeamData]]

  def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[UserData]]

  def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[ConversationData]]

  def findGuests(teamId: TeamId): Future[Set[UserId]]

  def getSelfTeams: Signal[Set[TeamData]]

  def getPermissions(userId: UserId, teamId: TeamId): Future[Option[Set[TeamMemberData.Permission]]]

  def onTeamsSynced(teams: Set[TeamData], members: Set[TeamMemberData], fullSync: Boolean = false): Future[Unit]

}

class TeamsServiceImpl(selfUser:          UserId,
                       teamStorage:       TeamsStorage,
                       userStorage:       UsersStorage,
                       convsStorage:      ConversationStorage,
                       convMemberStorage: MembersStorage,
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

    val teamsAdded   = events.collect { case Create(id) => id }.toSet
    val teamsRemoved = events.collect { case Delete(id) => id}.toSet

    def memberEvents(evs: Iterable[MemberEvent]): Map[TeamId, Set[UserId]] =
      evs.groupBy { _.teamId }.map { case (t, ev) => t -> ev.map(_.userId).toSet}

    val convsCreated = events.collect { case ConversationCreate(_, id) => id }.toSet
    val convsDeleted = events.collect { case ConversationDelete(_, id) => id }.toSet
    for {
      _ <- onTeamsAdded(teamsAdded -- teamsRemoved)
      _ <- onTeamsRemoved(teamsRemoved -- teamsAdded)
      _ <- RichFuture.processSequential(events.collect { case e:Update => e}) { case Update(id, name, icon, iconKey) => onTeamUpdated(id, name, icon, iconKey) }
      _ <- traverse(memberEvents(events.collect { case e:MemberJoin  => e})){ case (team, joined) => onMembersJoined(team, joined)}
      _ <- traverse(memberEvents(events.collect { case e:MemberLeave => e})){ case (team, left)   => onMembersLeft(team, left)}
      _ <- onConversationsCreated(convsCreated -- convsDeleted)
      _ <- onConversationsDeleted(convsDeleted -- convsCreated)
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

    def load = for {
      teamIds  <- teamMemberStorage.getByUser(Set(userId)).map(_.map(_.teamId))
      teamData <- teamStorage.getAll(teamIds)
    } yield teamData.collect { case Some(data) => data }.toSet

    /**
      * Any additions/removals to teamsStorage will be shortly followed by additions/removals to TeamMembers, so we only need to
      * listen to additions/removals there. Updates however, may be triggered on teams without changes to TeamMembers
      */
    val allChanges = EventStream.union(
      teamStorage.onUpdated.map(_.map(_._2.id)),
      EventStream.union {
        teamMemberStorage.onChanged.map(_.map(m => m.userId -> m.teamId))
        teamMemberStorage.onDeleted
      }.collect { case evs => evs.filter(_._1 == userId)}.map(_.map(_._2))
    )

    //TODO could be nice to avoid loading for updates/deletions, since we already know the loaded set of teams is for the target user
    //We need to check TeamMembersStorage for any new additions to check that they are for the target user, hence the refreshing signal.
    new RefreshingSignal[Set[TeamData], Seq[TeamId]](CancellableFuture.lift(load), allChanges)
  }

  override def getSelfTeams = getTeams(selfUser)

  override def getPermissions(userId: UserId, teamId: TeamId) =
    teamMemberStorage.get((userId, teamId)).map(_.map(_.selfPermissions))

  override def onTeamsSynced(teamsFetched: Set[TeamData], members: Set[TeamMemberData], fullSync: Boolean) = {
    verbose(s"onTeamsSynced: fullSync? $fullSync, teams: $teamsFetched \nmembers: $members")

    def insertFetchedData() = {
      for {
        _ <- teamStorage.insert(teamsFetched)
        _ <- teamMemberStorage.insert(members)
        //TODO should we check first if we already have these users in the database?
        _ <- sync.syncUsers(members.map(_.userId).toSeq: _* )
      } yield {}
    }

    if (fullSync) {
      for {
        localTeams   <- teamStorage.list().map(_.toSet)
        staleTeams   = (localTeams -- teamsFetched).map(_.id)
        _            <- onTeamsRemoved(staleTeams)
        _            <- insertFetchedData()
      } yield {}
    } else insertFetchedData()
  }

  private def onTeamsAdded(ids: Set[TeamId]) = {
    verbose(s"onTeamsAdded: $ids")
    if (ids.nonEmpty) sync.syncTeams(ids) else Future.successful({})
  }

  private def onTeamsRemoved(ids: Set[TeamId]) = {
    verbose(s"onTeamsRemoved: $ids")
    if (ids.nonEmpty)
      for {
        _          <- teamStorage.remove(ids)
        removed    <- teamMemberStorage.removeByTeam(ids)
        _          <- removeUnconnectedUsers(removed)
        staleConvs <- {
          import ConversationDataDao._
          convsStorage.find(c => c.team.exists(ids.contains), db => iterating(findInSet(Team, ids.map(Option(_)))(db)), _.id)
        }
        _          <- convsStorage.remove(staleConvs)
      } yield {}
    else Future.successful({})
  }

  private def onTeamUpdated(id: TeamId, name: Option[String], icon: Option[RAssetId], iconKey: Option[AESKey]) = {
    verbose(s"onTeamUpdated: $id, name: $name, icon: $icon, iconKey: $iconKey")
    teamStorage.update(id, team => team.copy(
      name    = name.getOrElse(team.name),
      icon    = icon.orElse(team.icon),
      iconKey = iconKey.orElse(team.iconKey)
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

  private def onConversationsCreated(convs: Set[RConvId]) = {
    verbose(s"onConversationsCreated: convs: $convs")
    if (convs.nonEmpty)
      for {
        convs <- Future.traverse(convs)(convsContent.convByRemoteId).map(_.collect { case Some(c) => c.id })
        _     <- sync.syncConversations(convs)
      } yield {}
    else Future.successful({})
  }

  private def onConversationsDeleted(convs: Set[RConvId]) = {
    verbose(s"onConversationsDeleted: convs: $convs")
    //TODO
    Future.successful({})
  }

  override def findGuests(teamId: TeamId) = {
    for {
      convs       <- searchTeamConversations(teamId).map(_.map(_.id))
      allUsers    <- convMemberStorage.getByConvs(convs).map(_.map(_.userId).toSet)
      teamMembers <- searchTeamMembers(teamId).map(_.map(_.id))
    } yield allUsers -- teamMembers

  }
}
