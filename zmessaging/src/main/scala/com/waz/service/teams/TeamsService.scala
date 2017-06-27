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
import com.waz.utils.{RichFuture, returning}
import com.waz.utils.events.{EventStream, RefreshingSignal, Signal}

import scala.collection.{Map, Seq}
import scala.concurrent.Future
import scala.concurrent.Future.traverse

//TODO - return Signals of the search results for UI??
trait TeamsService {

  def team(userId: UserId): Signal[Option[TeamData]]

  def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[UserData]]

  def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[ConversationData]]

  def guests(teamId: TeamId): Signal[Set[UserId]]

  def selfTeam: Signal[Option[TeamData]]

  def onTeamsSynced(teams: Set[TeamData], members: Set[UserId], fullSync: Boolean = false): Future[Unit]

}

class TeamsServiceImpl(selfUser:          UserId,
                       teamStorage:       TeamsStorage,
                       userStorage:       UsersStorage,
                       convsStorage:      ConversationStorage,
                       convMemberStorage: MembersStorage,
                       convsContent:      ConversationsContentUpdater,
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

    val membersJoined = events.collect { case MemberJoin(t, u)  => (t, u)}.toSet
    val membersLeft   = events.collect { case MemberLeave(t, u)  => (t, u)}.toSet

    def groupMemberEvents(evs: Iterable[(TeamId, UserId)]): Map[TeamId, Set[UserId]] =
      evs.groupBy { _._1 }.map { case (t, ev) => t -> ev.map(_._2).toSet}

    val convsCreated = events.collect { case ConversationCreate(_, id) => id }.toSet
    val convsDeleted = events.collect { case ConversationDelete(_, id) => id }.toSet
    for {
      _ <- onTeamsAdded(teamsAdded -- teamsRemoved)
      _ <- onTeamsRemoved(teamsRemoved -- teamsAdded)
      _ <- RichFuture.processSequential(events.collect { case e:Update => e}) { case Update(id, name, icon, iconKey) => onTeamUpdated(id, name, icon, iconKey) }
      _ <- traverse(groupMemberEvents(membersJoined -- membersLeft)){ case (team, joined) => onMembersJoined(team, joined)}
      _ <- traverse(groupMemberEvents(membersLeft -- membersJoined)){ case (team, left)   => onMembersLeft(team, left)}
      _ <- onConversationsCreated(convsCreated -- convsDeleted)
      _ <- onConversationsDeleted(convsDeleted -- convsCreated)
    } yield {}
  }

  //TODO - maybe include user permissions for supplied team
  override def searchTeamMembers(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = query match {
    case Some(q) => userStorage.searchByTeam(teamId, q, handleOnly)
    case None => userStorage.getByTeam(Set(teamId))
  }

  override def searchTeamConversations(teamId: TeamId, query: Option[SearchKey] = None, handleOnly: Boolean = false) = {
    verbose(s"searchTeamConversations: team: $teamId, query: $query, handleOnly?: $handleOnly")
    import ConversationDataDao._
    (query match {
      case Some(q) => convsStorage.search(q, selfUser, handleOnly, Some(teamId))
      case None    => convsStorage.find(_.team.contains(teamId), db => iterating(find(Team, Some(teamId))(db)), identity)
    }).map(_.toSet)
  }

  override def team(userId: UserId) = {
    verbose(s"getTeam: user: $userId")

    def load: Future[Option[TeamData]] = {
      val user = userStorage.get(userId)
      val team = user.map( _.flatMap(_.teamId))
      team.collect { case Some(teamId) => teamStorage.get(teamId) }.flatMap(identity)
    }

    val allChanges = teamStorage.onUpdated.map(_.map(_._2.id))

    //TODO could be nice to avoid loading for updates/deletions, since we already know the loaded set of teams is for the target user
    //We need to check TeamMembersStorage for any new additions to check that they are for the target user, hence the refreshing signal.
    new RefreshingSignal[Option[TeamData], Seq[TeamId]](CancellableFuture.lift(load), allChanges)
  }

  override def guests(teamId: TeamId) = {
    verbose(s"findGuests: team: $teamId")

    def load: Future[Set[UserId]] = for {
      convs       <- searchTeamConversations(teamId).map(_.map(_.id))
      allUsers    <- convMemberStorage.getByConvs(convs).map(_.map(_.userId).toSet)
      teamMembers <- userStorage.getByTeam(Set(teamId)).map(_.map(_.id))
    } yield allUsers -- teamMembers

    // TODO: findGuests as a signal, then in UI listen to this isgnal in TeamsAndUserController
    val allChanges = {
      val ev1 = convMemberStorage.onUpdated.map(_.map(_._2.userId))
      val ev2 = convMemberStorage.onDeleted.map(_.map(_._1))
      EventStream.union(ev1, ev2)
    }

    new RefreshingSignal[Set[UserId], Seq[UserId]](CancellableFuture.lift(load), allChanges)
  }

  override def selfTeam = team(selfUser)

  override def onTeamsSynced(teamsFetched: Set[TeamData], members: Set[UserId], fullSync: Boolean) = {
    verbose(s"onTeamsSynced: fullSync? $fullSync, teams: $teamsFetched \nmembers: $members")

    def insertFetchedData() = {
      for {
        _ <- teamStorage.insert(teamsFetched)
        //TODO should we check first if we already have these users in the database?
        _ <- sync.syncUsers(members.toSeq: _* )
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

  private def onTeamsAdded(teams: Set[TeamId]) = {
    verbose(s"onTeamsAdded: $teams")
    if (teams.nonEmpty) sync.syncTeams(teams) else Future.successful({})
  }

  private def onTeamsRemoved(teams: Set[TeamId]) = {
    verbose(s"onTeamsRemoved: $teams")
    if (teams.nonEmpty)
      for {
        _          <- teamStorage.remove(teams)
        removed    <- userStorage.removeByTeam(teams)
        _          <- removeAllTeamConversations(teams)
        _          <- removeUnconnectedUsers(removed.map(_.id))
        staleConvs <- {
          import ConversationDataDao._
          convsStorage.find(c => c.team.exists(teams.contains), db => iterating(findInSet(Team, teams.map(Option(_)))(db)), _.id)
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
    verbose(s"onTeamMembersJoined: $teamId, users: $users")
    sync.syncTeams(Set(teamId))
  }

  private def onMembersLeft(teamId: TeamId, userIds: Set[UserId]) = {
    verbose(s"onTeamMembersLeft: team: $teamId, users: $userIds")
    if (userIds.contains(selfUser)) {
      verbose("Self user removed from team - will now remove team locally")
      onTeamsRemoved(Set(teamId))
    } else {
      for {
        _ <- userStorage.remove(userIds)
        _ <- removeUsersFromTeamConversations(teamId, userIds)
        _ <- removeUnconnectedUsers(userIds)
      } yield {}
    }
  }

  private def removeUsersFromTeamConversations(teamId: TeamId, users: Set[UserId]) = {
    for {
      convs           <- searchTeamConversations(teamId).map(_.map(_.id))
      membersToRemove = for (u <- users; c <- convs) yield (u, c)
      _               <- convMemberStorage.remove(membersToRemove)
    } yield {}
  }

  private def removeAllTeamConversations(teams: Set[TeamId]) = {
    for {
      convs   <- convsStorage.findByTeams(teams).map(_.map(_.id))
      _       <- convsStorage.remove(convs)
      members <- convMemberStorage.getByConvs(convs).map(_.map(m => m.userId -> m.convId))
      _       <- convMemberStorage.remove(members)
    } yield {}
  }

  private def removeUnconnectedUsers(users: Set[UserId]): Future[Unit] = {
    (for {
      stillConnected   <- userStorage.find(u => users.contains(u.id), db => UserDataDao.findAll(users)(db), identity).map(_.filter(_.connection != Unconnected).map(_.id).toSet)
    } yield {
      returning(users -- stillConnected) { toRemove =>
        verbose(s"Removing users from database: $toRemove")
      }
    }).flatMap(userStorage.remove)
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

}
