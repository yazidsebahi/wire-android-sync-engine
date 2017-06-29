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
import com.waz.ZLog._
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

import scala.collection.Seq
import scala.concurrent.Future

//TODO - return Signals of the search results for UI??
trait TeamsService {

  def searchTeamMembers(query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[UserData]]

  def searchTeamConversations(query: Option[SearchKey] = None, handleOnly: Boolean = false): Future[Set[ConversationData]]

  val guests: Signal[Set[UserId]]

  val selfTeam: Signal[Option[TeamData]]

  def onTeamSynced(team: TeamData, members: Set[UserId]): Future[Unit]

}

class TeamsServiceImpl(selfUser:          UserId,
                       teamId:            Option[TeamId],
                       accountsStorage:   AccountsStorage,
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
      sync.syncTeam()
      false
    case v => v
  }

  val eventsProcessingStage = EventScheduler.Stage[TeamEvent] { (_, events) =>
    verbose(s"Handling events: $events")
    import TeamEvent._

    val membersJoined = events.collect { case MemberJoin(_, u)  => u}.toSet
    val membersLeft   = events.collect { case MemberLeave(_, u)  => u}.toSet

    val convsCreated = events.collect { case ConversationCreate(_, id) => id }.toSet
    val convsDeleted = events.collect { case ConversationDelete(_, id) => id }.toSet
    for {
      _ <- RichFuture.processSequential(events.collect { case e:Update => e}) { case Update(id, name, icon, iconKey) => onTeamUpdated(id, name, icon, iconKey) }
      _ <- onMembersJoined(membersJoined -- membersLeft)
      _ <- onMembersLeft(membersLeft -- membersJoined)
      _ <- onConversationsCreated(convsCreated -- convsDeleted)
      _ <- onConversationsDeleted(convsDeleted -- convsCreated)
    } yield {}
  }

  //TODO - maybe include user permissions for supplied team
  override def searchTeamMembers(query: Option[SearchKey] = None, handleOnly: Boolean = false) = teamId.map(id => query match {
    case Some(q) => userStorage.searchByTeam(id, q, handleOnly)
    case None => userStorage.getByTeam(Set(id))
  }).getOrElse(Future.successful(Set.empty))

  override def searchTeamConversations(query: Option[SearchKey] = None, handleOnly: Boolean = false) = teamId.map(id => {
    verbose(s"searchTeamConversations: team: $teamId, query: $query, handleOnly?: $handleOnly")
    import ConversationDataDao._
    (query match {
      case Some(q) => convsStorage.search(q, selfUser, handleOnly, Some(id))
      case None    => convsStorage.find(_.team.contains(id), db => iterating(find(Team, Some(id))(db)), identity)
    }).map(_.toSet)
  }).getOrElse(Future.successful(Set.empty))


  override lazy val guests = teamId.map(id => {
    verbose(s"findGuests: team: $teamId")

    def load: Future[Set[UserId]] = for {
      convs       <- searchTeamConversations().map(_.map(_.id))
      allUsers    <- convMemberStorage.getByConvs(convs).map(_.map(_.userId).toSet)
      teamMembers <- userStorage.getByTeam(Set(id)).map(_.map(_.id))
    } yield allUsers -- teamMembers

    val allChanges = {
      val ev1 = convMemberStorage.onUpdated.map(_.map(_._2.userId))
      val ev2 = convMemberStorage.onDeleted.map(_.map(_._1))
      EventStream.union(ev1, ev2)
    }

    new RefreshingSignal[Set[UserId], Seq[UserId]](CancellableFuture.lift(load), allChanges)
  }).getOrElse(Signal.empty[Set[UserId]])

  override lazy val selfTeam: Signal[Option[TeamData]] = teamId.map(id => {
    verbose(s"selfTeam")

    val allChanges = teamStorage.onUpdated.map(_.map(_._2.id))

    new RefreshingSignal[Option[TeamData], Seq[TeamId]](CancellableFuture.lift(teamStorage.get(id)), allChanges)
  }).getOrElse(Signal.empty[Option[TeamData]])

  override def onTeamSynced(team: TeamData, members: Set[UserId]) = {
    verbose(s"onTeamSynced: team: $team \nmembers: $members")

    for {
      _ <- teamStorage.insert(team)
      //TODO should we check first if we already have these users in the database?
      _ <- sync.syncUsers(members.toSeq: _* )
    } yield {}
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
  private def onMembersJoined(users: Set[UserId]) = {
    verbose(s"onTeamMembersJoined: users: $users")
    sync.syncTeam()
  }

  private def onMembersLeft(userIds: Set[UserId]) = {
    verbose(s"onTeamMembersLeft: users: $userIds")
    if (userIds.contains(selfUser)) {
      warn("Self user removed from team")
      Future.successful {}
    } else {
      for {
        _ <- userStorage.remove(userIds)
        _ <- removeUsersFromTeamConversations(userIds)
        _ <- removeUnconnectedUsers(userIds)
      } yield {}
    }
  }

  private def removeUsersFromTeamConversations(users: Set[UserId]) = {
    for {
      convs           <- searchTeamConversations().map(_.map(_.id))
      membersToRemove = for (u <- users; c <- convs) yield (u, c)
      _               <- convMemberStorage.remove(membersToRemove)
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
