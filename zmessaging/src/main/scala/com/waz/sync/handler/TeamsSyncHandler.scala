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
package com.waz.sync.handler

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.{debug, warn}
import com.waz.api.impl.ErrorResponse
import com.waz.model.{TeamData, TeamId, TeamMemberData}
import com.waz.service.teams.TeamsService
import com.waz.sync.SyncResult
import com.waz.sync.client.TeamsClient
import com.waz.sync.client.TeamsClient.TeamsResponse
import com.waz.sync.handler.TeamsSyncHandler.SyncException
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.ZNetClient.ErrorOrResponse

import scala.concurrent.Future
import scala.util.control.NoStackTrace

trait TeamsSyncHandler {

  def syncTeams(teams: Set[TeamId]): Future[SyncResult]

}

class TeamsSyncHandlerImpl(client: TeamsClient, service: TeamsService) extends TeamsSyncHandler {

  import Threading.Implicits.Background

  override def syncTeams(ids: Set[TeamId]) =
    if (ids.isEmpty) syncAllTeams() else syncBatchTeams(ids)

  private def syncAllTeams(): Future[SyncResult] = {

    def recursive(start: Option[TeamId], teamsDownloaded: Set[TeamData], membersDownloaded: Set[TeamMemberData]): ErrorOrResponse[(Set[TeamData], Set[TeamMemberData])] = {
      client.getTeams(start).flatMap {
        case Right(TeamsResponse(teams, hasMore)) =>
          debug(s"syncTeams received data: $teams, hasMore? $hasMore")
          CancellableFuture.lift(downloadMembers(teams.map(_.id).toSet)).flatMap { teamMembers =>
            if (hasMore) recursive(teams.lastOption.map(_.id), teams.toSet, teamMembers)
            else CancellableFuture.successful(Right((teams.toSet ++ teamsDownloaded, teamMembers ++ membersDownloaded)))
          }
        case Left(err) => CancellableFuture.successful(Left(err))
      }
    }

    recursive(None, Set.empty, Set.empty).future.flatMap {
      case Right((teams, members)) =>  service.onTeamsSynced(teams, members, fullSync = true).map(_ => SyncResult.Success)
      case Left(err) => Future.successful(SyncResult(err))
    }.recover {
      case e@SyncException(_, err) =>
        warn("Failed to sync teams", e)
        SyncResult(err)
    }
  }

  private def syncBatchTeams(ids: Set[TeamId]): Future[SyncResult] = client.getTeams(ids).future.flatMap {
    case Right(TeamsResponse(teams, _)) =>
      for {
        members <- downloadMembers(ids)
        _       <- service.onTeamsSynced(teams.toSet, members)
      } yield SyncResult.Success
    case Left(error) =>
      warn(s"TeamsClient.syncBatchTeams: $ids failed with error: $error")
      Future.successful(SyncResult(error))
  }

  private def downloadMembers(teams: Set[TeamId]): Future[Set[TeamMemberData]] =
    Future.traverse(teams) { id =>
      client.getTeamMembers(id).future.map {
        case Right(teamMembers) =>
          debug(s"Received members for team: $id, $teamMembers")
          teamMembers
        case Left(err) => throw SyncException(s"Failed to download members for team: $id", err)
      }
    }.map(_.flatten)

}

object TeamsSyncHandler {

  def apply(client: TeamsClient, service: TeamsService): TeamsSyncHandler = new TeamsSyncHandlerImpl(client, service)

  case class SyncException(msg: String, err: ErrorResponse) extends Exception(msg) with NoStackTrace
}
