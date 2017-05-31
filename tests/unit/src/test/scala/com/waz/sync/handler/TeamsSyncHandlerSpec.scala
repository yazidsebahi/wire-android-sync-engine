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

import com.waz.api.impl.ErrorResponse
import com.waz.model.{TeamData, TeamId, TeamMemberData, UserId}
import com.waz.service.teams.TeamsService
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncResult
import com.waz.sync.client.TeamsClient
import com.waz.sync.client.TeamsClient.TeamsResponse
import com.waz.threading.CancellableFuture

import scala.concurrent.Future


class TeamsSyncHandlerSpec extends AndroidFreeSpec {


  val client = mock[TeamsClient]
  val service = mock[TeamsService]

  feature("Sync all teams") {

    scenario("Basic single team with some members sync") {

      val teamId = TeamId()
      val teams = Seq(TeamData(teamId, "My Team", UserId()))
      val members = Set(
        TeamMemberData(UserId(), teamId),
        TeamMemberData(UserId(), teamId)
      )

      (client.getTeams(_: Option[TeamId])).expects(None).once().returning(CancellableFuture.successful(Right(TeamsResponse(teams, hasMore = false))))
      (client.getTeamMembers _).expects(teamId).once().returning(CancellableFuture.successful(Right(members)))

      val teamsFetched = teams.toSet
      (service.onTeamsSynced _).expects(teamsFetched, members, true).once().returning(Future.successful({}))

      result(initHandler.syncTeams(Set.empty)) shouldEqual SyncResult.Success

    }

    scenario("Paginated teams with some members sync") {

      val teamIds = Seq(TeamId("1"), TeamId("2"))
      val teams = teamIds.map { id =>
        TeamData(id, s"Team: ${id.str}", UserId())
      }
      val members = teamIds.map { id =>
        Set(
          TeamMemberData(UserId(s"User 1 team ${id.str}"), id),
          TeamMemberData(UserId(s"User 2 team ${id.str}"), id)
        )
      }

      var callsToTeams = 0
      (client.getTeams(_: Option[TeamId])).expects(*).twice().onCall { start: Option[TeamId] =>
        callsToTeams += 1

        val resp = callsToTeams match {
          case 1 =>
            start shouldEqual None
            TeamsResponse(teams.init, hasMore = true)
          case 2 =>
            start shouldEqual teamIds.headOption
            TeamsResponse(teams.tail, hasMore = false)
          case _ => fail("Unexpected number of calls to getTeams")
        }

        CancellableFuture.successful(Right(resp))
      }

      var callsToMembers = 0
      (client.getTeamMembers _).expects(*).twice().onCall { teamId: TeamId =>
        callsToMembers += 1

        val resp = callsToMembers match {
          case 1 =>
            teamId shouldEqual teamIds.head
            members.head
          case 2 =>
            teamId shouldEqual teamIds.last
            members.last
          case _ => fail("Unexpected number of calls to getTeamMembers")
        }

        CancellableFuture.successful(Right(resp))
      }

      val teamsReturned = teams.toSet
      val membersReturned = members.flatten.toSet
      (service.onTeamsSynced _).expects(teamsReturned, membersReturned, true).once().returning(Future.successful({}))
      result(initHandler.syncTeams(Set.empty)) shouldEqual SyncResult.Success
    }

    scenario("Failed members download should fail entire sync") {

      val teamId = TeamId()
      val teams = Seq(TeamData(teamId, "My Team", UserId()))

      val timeoutError = ErrorResponse(ErrorResponse.ConnectionErrorCode, s"Request failed with timeout", "connection-error")

      (client.getTeams(_: Option[TeamId])).expects(None).once().returning(CancellableFuture.successful(Right(TeamsResponse(teams, hasMore = true))))
      (client.getTeamMembers _).expects(teamId).once().returning(CancellableFuture.successful(Left(timeoutError)))

      (service.onTeamsSynced _).expects(*, *, *).never().returning(Future.successful({}))

      result(initHandler.syncTeams(Set.empty)) shouldEqual SyncResult(timeoutError)
    }
  }

  def initHandler = new TeamsSyncHandlerImpl(client, service)

}
