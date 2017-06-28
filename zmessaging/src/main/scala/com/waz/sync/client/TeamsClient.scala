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
package com.waz.sync.client

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.{debug, warn}
import com.waz.model._
import com.waz.sync.client.TeamsClient.TeamBindingResponse
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonDecoder
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._
import org.json.JSONObject

import scala.util.Try

trait TeamsClient {
  def getTeams(start: Option[TeamId]): ErrorOrResponse[TeamBindingResponse]
  def getTeams(id: Set[TeamId]): ErrorOrResponse[TeamBindingResponse]
  def getTeamMembers(id: TeamId): ErrorOrResponse[Set[UserId]]
  def getTeamData(id: TeamId): ErrorOrResponse[TeamData]
  def getTeamId(start: Option[TeamId] = None): ErrorOrResponse[Option[TeamId]]
}

class TeamsClientImpl(zNetClient: ZNetClient) extends TeamsClient {
  import TeamsClient._
  import Threading.Implicits.Background

  override def getTeams(start: Option[TeamId]) = {
    zNetClient.withErrorHandling("loadAllTeams", Request.Get(teamsPaginatedQuery(start))) {
      case Response(SuccessHttpStatus(), TeamBindingResponse(teams, hasMore), _) => TeamBindingResponse(teams, hasMore)
    }
  }

  override def getTeams(ids: Set[TeamId]) = {
    zNetClient.withErrorHandling("loadBatchTeams", Request.Get(teamsBatchQuery(ids))) {
      case Response(SuccessHttpStatus(), TeamBindingResponse(teams, _), _) => TeamBindingResponse(teams, hasMore = false)
    }
  }

  override def getTeamMembers(id: TeamId) = {
    zNetClient.withErrorHandling("loadTeamMembers", Request.Get(teamMembersPath(id))) {
      case Response(SuccessHttpStatus(), TeamMembersResponse(members), _) => members.map(_._1)
    }
  }

  override def getTeamData(id: TeamId) = {
    zNetClient.withErrorHandling("loadTeamData", Request.Get(teamQuery(id))) {
      case Response(SuccessHttpStatus(), TeamResponse(data), _) => data
    }
  }

  override def getTeamId(start: Option[TeamId] = None): ErrorOrResponse[Option[TeamId]] = getTeams(start).flatMap {
    case Left(err) => CancellableFuture.successful(Left(err))
    case Right(TeamBindingResponse(teams, hasMore)) =>
      debug(s"getTeamId received data: $teams, hasMore? $hasMore")
      teams.find(_._2).map(_._1) match {
        case Some(teamId) => CancellableFuture.successful(Right(Some(teamId)))
        case None if hasMore => getTeamId(teams.lastOption.map(_._1))
        case None => CancellableFuture.successful(Right(None))
      }
  }
}

object TeamsClient {

  def apply(zNetClient: ZNetClient): TeamsClient = new TeamsClientImpl(zNetClient)

  val TeamsPath = "/teams"
  val TeamsPageSize = 100

  def teamMembersPath(id: TeamId) = s"$TeamsPath/${id.str}/members"

  def teamsPaginatedQuery(start: Option[TeamId]): String =
    Request.query(TeamsPath, ("size", TeamsPageSize) :: start.toList.map("start" -> _.str) : _*)

  def teamsBatchQuery(ids: Set[TeamId]): String =
    Request.query(TeamsPath, ("ids", ids.mkString(",")))

  def teamQuery(id: TeamId): String = Request.query(TeamsPath, "id" -> id)

  import JsonDecoder._

  case class TeamBindingResponse(teams: Seq[(TeamId, Boolean)], hasMore: Boolean)

  object TeamBindingResponse {
    def unapply(response: ResponseContent): Option[(Seq[(TeamId, Boolean)], Boolean)] =
      response match {
        case JsonObjectResponse(js) if js.has("teams") =>
          Try(decodeSeq('teams)(js, TeamBindingDecoder), decodeOptBoolean('has_more)(js).getOrElse(false)).toOption
        case _ =>
          warn(s"Unexpected response: $response")
          None
      }
  }

  lazy val TeamBindingDecoder: JsonDecoder[(TeamId, Boolean)] = new JsonDecoder[(TeamId, Boolean)] {
    override def apply(implicit js: JSONObject): (TeamId, Boolean) = {
      import JsonDecoder._
      (TeamId('id), decodeOptBoolean('binding).getOrElse(false))
    }
  }

  object TeamMembersResponse {
    lazy val MemberDecoder = new JsonDecoder[(UserId, Long, Long)] {
      override def apply(implicit js: JSONObject) = (decodeId[UserId]('user), decodeInt('self)('permissions), decodeInt('copy)('permissions))
    }

    def unapply(response: ResponseContent): Option[Set[(UserId, Long, Long)]] = {
      response match {
        case JsonObjectResponse(js) if js.has("members") =>
          Try(decodeSet('members)(js, MemberDecoder)).toOption
        case _ =>
          warn(s"Unexpected response: $response")
          None
      }
    }
  }

  object TeamResponse {
    def unapply(response: ResponseContent): Option[TeamData] =
      response match {
        case JsonObjectResponse(js) => Try(TeamData.Decoder(js)).toOption
        case _ =>
          warn(s"Unexpected response: $response")
          None
      }
  }
}
