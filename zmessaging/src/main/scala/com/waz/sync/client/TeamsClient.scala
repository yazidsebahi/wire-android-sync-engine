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
import com.waz.ZLog.warn
import com.waz.model._
import com.waz.sync.client.TeamsClient.TeamsResponse
import com.waz.threading.Threading
import com.waz.utils.JsonDecoder
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._
import org.json.JSONObject

import scala.util.Try

trait TeamsClient {
  def getTeams(start: Option[TeamId]): ErrorOrResponse[TeamsResponse]
  def getTeams(id: Set[TeamId]): ErrorOrResponse[TeamsResponse]
  def getTeamMembers(id: TeamId): ErrorOrResponse[Set[TeamMemberData]]
}

class TeamsClientImpl(zNetClient: ZNetClient) extends TeamsClient {
  import TeamsClient._
  import Threading.Implicits.Background

  override def getTeams(start: Option[TeamId]) = {
    zNetClient.withErrorHandling("loadAllTeams", Request.Get(teamsPaginatedQuery(start))) {
      case Response(SuccessHttpStatus(), TeamsResponse(teams, hasMore), _) => TeamsResponse(teams, hasMore)
    }
  }

  override def getTeams(ids: Set[TeamId]) = {
    zNetClient.withErrorHandling("loadBatchTeams", Request.Get(teamsBatchQuery(ids))) {
      case Response(SuccessHttpStatus(), TeamsResponse(teams, _), _) => TeamsResponse(teams, hasMore = false)
    }
  }

  override def getTeamMembers(id: TeamId) = {
    zNetClient.withErrorHandling("loadTeamMembers", Request.Get(teamMembersPath(id))) {
      case Response(SuccessHttpStatus(), TeamMembersResponse(members), _) =>
        members.map { case (userId, permissions) => TeamMemberData(userId, id, permissions)}
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

  import JsonDecoder._

  case class TeamsResponse(teams: Set[TeamData], hasMore: Boolean)

  object TeamsResponse {
    def unapply(response: ResponseContent): Option[(Set[TeamData], Boolean)] =
      response match {
        case JsonObjectResponse(js) if js.has("teams") =>
          Try(JsonDecoder.decodeSet('teams)(js, TeamData.Decoder), decodeOptBoolean('has_more)(js).getOrElse(false)).toOption
        case _ =>
          warn(s"Unexpected response: $response")
          None
      }
  }

  object TeamMembersResponse {
    lazy val MemberDecoder = new JsonDecoder[(UserId, Long)] {
      override def apply(implicit js: JSONObject) = {
        //TODO do we want to keep the 'copy' permissions for display
        (decodeId[UserId]('user), decodeInt('self)('permissions))
      }
    }

    def unapply(response: ResponseContent): Option[Set[(UserId, Long)]] = {
      response match {
        case JsonObjectResponse(js) if js.has("members") =>
          Try(decodeSet('members)(js, MemberDecoder)).toOption
        case _ =>
          warn(s"Unexpected response: $response")
          None
      }
    }
  }
}
