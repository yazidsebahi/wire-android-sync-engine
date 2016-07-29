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

import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.model.SearchQuery.{Recommended, TopPeople}
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.JsonDecoder
import com.waz.znet.Response.Status.NotFound
import com.waz.znet.Response.{Status, SuccessHttpStatus}
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet.{JsonObjectResponse, _}
import org.json.JSONObject

import scala.util.control.NonFatal

class UserSearchClient(netClient: ZNetClient) {
  import Threading.Implicits.Background
  import UserSearchClient._
  private implicit val tag: LogTag = logTagFor[UserSearchClient]

  def graphSearch(query: SearchQuery, limit: Int): ErrorOrResponse[Seq[UserSearchEntry]] = {
    debug(s"graphSearch('$query', $limit)")
    query match {
      case TopPeople           => extractUsers("TopPeople", Request.Get(Request.query(TopUserPath, "size" -> limit)))
      case Recommended(prefix) => extractUsers(s"Recommended($prefix)", Request.Get(graphSearchQuery(prefix, limit, Relation.Third.id, useDirectory = true)))
    }
  }

  def graphSearchQuery(query: String, limit: Int, level: Int, useDirectory: Boolean): String =
    Request.query(GraphSearchPath, "q" -> query, "size" -> limit, "l" -> level, "d" -> (if (useDirectory) 1 else 0))

  def loadCommonConnections(id: UserId): ErrorOrResponse[Seq[UserSearchEntry]] =
    extractUsers("loadCommonConnections", Request.Get(CommonConnectionsPath + "/" + id))

  private def extractUsers(name: String, req: Request[Unit]): ErrorOrResponse[Seq[UserSearchEntry]] = {
    val handling404s: PartialFunction[Response, Either[ErrorResponse, Seq[UserSearchEntry]]] = {
      case Response(SuccessHttpStatus(), UserSearchResponse(users), _) =>
        debug(s"user search received: $users")
        Right(users)
      case Response(status, _, _) if status.status == NotFound =>
        warn(s"search service doesn't know about us yet")
        Left(ErrorResponse(Status.RateLimiting, "user not found", "internal-error")) // convert to rate limited so that it will be retried...
    }

    netClient(req) map (handling404s orElse ZNetClient.errorHandling(name))
  }
}

object UserSearchClient {
  val GraphSearchPath = "/search/contacts"
  val CommonConnectionsPath = "/search/common"
  val TopUserPath = "/search/top"

  case class UserSearchEntry(id: UserId, name: String, email: Option[EmailAddress], phone: Option[PhoneNumber], colorId: Int, connected: Option[Boolean], blocked: Boolean, level: Relation, commonCount: Option[Int] = None, common: Seq[UserId] = Nil)

  object UserSearchEntry {
    import JsonDecoder._

    implicit lazy val Decoder: JsonDecoder[UserSearchEntry] = new JsonDecoder[UserSearchEntry] {
      override def apply(implicit js: JSONObject): UserSearchEntry =
        UserSearchEntry('id, 'name, 'email, 'phone, decodeOptInt('accent_id).getOrElse(0), 'connected, 'blocked,
          decodeOptInt('level).fold(Relation.Other)(Relation.withId), 'total_mutual_friends, decodeStringSeq('mutual_friends).map(UserId(_)))
    }
  }

  object UserSearchResponse {
    private implicit val logTag: LogTag = logTagFor(UserSearchResponse)

    def unapply(resp: ResponseContent): Option[Seq[UserSearchEntry]] = resp match {
      case JsonObjectResponse(js) if js.has("documents") =>
        try {
          Some(JsonDecoder.array[UserSearchEntry](js.getJSONArray("documents")))
        } catch {
          case NonFatal(ex) => warn(s"parsing failed", ex)
            None
        }
      case _ => None
    }
  }
}
