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
import com.waz.ZLog._
import com.waz.api.IConversation.{Access, AccessRole}
import com.waz.api.impl.ErrorResponse
import com.waz.model.ConversationData.{ConversationType, Link}
import com.waz.model._
import com.waz.service.BackendConfig
import com.waz.sync.client.ConversationsClient.ConversationResponse.{ConversationIdsResponse, ConversationsResult}
import com.waz.utils.JsonEncoder.{encodeAccess, encodeAccessRole}
import com.waz.utils.{Json, JsonDecoder, JsonEncoder, returning, _}
import com.waz.znet.ZNetClient._
import com.waz.znet.{JsonArrayResponse, JsonObjectResponse, ResponseContent}
import com.waz.znet2.AuthRequestInterceptor
import com.waz.znet2.http._
import org.json
import org.json.{JSONArray, JSONObject}
import org.threeten.bp.Instant

import scala.util.control.NonFatal

trait ConversationsClient {
  import ConversationsClient._
  def loadConversationIds(start: Option[RConvId] = None): ErrorOrResponse[ConversationIdsResponse]
  def loadConversations(start: Option[RConvId] = None, limit: Int = ConversationsPageSize): ErrorOrResponse[ConversationsResult]
  def loadConversations(ids: Seq[RConvId]): ErrorOrResponse[Seq[ConversationResponse]]
  def loadConversation(id: RConvId): ErrorOrResponse[ConversationResponse]
  def postName(convId: RConvId, name: String): ErrorOrResponse[Option[RenameConversationEvent]]
  def postConversationState(convId: RConvId, state: ConversationState): ErrorOrResponse[Boolean]
  def postMemberJoin(conv: RConvId, members: Set[UserId]): ErrorOrResponse[Option[MemberJoinEvent]]
  def postMemberLeave(conv: RConvId, user: UserId): ErrorOrResponse[Option[MemberLeaveEvent]]
  def createLink(conv: RConvId): ErrorOrResponse[Link]
  def removeLink(conv: RConvId): ErrorOrResponse[Unit]
  def getLink(conv: RConvId): ErrorOrResponse[Option[Link]]
  def postAccessUpdate(conv: RConvId, access: Set[Access], accessRole: AccessRole): ErrorOrResponse[Unit]
  //TODO Use case class instead of this parameters list
  def postConversation(users: Set[UserId], name: Option[String] = None, team: Option[TeamId], access: Set[Access], accessRole: AccessRole): ErrorOrResponse[ConversationResponse]
}

class ConversationsClientImpl(implicit
                              private val backendConfig: BackendConfig,
                              private val httpClient: HttpClient,
                              private val authRequestInterceptor: AuthRequestInterceptor) extends ConversationsClient {

  import BackendConfig.backendUrl
  import ConversationsClient._
  import HttpClient.dsl._
  import com.waz.threading.Threading.Implicits.Background

  private implicit val ConversationIdsResponseDeserializer: RawBodyDeserializer[ConversationIdsResponse] =
    RawBodyDeserializer[JSONObject].map { json =>
      val (ids, hasMore) = ConversationIdsResponse.unapply(JsonObjectResponse(json)).get
      ConversationIdsResponse(ids, hasMore)
    }

  override def loadConversationIds(start: Option[RConvId] = None): ErrorOrResponse[ConversationIdsResponse] = {
    val request = Request.withoutBody(url = backendUrl(conversationIdsQuery(start)))
    Prepare(request)
      .withResultType[ConversationIdsResponse]
      .withErrorType[ErrorResponse]
      .executeSafe
  }

  private implicit val ConversationsResultDeserializer: RawBodyDeserializer[ConversationsResult] =
    RawBodyDeserializer[JSONObject].map { json =>
      val (convs, hasMore) = ConversationsResult.unapply(JsonObjectResponse(json)).get
      ConversationsResult(convs, hasMore)
    }

  override def loadConversations(start: Option[RConvId] = None, limit: Int = ConversationsPageSize): ErrorOrResponse[ConversationsResult] = {
    val request = Request.withoutBody(url = backendUrl(conversationsQuery(start, limit)))
    Prepare(request)
      .withResultType[ConversationsResult]
      .withErrorType[ErrorResponse]
      .executeSafe
  }

  override def loadConversations(ids: Seq[RConvId]): ErrorOrResponse[Seq[ConversationResponse]] = {
    val request = Request.withoutBody(url = backendUrl(conversationsQuery(ids = ids)))
    Prepare(request)
      .withResultType[ConversationsResult]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map(_.conversations))
  }

  override def loadConversation(id: RConvId): ErrorOrResponse[ConversationResponse] = {
    val request = Request.withoutBody(url = backendUrl(s"$ConversationsPath/$id"))
    Prepare(request)
      .withResultType[ConversationsResult]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map(_.conversations.head))
  }

  private implicit val EventsResponseDeserializer: RawBodyDeserializer[List[ConversationEvent]] =
    RawBodyDeserializer[JSONObject].map(json => EventsResponse.unapplySeq(JsonObjectResponse(json)).get)

  override def postName(convId: RConvId, name: String): ErrorOrResponse[Option[RenameConversationEvent]] = {
    val request = Request.create(
      url = backendUrl(s"$ConversationsPath/$convId"),
      method = Method.Put,
      body = Json("name" -> name)
    )
    Prepare(request)
      .withResultType[List[ConversationEvent]]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map {
        case (event: RenameConversationEvent) :: Nil => Some(event)
        case _ => None
      })
  }

  //TODO Why not ErrorOrResponse[Unit] as a result type?
  override def postConversationState(convId: RConvId, state: ConversationState): ErrorOrResponse[Boolean] = {
    val request = Request.create(
      url = backendUrl(s"$ConversationsPath/$convId/self"),
      method = Method.Put,
      body = state
    )
    Prepare(request)
      .withResultType[Unit]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map(_ => true))
  }

  override def postMemberJoin(conv: RConvId, members: Set[UserId]): ErrorOrResponse[Option[MemberJoinEvent]] = {
    val request = Request.create(
      url = backendUrl(s"$ConversationsPath/$conv/members"),
      method = Method.Post,
      body = Json("users" -> Json(members))
    )
    Prepare(request)
      .withResultType[Option[List[ConversationEvent]]]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map {
        _.collect { case (event: MemberJoinEvent) :: Nil => event }
      })
  }

  override def postMemberLeave(conv: RConvId, user: UserId): ErrorOrResponse[Option[MemberLeaveEvent]] = {
    val request = Request.withoutBody(
      url = backendUrl(s"$ConversationsPath/$conv/members/$user"),
      method = Method.Delete
    )
    Prepare(request)
      .withResultType[Option[List[ConversationEvent]]]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map {
        _.collect { case (event: MemberLeaveEvent) :: Nil => event }
      })
  }

  override def createLink(conv: RConvId): ErrorOrResponse[Link] = {
    val request = Request.create(
      url = backendUrl(s"$ConversationsPath/$conv/code"),
      method = Method.Post,
      body = ""
    )
    Prepare(request)
      .withResultType[Response[JSONObject]]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(
        _.map { response =>
          val js = response.body
          if (response.code == ResponseCode.Success && js.has("uri"))
            Link(js.getString("uri"))
          else if (response.code == ResponseCode.Created && js.getJSONObject("data").has("uri"))
            Link(js.getJSONObject("data").getString("uri"))
          else
            throw new IllegalArgumentException(s"Can not extract link from json: $js")
        }
      )
  }

  def removeLink(conv: RConvId): ErrorOrResponse[Unit] = {
    val request = Request.withoutBody(
      url = backendUrl(s"$ConversationsPath/$conv/code"),
      method = Method.Delete
    )
    Prepare(request)
      .withResultType[Unit]
      .withErrorType[ErrorResponse]
      .executeSafe
  }

  def getLink(conv: RConvId): ErrorOrResponse[Option[Link]] = {
    val request = Request.withoutBody(url = backendUrl(s"$ConversationsPath/$conv/code"))
    Prepare(request)
      .withResultHttpCodes(ResponseCode.successCodes + ResponseCode.NotFound)
      .withResultType[Response[JSONObject]]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(
        _.map { response =>
          val js = response.body
          if (ResponseCode.isSuccessful(response.code) && js.has("uri"))
            Some(Link(js.getString("uri")))
          else if (response.code == ResponseCode.NotFound)
            None
          else
            throw new IllegalArgumentException(s"Can not extract link from json: $js")
        }
      )
  }

  def postAccessUpdate(conv: RConvId, access: Set[Access], accessRole: AccessRole): ErrorOrResponse[Unit] = {
    val request = Request.create(
      url = backendUrl(accessUpdatePath(conv)),
      method = Method.Put,
      body = Json(
        "access" -> encodeAccess(access),
        "access_role" -> encodeAccessRole(accessRole)
      )
    )
    Prepare(request)
      .withResultType[Unit]
      .withErrorType[ErrorResponse]
      .executeSafe
  }

  def postConversation(users: Set[UserId], name: Option[String] = None, team: Option[TeamId], access: Set[Access], accessRole: AccessRole): ErrorOrResponse[ConversationResponse] = {
    debug(s"postConversation($users, $name)")
    val payload = JsonEncoder { o =>
      o.put("users", Json(users))
      name.foreach(o.put("name", _))
      team.foreach(t => o.put("team", returning(new json.JSONObject()) { o =>
        o.put("teamid", t.str)
        o.put("managed", false)
      }))
      o.put("access", encodeAccess(access))
      o.put("access_role", encodeAccessRole(accessRole))
    }
    val request = Request.create(url = backendUrl(ConversationsPath), method = Method.Post, body = payload)
    Prepare(request)
      .withResultType[ConversationsResult]
      .withErrorType[ErrorResponse]
      .executeSafe
      .map(_.map(_.conversations.head))
  }
}

object ConversationsClient {
  val ConversationsPath = "/conversations"
  val ConversationIdsPath = "/conversations/ids"
  val ConversationsPageSize = 100
  val ConversationIdsPageSize = 1000
  val IdsCountThreshold = 32

  def accessUpdatePath(id: RConvId) = s"$ConversationsPath/${id.str}/access"

  def conversationsQuery(start: Option[RConvId] = None, limit: Int = ConversationsPageSize, ids: Seq[RConvId] = Nil): String = {
    val args = (start, ids) match {
      case (None, Nil) =>   Seq("size" -> limit)
      case (Some(id), _) => Seq("size" -> limit, "start" -> id.str)
      case _ =>             Seq("ids" -> ids.mkString(","))
    }
    com.waz.znet.Request.query(ConversationsPath, args: _*)
  }

  def conversationIdsQuery(start: Option[RConvId]): String =
    com.waz.znet.Request.query(ConversationIdsPath, ("size", ConversationIdsPageSize) :: start.toList.map("start" -> _.str) : _*)

  case class ConversationResponse(conversation: ConversationData, members: Seq[ConversationMemberData])

  object ConversationResponse {
    import com.waz.utils.JsonDecoder._

    def memberDecoder(convId: ConvId) = new JsonDecoder[Option[ConversationMemberData]] {
      override def apply(implicit js: JSONObject) = Some(ConversationMemberData('id, convId))
    }

    def conversationData(js: JSONObject, self: JSONObject) = {
      val (creator, name, team, id, convType, lastEventTime, access, accessRole, link) = {
        implicit val jsObj = js
        (
          decodeUserId('creator),
          decodeOptString('name),
          decodeOptId[TeamId]('team),
          decodeRConvId('id),
          ConversationType(decodeInt('type)),
          decodeISOInstant('last_event_time),
          decodeAccess('access),
          decodeOptAccessRole('access_role),
          decodeOptString('link).map(Link)
        )
      }
      val state = ConversationState.Decoder(self)
      //TODO Teams: how do we tell if a conversation is managed, currently defaulting to false
      val isManaged = team.map(_ => false)

      ConversationData(
        ConvId(id.str),
        id,
        name.filterNot(_.isEmpty),
        creator,
        convType,
        team,
        isManaged,
        lastEventTime,
        isActive = true,
        Instant.EPOCH,
        state.muted.getOrElse(false),
        state.muteTime.getOrElse(lastEventTime),
        state.archived.getOrElse(false),
        state.archiveTime.getOrElse(lastEventTime),
        access = access,
        accessRole = accessRole,
        link = link
      )
    }

    implicit lazy val Decoder: JsonDecoder[ConversationResponse] = new JsonDecoder[ConversationResponse] {
      override def apply(implicit js: JSONObject): ConversationResponse = {
        debug(s"decoding response: $js")
        val members = js.getJSONObject("members")
        val self = members.getJSONObject("self")
        val conversation = conversationData(js, self)
        ConversationResponse(conversation, array(members.getJSONArray("others"))(memberDecoder(conversation.id)).flatten)
      }
    }

    case class ConversationsResult(conversations: Seq[ConversationResponse], hasMore: Boolean)
    
    object ConversationsResult {

      def unapply(response: ResponseContent): Option[(List[ConversationResponse], Boolean)] = try {
        response match {
          case JsonObjectResponse(js) if js.has("conversations") =>
            Some((array[ConversationResponse](js.getJSONArray("conversations")).toList, decodeBool('has_more)(js)))
          case JsonArrayResponse(js) => Some((array[ConversationResponse](js).toList, false))
          case JsonObjectResponse(js) => Some((List(Decoder(js)), false))
          case _ => None
        }
      } catch {
        case NonFatal(e) =>
          warn(s"couldn't parse conversations response: $response", e)
          warn("json decoding failed", e)
          None
      }
    }

    case class ConversationIdsResponse(ids: Seq[RConvId], hasMore: Boolean)

    object ConversationIdsResponse {

      val idExtractor = { (arr: JSONArray, i: Int) => RConvId(arr.getString(i)) }

      def unapply(response: ResponseContent): Option[(Seq[RConvId], Boolean)] = try {
        response match {
          case JsonObjectResponse(js) if js.has("conversations") => Some((array[RConvId](js.getJSONArray("conversations"), idExtractor), decodeBool('has_more)(js)))
          case JsonArrayResponse(js) => Some((array[RConvId](js, idExtractor), false))
          case _ => None
        }
      } catch {
        case NonFatal(e) =>
          warn(s"couldn't parse conversations response: $response", e)
          None
      }
    }
  }

  object EventsResponse {
    import com.waz.utils.JsonDecoder._

    def unapplySeq(response: ResponseContent): Option[List[ConversationEvent]] = try {
      response match {
        case JsonObjectResponse(js) if js.has("events") => Some(array[ConversationEvent](js.getJSONArray("events")).toList)
        case JsonArrayResponse(js) => Some(array[ConversationEvent](js).toList)
        case JsonObjectResponse(js) => Some(List(implicitly[JsonDecoder[ConversationEvent]].apply(js)))
        case _ => None
      }
    } catch {
      case NonFatal(e) =>
        warn(s"couldn't parse events response $response", e)
        None
    }
  }
}
