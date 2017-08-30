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
import com.waz.ZLog.ImplicitTag._
import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.ErrorResponse.{ConnectionErrorCode, TimeoutCode}
import com.waz.model.otr.ClientId
import com.waz.model._
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonDecoder
import com.waz.utils.JsonDecoder.arrayColl
import com.waz.znet.Response.ErrorStatus
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet.{JsonObjectResponse, _}
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.util.control.NonFatal

class PushNotificationsClient(netClient: ZNetClient, pageSize: Int = PushNotificationsClient.PageSize) {
  import PushNotificationsClient._
  import Threading.Implicits.Background

  def loadNotifications(since: Option[Uid], client: ClientId): ErrorOrResponse[LoadNotificationsResponse] =
    netClient.chainedWithErrorHandling(RequestTag, Request.Get(notificationsPath(since, client, pageSize))) {
      case Response(ErrorStatus(), ErrorResponse(code@(TimeoutCode|ConnectionErrorCode), msg, label), _) =>
        CancellableFuture.successful(Left(ErrorResponse(code, msg, label)))

      case Response(_, PagedNotificationsResponse((notifications, hasMore, time)), _) =>
        CancellableFuture.successful(Right( LoadNotificationsResponse(notifications, hasMore, time) ))
    }
}

object PushNotificationsClient {
  val RequestTag = "loadNotifications"
  val NotificationsPath = "/notifications"
  val PageSize = 1000

  def notificationsPath(since: Option[Uid], client: ClientId, pageSize: Int) = {
    val args = Seq("since" -> since, "client" -> Some(client), "size" -> Some(pageSize)) collect { case (key, Some(v)) => key -> v }
    Request.query(NotificationsPath, args: _*)
  }

  case class LoadNotificationsResponse(notifications: Vector[PushNotification], hasMore: Boolean, beTime: Option[Instant])

  object PagedNotificationsResponse {

    import com.waz.utils.JsonDecoder._

    def unapply(response: ResponseContent): Option[(Vector[PushNotification], Boolean, Option[Instant])] = try response match {
      case JsonObjectResponse(js) if js.has("notifications") =>
        Some((arrayColl[PushNotification, Vector](js.getJSONArray("notifications")), decodeBool('has_more)(js), decodeOptISOInstant('time)(js)))
      case JsonArrayResponse(js) => Some((arrayColl[PushNotification, Vector](js), false, None))
      case _ => None
    } catch {
      case NonFatal(e) =>
        warn(s"couldn't parse paged push notifications from response: $response", e)
        None
    }
  }

  object NotificationsResponse {
    def unapplySeq(response: ResponseContent): Option[Seq[PushNotification]] = try response match {
      case JsonObjectResponse(js) => Some(Vector(implicitly[JsonDecoder[PushNotification]].apply(js)))
      case JsonArrayResponse(js) => Some(arrayColl[PushNotification, Vector](js))
      case _ => None
    } catch {
      case NonFatal(e) =>
        warn(s"couldn't parse push notification(s) from response: $response", e)
        None
    }
  }
}

case class PushNotification(id: Uid, events: Seq[Event], transient: Boolean = false) {

  /**
    * Check if notification contains events intended for current client. In some (rare) cases it may happen that
    * BE sends us notifications intended for different device, we can verify that by checking recipient field.
    * Unencrypted events are always considered to belong to us.
    */
  def hasEventForClient(clientId: ClientId) = events.forall(forUs(clientId, _))

  def eventsForClient(clientId: ClientId) = events.filter(forUs(clientId, _))

  private def forUs(clientId: ClientId, event: Event) = event match {
    case ev: OtrEvent => clientId == ev.recipient
    case _ => true
  }
}

object PushNotification {
  implicit lazy val NotificationDecoder: JsonDecoder[PushNotification] = new JsonDecoder[PushNotification] {

    import com.waz.utils.JsonDecoder._

    override def apply(implicit js: JSONObject): PushNotification =
      PushNotification('id, array[Event](js.getJSONArray("payload")), 'transient)
  }
}
