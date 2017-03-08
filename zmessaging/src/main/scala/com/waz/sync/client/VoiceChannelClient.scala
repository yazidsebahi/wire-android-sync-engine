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
import com.waz.api.CauseForCallStateEvent
import com.waz.model.Event.EventDecoder
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._

import scala.util.{Failure, Success, Try}

class VoiceChannelClient(netClient: ZNetClient) {
  import Threading.Implicits.Background
  import com.waz.sync.client.VoiceChannelClient._
  private implicit val logTag: LogTag = logTagFor[VoiceChannelClient]

  def loadCallState(id: RConvId): ErrorOrResponse[CallStateEvent] = netClient.withErrorHandling(s"loadCallState($id)", Request.Get(callStatePath(id))) {
    case Response(SuccessHttpStatus(), CallState(device, participants, cause, s, idx), _) => CallStateEvent(id, Some(participants), Some(device), cause, s, idx)
  }

  def updateSelfCallState(id: RConvId, deviceState: CallDeviceState, cause: CauseForCallStateEvent): ErrorOrResponse[Either[JoinCallFailed, CallStateEvent]] = {
    val payload = JsonEncoder { o =>
      o.put("self", CallDeviceState.Encoder(deviceState))
      o.put("cause", mapCauseToPermittedBackendValues(cause).asJson)
    }
    netClient.withErrorHandling("updateSelfCallState", Request.Put(callStatePath(id), payload)) {
      case Response(SuccessHttpStatus(), CallState(d, p, c, s, idx), _) => Right(CallStateEvent(id, Some(p), Some(d), c, s, idx))
      case Response(status, JoinCallFailed(label, maxJoined, memberCount, maxMembers), _) if status.status == 409 && deviceState.joined => Left(JoinCallFailed(label, maxJoined, memberCount, maxMembers))
    }
  }

  private def mapCauseToPermittedBackendValues(cause: CauseForCallStateEvent): CauseForCallStateEvent = // TODO remove this once backend supports all our causes
    if (causesKnownToBackend contains cause) cause else CauseForCallStateEvent.REQUESTED
  
}

object VoiceChannelClient {
  val Joined = "joined"
  val Idle = "idle"

  val causesKnownToBackend = Set(CauseForCallStateEvent.DISCONNECTED, CauseForCallStateEvent.INTERRUPTED, CauseForCallStateEvent.REQUESTED, CauseForCallStateEvent.GONE)
  
  def callStatePath(conv: RConvId) = s"/conversations/$conv/call/state"

  case class JoinCallFailed(label: String, maxJoined: Option[Int], memberCount: Option[Int], maxMembers: Option[Int])

  object JoinCallFailed {
    import JsonDecoder._

    def unapply(resp: ResponseContent): Option[(String, Option[Int], Option[Int], Option[Int])] = resp match {
      case JsonObjectResponse(js) =>
        implicit val _js = js
        Try((decodeString('label), decodeOptInt('max_joined), decodeOptInt('member_count), decodeOptInt('max_members))).toOption
      case _ => None
    }
  }

  object CallState {
    private implicit val logTag: LogTag = logTagFor(CallState)

    def unapply(resp: ResponseContent): Option[(CallDeviceState, Set[CallParticipant], CauseForCallStateEvent, Option[CallSessionId], Option[CallSequenceNumber])] = resp match {
      case JsonObjectResponse(js) =>
        Try {
          val s = js.getJSONObject("self")
          (EventDecoder.callDeviceState(s), EventDecoder.callParticipants(js), EventDecoder.cause(js), JsonDecoder.decodeOptId('session)(js, CallSessionId.Id), JsonDecoder.decodeOptCallSequenceNumber('sequence)(js))
        } match {
          case Success(res) => Some(res)
          case Failure(ex) => error("call state parsing failed", ex); None
        }
      case _ => None
    }
  }
}
