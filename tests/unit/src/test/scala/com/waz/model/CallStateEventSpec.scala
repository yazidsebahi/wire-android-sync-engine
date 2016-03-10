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
package com.waz.model

import com.waz.Generators.arbCallDeviceState
import com.waz.api.CauseForCallStateEvent
import com.waz.model.Event.EventDecoder
import org.json.JSONObject
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FeatureSpec}

class CallStateEventSpec extends FeatureSpec with Matchers with GeneratorDrivenPropertyChecks {
  feature("json de-/serialisation of call device state") {
    scenario("serialise and deserialise") {
      forAll { e: CallDeviceState =>
        EventDecoder.callDeviceState(CallDeviceState.Encoder(e)) shouldEqual e
      }
    }
  }

  feature("json deserialisation of call state events") {
    scenario("deserialise some json data") {
      Event.EventDecoder.callStateEvent(Uid("event-id"))(new JSONObject(callState)) shouldEqual CallStateEvent(
        Uid("event-id"), RConvId("conversation-id"),
        participants = Some(Set(
          CallParticipant(UserId("idle-id"), joined = false, Set.empty),
          CallParticipant(UserId("joined-id"), joined = true, Set.empty),
          CallParticipant(UserId("muted-suspended-id"), joined = true, Set(CallProperty.Muted, CallProperty.Suspended)),
          CallParticipant(UserId("video-id"), joined = true, Set(CallProperty.SendsVideo)))),
        device = Some(CallDeviceState(joined = false, Set(CallProperty.Ignored))),
        cause = CauseForCallStateEvent.REQUESTED,
        sessionId = Some(CallSessionId("session-id")),
        sequenceNumber = Some(CallSequenceNumber(149)))
    }
  }

  def callState =
    s"""{
       |  "type": "call-state",
       |  "conversation": "conversation-id",
       |  "session": "session-id",
       |  "sequence": 149,
       |  "self": {
       |    "state": "idle",
       |    "ignored": true,
       |    "muted": false
       |  },
       |  "participants": {
       |      "idle-id": {
       |        "state": "idle"
       |      },
       |      "joined-id": {
       |        "state": "joined",
       |        "videod": false
       |      },
       |      "muted-suspended-id": {
       |        "state": "joined",
       |        "suspended": true,
       |        "muted": true
       |      },
       |      "video-id": {
       |        "state": "joined",
       |        "videod": true,
       |        "muted": false
       |      }
       |  }
       |
       |}""".stripMargin
}
