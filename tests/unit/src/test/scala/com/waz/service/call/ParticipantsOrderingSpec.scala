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
package com.waz.service.call

import com.waz.model.VoiceChannelData.ConnectionState
import com.waz.model.VoiceChannelData.ConnectionState._
import com.waz.model.{ConvId, VoiceParticipantData, UserId}
import org.scalatest.{OptionValues, FeatureSpec, Matchers}

class ParticipantsOrderingSpec extends FeatureSpec with Matchers with OptionValues {

  lazy val self = UserId("self")

  feature("Ordering of group call participants") {
    scenario("For an odd number of participants, self should always be exactly in middle position") {
      sortedParticipants(participants("user-1", "user-2", "user-3", "user-4", "self"): _*).find(_.userId == self).value.idx shouldEqual 2
    }

    scenario("For an even number of participants, self should always be exactly in the left middle position") {
      sortedParticipants(participants("user-1", "user-2", "user-3", "self"): _*).find(_.userId == self).value.idx shouldEqual 1
    }

    scenario("Inactive users should always have position index -1") {
      sortedParticipants(
        participant("user-1", Idle), participant("user-2"), participant("user-3", Connecting), participant("user-4", Unknown), participant("self")
      ).filter(_.idx == -1).map(_.userId) should contain theSameElementsAs Seq("user-1", "user-4").map(UserId)
    }

    scenario("Inactive users should not count with regards to self's middle position") {
      sortedParticipants(participants("self", "user-1", "user-2") :+ participant("user-3", Idle) :+ participant("user-4", Idle) :_*).find(_.userId == self).value.idx shouldEqual 1
    }
  }

  def participants(ids: String*): Seq[VoiceParticipantData] = ids map (participant(_))
  def participant(id: String, state: ConnectionState = Connected): VoiceParticipantData = VoiceParticipantData(ConvId(), UserId(id), state)

  def sortedParticipants(ps: VoiceParticipantData*): Seq[VoiceParticipantData] =
    VoiceChannelHandle.assignOrderingIndexToParticipants(_.sorted)(ps.map { p => p.userId -> p } .toMap, self).values.toSeq
}
