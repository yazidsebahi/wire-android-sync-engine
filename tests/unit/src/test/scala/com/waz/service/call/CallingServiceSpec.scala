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

import com.waz.api.NetworkMode
import com.waz.model.UserId
import com.waz.service.{MediaManagerService, NetworkModeService}
import com.waz.utils.events.Signal
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.Future

class CallingServiceSpec extends FeatureSpec with Matchers with MockFactory {


  val avsMock = mock[AvsV3]
  val flows   = mock[FlowManagerService]
  val mm      = mock[MediaManagerService]
  val network = mock[NetworkModeService]

  feature("Group tests with features") {
    scenario("CallingService intialization") {
      println("hello")
      1 shouldEqual 1

      (avsMock.available _).expects().once().returning(Future.successful({}))

      (flows.flowManager _).expects().once().returning(None)
      (mm.mediaManager _).expects().once().returning(None)
      (network.networkMode _).expects().once().returning(Signal.empty[NetworkMode])

      (avsMock.init _).expects(*).once().onCall{ service: CallingService =>
        println("Init was called, horray!")
        Future.successful({})
      }

      new DefaultCallingService(null, UserId(), avsMock, null, null, null, flows, null, mm, null, null, network, null)

    }
  }

}
