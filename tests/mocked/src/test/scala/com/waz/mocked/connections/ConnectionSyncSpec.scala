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
package com.waz.mocked.connections

import com.waz.api.MockedClientApiSpec
import com.waz.mocked.{MockBackend, PushBehaviour, Timeline}
import com.waz.model.UserData.ConnectionStatus
import com.waz.model.{RConvId, Uid, UserConnectionEvent, UserId}
import com.waz.testutils.Matchers._
import com.waz.testutils.TestApplication
import com.waz.testutils.TestApplication._
import org.robolectric.annotation.Config
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.concurrent.duration._

@Config(application = classOf[TestApplication])
class ConnectionSyncSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with MockBackend with MockedClientApiSpec { test =>

  val past = Timeline.sometimeInThePast

  lazy val convs = api.getConversations
  lazy val incoming = convs.getIncomingConversations
  lazy val inbox = api.getIncomingMessages

  override protected def beforeAll(): Unit = {
    1 to 3 map (_ => UserId()) foreach { userId =>
      connections(userId) = UserConnectionEvent(Uid(), RConvId(), selfUserId, userId, Some(s"Hello, let's connect $userId"), ConnectionStatus.PendingFromOther, past.next())
    }

    super.beforeAll()
  }

  scenario("after initial slow-sync, the incoming connections should be there") {
    soon {
      incoming.size shouldEqual 3
      inbox.size shouldEqual 0
    }
  }

  scenario("The (old) incoming connections should not result in connection request messages in the notifications") {
    notificationsSpy.gcms.flatten should be(empty)
  }

  scenario("The (old) incoming connections should not result in connection request messages in the inbox") {
    awaitUi(10.millis)
    soon { inbox.size shouldEqual 0 }
  }

  scenario("freshly incoming connect requests should still go into the inbox, though") {
    1 to 2 foreach (_ => addIncomingConnectionRequest()(PushBehaviour.Push))

    soon {
      incoming.size shouldEqual 5
      inbox.size shouldEqual 2
    }
  }
}
