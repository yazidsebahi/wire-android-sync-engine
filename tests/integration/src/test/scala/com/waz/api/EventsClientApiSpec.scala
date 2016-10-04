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
package com.waz.api

import com.waz.ZLog._
import com.waz.model.Uid
import com.waz.model.otr.ClientId
import com.waz.sync.client.EventsClient
import com.waz.sync.client.EventsClient.LoadNotificationsResponse
import com.waz.testutils.Matchers._
import com.waz.threading.Threading
import com.waz.utils.events.EventContext
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class EventsClientApiSpec extends FeatureSpec with Matchers with BeforeAndAfter with ProvisionedApiSpec {

  private implicit val logTag: LogTag = logTagFor[EventsClientApiSpec]
  implicit val ec = EventContext.Global
  implicit val timeout = 10.seconds: Timeout
  override val provisionFile = "/one_user.json"

  lazy val client: EventsClient = zmessaging.eventsClient

  var lastResponse = Option.empty[LoadNotificationsResponse]

  override def beforeAll(): Unit = {
    super.beforeAll()
    client.onNotificationsPageLoaded.on(Threading.Ui) { r =>
      lastResponse = Some(r)
    }
  }

  before {
    lastResponse = Option.empty[LoadNotificationsResponse]
  }

  scenario("Load all notifications") {
    val since = Option(Uid())
    Await.result(client.loadNotifications(since = since, client = ClientId(), pageSize = 100), timeout) match {
      case Right(Some(lastId)) => info(s"received notifications, ok - last id is $lastId")
      case _ => fail("load notifications failed - got None")
    }

    {
      lastResponse should beMatching({ case Some(LoadNotificationsResponse(_, false)) => () })
    } .soon
  }
}
