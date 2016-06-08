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
package com.waz.service.push

import com.waz.RobolectricUtils
import com.waz.content.{KeyValueStorage, ZStorage}
import com.waz.model.otr.ClientId
import com.waz.model.{Uid, ZUserId}
import com.waz.service.LastNotificationIdService.State._
import com.waz.service.PushService.SlowSyncRequest
import com.waz.service._
import com.waz.sync.client.{EventsClient, PushNotification}
import com.waz.testutils.DefaultPatienceConfig
import com.waz.testutils.Matchers._
import com.waz.threading.CancellableFuture
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils.events.Signal
import com.waz.znet.ZNetClient.EmptyClient
import org.robolectric.Robolectric
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

class LastNotificationIdServiceSpec extends FeatureSpec with Matchers with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig {

  val wsConnected = Signal(false)
  val otrClient = Future successful ClientId()

  val context = Robolectric.application

  lazy val pushSignals = new PushServiceSignals {
    override val pushConnected: Signal[Boolean] = wsConnected
  }
  lazy val eventsClient = new EventsClient(new EmptyClient) {
    override def loadLastNotification(client: ClientId) = CancellableFuture.delayed(100.millis)(Right(lastNotificationResponse))
  }
  lazy val keyValue = new KeyValueService(new KeyValueStorage(context, new ZStorage(ZUserId(), context)), new ReportingService {})
  lazy val service = new LastNotificationIdService(keyValue, pushSignals, eventsClient, otrClient)

  var lastNotificationResponse = Option(PushNotification(Uid(), Nil))

  scenario("Don't update pref when disconnected") {
    service.currentState should eventually(be(Disconnected))
    service.updateLastIdOnNotification(Uid(), Future.successful(()))
    awaitUi(200.millis)
    service.lastNotificationId() should eventually(be('empty))
  }

  scenario("Switch to Waiting on connect") {
    wsConnected ! true
    service.currentState should eventually(be(Waiting))
  }

  scenario("Don't update pref when waiting") {
    service.updateLastIdOnNotification(Uid(2, 2), Future.successful(()))
    awaitUi(200.millis)
    service.lastNotificationId() should eventually(be('empty))
  }

  scenario("Switch to default and update pref to fresh notification once history is synced") {
    service.updateLastIdOnHistorySynced(Some(Uid(1, 1))).futureValue
    awaitUi(100.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(2, 2))))
    service.currentState should eventually(be(Running))
  }

  scenario("Update pref on fresh notification in default state") {
    service.updateLastIdOnNotification(Uid(3, 3), Future.successful(()))
    service.updateLastIdOnNotification(Uid(4, 4), Future.successful(()))
    withDelay(service.lastNotificationId() should eventually(be(Some(Uid(4, 4)))))
    service.updateLastIdOnNotification(Uid(5, 5), Future.successful(()))
    withDelay(service.lastNotificationId() should eventually(be(Some(Uid(5, 5)))))
  }

  scenario("Update pref only when notification processing is completed") {
    val p = Promise[Unit]()
    service.updateLastIdOnNotification(Uid(2, 2), p.future)
    awaitUi(200.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(5, 5))))
    p.trySuccess(())
    awaitUi(100.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(2, 2))))
  }

  scenario("Update pref to last received notification when it's completed") {
    val p1 = Promise[Unit]()
    val p2 = Promise[Unit]()
    service.updateLastIdOnNotification(Uid(3, 3), p1.future)
    service.updateLastIdOnNotification(Uid(4, 4), p2.future)
    p1.trySuccess(())
    awaitUi(200.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(2, 2))))
    p2.trySuccess(())
    awaitUi(100.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(4, 4))))
  }

  scenario("Switch state on disconnect") {
    wsConnected ! false
    service.currentState should eventually(be(Disconnected))
  }

  scenario("Switch from waiting to default on slow sync and fetch last notification from backend") {
    lastNotificationResponse = Some(PushNotification(Uid(6, 6), Nil))
    wsConnected ! true
    pushSignals.onSlowSyncNeeded ! SlowSyncRequest(System.currentTimeMillis())
    withDelay {
      service.lastNotificationId() should eventually(be(Some(Uid(6, 6))))
      service.currentState should eventually(be(Running))
    }
  }

  scenario("Fetch last notification id from server on another slow sync") {
    lastNotificationResponse = Some(PushNotification(Uid(7, 7), Nil))
    pushSignals.onSlowSyncNeeded ! SlowSyncRequest(System.currentTimeMillis())
    withDelay {
      service.lastNotificationId() should eventually(be(Some(Uid(7, 7))))
    }
  }

  scenario("Don't update pref from backend if fresh event was received") {
    wsConnected ! false
    lastNotificationResponse = Some(PushNotification(Uid(8, 8), Nil))
    wsConnected ! true
    pushSignals.onSlowSyncNeeded ! SlowSyncRequest(System.currentTimeMillis())
    awaitUi(50.millis)
    service.updateLastIdOnNotification(Uid(9, 9), Future.successful(()))
    awaitUi(200.millis)
    service.lastNotificationId() should eventually(be(Some(Uid(9, 9))))
    service.currentState should eventually(be(Running))
  }

  scenario("Update pref to last historical id if no fresh notification is received") {
    wsConnected ! false
    wsConnected ! true
    service.updateLastIdOnHistorySynced(Some(Uid(12, 12))).futureValue
    service.lastNotificationId() should eventually(be(Some(Uid(12, 12))))
    service.currentState should eventually(be(Running))
  }
}
