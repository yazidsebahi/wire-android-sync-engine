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

import java.util.Date

import com.waz.api.impl.ErrorResponse
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.{EventPipeline, Timeouts, ZMessaging}
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.PushNotificationsClient.LoadNotificationsResponse
import com.waz.sync.client.{PushNotification, PushNotificationsClient}
import com.waz.testutils.Matchers.eventually
import com.waz.testutils.{MockZMessaging, TestUserPreferences}
import com.waz.threading.CancellableFuture
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils.{FakeWakeLock, WakeLock}
import com.waz.utils.events.EventContext.Implicits.{global => evc}
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet.ZNetClient
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.threeten.bp.Instant

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

class PushServiceSpec extends AndroidFreeSpec { test =>

  val wsConnected = Signal(false)

  val context = mock[Context]
  val pipeline = mock[EventPipeline]
  val webSocket = mock[WebSocketClientService]
  val znet = mock[ZNetClient]
  val sync = mock[SyncServiceHandle]

  val lastId = Uid()
  val orgNots = Vector(PushNotification(lastId, Vector.empty))
  var notifications = orgNots

  var loadFailure = false
  lazy val client = new PushNotificationsClient(znet) {
    override def loadNotifications(since: Option[Uid], client: ClientId): ErrorOrResponse[LoadNotificationsResponse] = {
      if (loadFailure) CancellableFuture.successful(Left(ErrorResponse.InternalError))
      else CancellableFuture.successful(Right( LoadNotificationsResponse(notifications, hasMore = false, None) ))
    }
  }

  (webSocket.connected _).expects().anyNumberOfTimes().returning(wsConnected)
  (webSocket.client _).expects().anyNumberOfTimes().returning(Signal.const(None))
  (pipeline.apply _).expects(*).anyNumberOfTimes().returning(Future.successful({}))
  var syncPerformed = 0
  (sync.performFullSync _).expects().anyNumberOfTimes().onCall { _ => Future.successful { syncPerformed += 1 } }

  private def createService: PushServiceImpl = {
    new PushServiceImpl(context, new TestUserPreferences(), client, ClientId(), pipeline, webSocket, sync){
      override lazy val wakeLock: WakeLock = new FakeWakeLock
    }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    wsConnected ! false
    awaitAllTasks
    syncPerformed = 0
    notifications = orgNots
    loadFailure = false
  }

  feature("Updating lastNotificationId pref") {
   scenario("Update pref on fresh notification") {
     val service = createService

      service.updateLastNotificationId(Uid(3, 3))
      service.updateLastNotificationId(Uid(4, 4))
      service.updateLastNotificationId(Uid(5, 5))
      withDelay(service.lastNotificationId() should eventually(be(Some(Uid(5, 5)))))
    }

    scenario("Update pref only when notification processing is completed") {
      val service = createService

      val p = Promise[Unit]()
      p.future.onSuccess { case _ => service.updateLastNotificationId(Uid(2, 2)) }
      service.updateLastNotificationId(Uid(5, 5))
      p.trySuccess(())
      awaitAllTasks
      withDelay(service.lastNotificationId() should eventually(be(Some(Uid(2, 2)))))
    }

    scenario("Update pref to last received notification when it's completed") {
      val service = createService

      val p1 = Promise[Unit]()
      val p2 = Promise[Unit]()
      p1.future.onSuccess { case _ => service.updateLastNotificationId(Uid(3, 3)) }
      p2.future.onSuccess { case _ => service.updateLastNotificationId(Uid(4, 4)) }
      p1.trySuccess(())
      awaitAllTasks
      p2.trySuccess(())
      awaitAllTasks
      withDelay(service.lastNotificationId() should eventually(be(Some(Uid(4, 4)))))
    }

    scenario("store last notification Id on new event") {
      val service = createService

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      withDelay {
        service.lastNotificationId should eventually(be('defined))
      }
      awaitAllTasks

      val id = Uid()
      service.onPushNotification(PushNotification(id, Seq(MemberJoinEvent(RConvId(), new Date, UserId(), Nil))))
      withDelay {
        service.lastNotificationId should eventually(be(Some(id)))
      }

    }

    scenario("don't store id on transient notification") {
      val service = createService

      wsConnected ! true
      awaitAllTasks

      withDelay {
        service.lastNotificationId should eventually(be('defined))
      }

      val newId = Uid()
      service.onPushNotification(PushNotification(newId, Nil, transient = true))
      awaitAllTasks
      withDelay(service.lastNotificationId should eventually(be(lastId)))
    }

    scenario("don't update id on otr notification not intended for us") {
      val service = createService

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      withDelay {
        service.lastNotificationId should eventually(be('defined))
      }
      awaitAllTasks

      service.onPushNotification(PushNotification(Uid(), Seq(OtrMessageEvent(RConvId(), new Date, UserId(), ClientId(), ClientId(), Array.empty))))
      withDelay(service.lastNotificationId should eventually(be(lastId)))
    }
  }

  feature("/notifications") {

    scenario("fetch last notification when there is no local last notification id") {
      val service = createService

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      withDelay {
        syncPerformed shouldEqual 1
        service.lastNotificationId should eventually(be(Some(lastId)))
      }
    }

    scenario("fetch notifications and update last id") {
      val service = createService

      service.updateLastNotificationId(lastId)
      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      notifications = Vector(notification1, notification2)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      withDelay {
        service.lastNotificationId should eventually(be(Some(notification2.id)))
        syncPerformed shouldEqual 0
      }
    }

    scenario("request slow sync and fetch last notification if /notifications returns error") {
      val service = createService
      loadFailure = true

      service.updateLastNotificationId(lastId)
      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      notifications = Vector(notification1, notification2)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      withDelay {
        service.lastNotificationId should eventually(be(Some(lastId)))
        syncPerformed shouldEqual 1
      }
    }

  }
}
