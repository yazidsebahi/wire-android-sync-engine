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

import android.net.Uri
import com.koushikdutta.async.http.WebSocket
import com.waz.api.NetworkMode
import com.waz.api.impl.ErrorResponse
import com.waz.content.UserPreferences.LastStableNotification
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.{EventPipeline, NetworkModeService}
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.PushNotificationsClient.LoadNotificationsResponse
import com.waz.sync.client.{PushNotification, PushNotificationsClient}
import com.waz.testutils.TestUserPreferences
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.{Backoff, FakeWakeLock, WakeLock}
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet._
import org.json.JSONObject

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable

class PushServiceSpec extends AndroidFreeSpec { test =>

  val wsConnected = Signal(false)

  val context = mock[Context]
  val pipeline = mock[EventPipeline]
  val wscService = mock[WebSocketClientService]
  val znet = mock[ZNetClient]
  val sync = mock[SyncServiceHandle]
  val userPrefs = new TestUserPreferences
  val network = mock[NetworkModeService]

  val lastId = Uid()

  implicit val ctx = Threading.Background
  val client = mock[PushNotificationsClient]

  val wsClient = Signal(Option.empty[WebSocketClient])

  (wscService.connected _).expects().anyNumberOfTimes().returning(wsConnected)
  (wscService.client _).expects().anyNumberOfTimes().returning(wsClient)
  (wscService.network _).expects().anyNumberOfTimes().returning(network)
  (pipeline.apply _).expects(*).anyNumberOfTimes().returning(Future.successful({}))

  var syncPerformed = 0
  (sync.performFullSync _).expects().anyNumberOfTimes().onCall { _ =>
    Future.successful { syncPerformed += 1 }
  }

  val networkMode = Signal(NetworkMode._4G)
  (network.networkMode _).expects().anyNumberOfTimes.returning(networkMode)

  val notificationsLog = mutable.ListBuffer[Uid]()

  val service = new PushServiceImpl(context, userPrefs, client, ClientId(), pipeline, wscService, sync) {
    override lazy val wakeLock: WakeLock = new FakeWakeLock

    override def onPushNotifications(allNs: Seq[PushNotification]): Unit = {
      super.onPushNotifications(allNs)
      notificationsLog ++= allNs.map(_.id)
    }
  }

  private def waitForLastNotIdChangeTo(id: Option[Uid]) = {
    val idSig = userPrefs.preference(LastStableNotification).signal
    result(idSig.filter(_ == id).head)
  }

  feature("Updating lastNotificationId pref") {

    scenario("Update pref on fresh notification") {
      waitForLastNotIdChangeTo(None)

      service.updateLastNotificationId(Uid(3, 3))
      service.updateLastNotificationId(Uid(4, 4))
      service.updateLastNotificationId(Uid(5, 5))
      waitForLastNotIdChangeTo(Some(Uid(5, 5)))
    }

    scenario("Update pref only when notification processing is completed") {
      val p = Promise[Unit]()
      p.future.onSuccess { case _ => service.updateLastNotificationId(Uid(2, 2)) }(ctx)
      service.updateLastNotificationId(Uid(5, 5))
      waitForLastNotIdChangeTo(Some(Uid(5, 5)))
      p.trySuccess(())
      waitForLastNotIdChangeTo(Some(Uid(2, 2)))
    }

    scenario("Update pref to last received notification when it's completed") {
      val p1 = Promise[Unit]()
      val p2 = Promise[Unit]()
      p1.future.onSuccess { case _ => service.updateLastNotificationId(Uid(3, 3)) }(ctx)
      p2.future.onSuccess { case _ => service.updateLastNotificationId(Uid(4, 4)) }(ctx)
      p1.trySuccess(())
      p2.trySuccess(())
      waitForLastNotIdChangeTo(Some(Uid(4, 4)))
    }

    scenario("store last notification Id on new event") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None)))
      )

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(userPrefs.preference(LastStableNotification).signal.filter(_.isDefined).head)

      val id = Uid()
      service.onPushNotification(PushNotification(id, Seq(MemberJoinEvent(RConvId(), new Date, UserId(), Nil))))
      waitForLastNotIdChangeTo(Some(id))
    }

    scenario("don't store id on transient notification") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None)))
      )

      wsConnected ! true
      result(userPrefs.preference(LastStableNotification).signal.filter(_.isDefined).head)

      val newId = Uid()
      service.onPushNotification(PushNotification(newId, Nil, transient = true))
      waitForLastNotIdChangeTo(Some(lastId))
    }

    scenario("don't update id on otr notification not intended for us") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None) ))
      )

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(lastId))

      service.onPushNotification(PushNotification(Uid(), Seq(OtrMessageEvent(RConvId(), new Date, UserId(), ClientId(), ClientId(), Array.empty))))

      awaitAllTasks
      result(service.lastNotificationId) shouldEqual Some(lastId)
    }
  }

  feature("/notifications") {

    scenario("fetch last notification when there is no local last notification id") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None) ))
      )

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(lastId))
      syncPerformed shouldEqual 0
    }

    scenario("fetch notifications and update last id") {
      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )
      service.updateLastNotificationId(lastId)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      waitForLastNotIdChangeTo(Some(notification2.id))
      syncPerformed shouldEqual 0
    }

    scenario("request slow sync and fetch last notification if /notifications returns error") {
      AndroidFreeSpec.clock.reset()
      AndroidFreeSpec.clock.advance(10 minutes)
      val time = AndroidFreeSpec.clock.instant()
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse(Response.Status.NotFound, "", "")))
      )

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(PushNotification(Uid(), Nil), PushNotification(Uid(), Nil)), hasMore = false, None) ))
      )

      service.updateLastNotificationId(lastId)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      waitForLastNotIdChangeTo(Some(lastId))
      result(service.onHistoryLost.filter(_ == time).head)
      syncPerformed shouldEqual 1
      // TODO: this test reports an NPE to Hockey; probably one of the futures finishes after the test has finished
    }

  }

  feature("network changes") {
    scenario("schedule a delayed retry after a failed load") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )

      val backoff = mock[Backoff]
      var backoffDelayCalled = 0
      (backoff.delay _).expects(*, *).anyNumberOfTimes().onCall { _ =>
        backoffDelayCalled += 1
        1.seconds
      }

      PushService.setBackoff(backoff)
      service.updateLastNotificationId(lastId)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(notification2.id))

      backoffDelayCalled shouldEqual 1
    }

    scenario("sync history on network change after a failed load") {
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )

      val backoff = mock[Backoff]
      (backoff.delay _).expects(*, *).anyNumberOfTimes().returning(1.days) // long enough so we don't have to worry about it
      PushService.setBackoff(backoff)

      service.updateLastNotificationId(lastId)
      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(service.lastNotificationId) should equal(Some(lastId))

      networkMode ! NetworkMode.WIFI // switching to wifi should trigger reloading
      result(networkMode.filter(_ == NetworkMode.WIFI).head)

      waitForLastNotIdChangeTo(Some(notification2.id))
    }

    scenario("don't sync history on network change without a failed load") {

      val notification1 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1), hasMore = false, None) ))
      )

      val backoff = mock[Backoff]
      (backoff.delay _).expects(*, *).anyNumberOfTimes().returning(1.days) // long enough so we don't have to worry about it
      PushService.setBackoff(backoff)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(notification1.id))

      networkMode ! NetworkMode.WIFI // switching to wifi should NOT trigger reloading
      result(networkMode.filter(_ == NetworkMode.WIFI).head)

      result(service.lastNotificationId) shouldEqual Some(notification1.id)
    }
  }

  feature("web socket notifications") {
    val ws = new WebSocketClient(context, AccountId(), mock[AsyncClient], Uri.parse(""), mock[AccessTokenProvider]) {
      override lazy val wakeLock: WakeLock = new FakeWakeLock

      override def connect(): CancellableFuture[WebSocket] = CancellableFuture.successful(mock[WebSocket])
    }

    wsClient ! Some(ws)

    val wsNotification1 =
      """
        | {
        |         "id":"fbe54fb4-463e-4746-9861-c28c2961bdd0",
        |         "payload":[
        |            {
        |               "conversation":"3b45e65a-8bf2-447b-bd8a-03c207deae3f",
        |               "data":{
        |                  "content":"Test message 2",
        |                  "nonce":"47745f9f0-0dab-113c-43ad7ee9-394c562"
        |               },
        |               "from":"13962457-c316-4de1-9962-929c40f8cff4",
        |               "id":"f.80011231430865a7",
        |               "time":"2014-04-14T09:56:00.185Z",
        |               "type":"conversation.message-add"
        |            }
        |         ]
        |      }
      """.stripMargin

    scenario("receive notifications first with a fetch, then with a push") {

      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(wsNotification1)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1), hasMore = false, None)))
      )

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(notification1.id))

      ws.onMessage ! JsonObjectResponse(wsJson1)

      waitForLastNotIdChangeTo(Some(wsNot1.id))

      notificationsLog.size shouldEqual 2
      notificationsLog(0) shouldEqual notification1.id
      notificationsLog(1) shouldEqual wsNot1.id
    }

    scenario("receive a push notification during a fetch") {
      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(wsNotification1)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      (client.loadNotifications _).expects(*, *).once().onCall { _ =>
        Future {
          ws.onMessage ! JsonObjectResponse(wsJson1)
        }
        Thread.sleep(100L)

        CancellableFuture.successful {
          Right(LoadNotificationsResponse(Vector(notification1), hasMore = false, None))
        }
      }

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      waitForLastNotIdChangeTo(Some(notification1.id))

      waitForLastNotIdChangeTo(Some(wsNot1.id))

      notificationsLog.size shouldEqual 2
      notificationsLog(0) shouldEqual notification1.id
      notificationsLog(1) shouldEqual wsNot1.id // processed as second even though received first
    }

    scenario("ignore a push notification while waiting for a network change") {
      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(wsNotification1)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      service.updateLastNotificationId(lastId)
      waitForLastNotIdChangeTo(Some(lastId))

      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      // this should be triggered by a network change and it still should use lastId, not wsNot1.id
      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1, wsNot1), hasMore = false, None)))
      )

      (client.loadNotifications _).expects(Some(wsNot1.id), *).never()

      val backoff = mock[Backoff]
      (backoff.delay _).expects(*, *).anyNumberOfTimes().returning(1.days) // long enough so we don't have to worry about it
      PushService.setBackoff(backoff)

      println("sync history")

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      result(service.lastNotificationId) should equal(Some(lastId))

      println("initial syncHistory done (and failed)")
      println("sending a push notification")

      ws.onMessage ! JsonObjectResponse(wsJson1)

      println("changing the network mode")

      networkMode ! NetworkMode.WIFI // switching to wifi should trigger reloading, but the sinceId should stay the same (lastId)

      println("should trigger fetch retry")
      result(networkMode.filter(_ == NetworkMode.WIFI).head)

      println("should set the new lastId")

      waitForLastNotIdChangeTo(Some(wsNot1.id))

      println("done")

      notificationsLog.size shouldEqual 2
      notificationsLog(0) shouldEqual notification1.id
      notificationsLog(1) shouldEqual wsNot1.id // received only once
    }

    scenario("ignore a push notification while waiting for a delayed retry") {
      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(wsNotification1)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      service.updateLastNotificationId(lastId)
      waitForLastNotIdChangeTo(Some(lastId))

      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      // this should be triggered by a network change and it still should use lastId, not wsNot1.id
      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1, wsNot1), hasMore = false, None)))
      )

      (client.loadNotifications _).expects(Some(wsNot1.id), *).never()

      val backoff = mock[Backoff]
      (backoff.delay _).expects(*, *).anyNumberOfTimes().returning(2.seconds)
      PushService.setBackoff(backoff)

      println("sync history")

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      result(service.lastNotificationId) should equal(Some(lastId))

      println("initial syncHistory done (and failed)")
      println("sending a push notification")

      ws.onMessage ! JsonObjectResponse(wsJson1)

      result(service.lastNotificationId) should equal(Some(lastId))

      waitForLastNotIdChangeTo(Some(wsNot1.id))

      println("done")

      notificationsLog.size shouldEqual 2
      notificationsLog(0) shouldEqual notification1.id
      notificationsLog(1) shouldEqual wsNot1.id // received only once
    }
  }

}
