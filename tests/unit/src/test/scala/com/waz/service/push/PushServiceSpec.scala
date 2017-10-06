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
import com.waz.testutils.{TestBackoff, TestGlobalPreferences, TestUserPreferences}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet._
import com.waz.utils._
import org.json.JSONObject

import scala.concurrent.Future
import scala.concurrent.duration._

class PushServiceSpec extends AndroidFreeSpec { test =>

  val wsConnected = Signal(false)

  val account = AccountId()
  val context = mock[Context]
  val pipeline = mock[EventPipeline]
  val wscService = mock[WebSocketClientService]
  val znet = mock[ZNetClient]
  val sync = mock[SyncServiceHandle]
  val prefs     = new TestGlobalPreferences
  val userPrefs = new TestUserPreferences
  val network = mock[NetworkModeService]

  implicit val ctx = Threading.Background
  val client = mock[PushNotificationsClient]

  val wsClient = Signal(Option.empty[WebSocketClient])

  (wscService.connected _).expects().anyNumberOfTimes().returning(wsConnected)
  (wscService.client _).expects().anyNumberOfTimes().returning(wsClient)
  (pipeline.apply _).expects(*).anyNumberOfTimes().returning(Future.successful({}))

  val syncPerformed = Signal(0)
  (sync.performFullSync _).expects().anyNumberOfTimes().onCall { _ =>
    Future.successful { syncPerformed.mutate { _ + 1 } }
  }

  val networkMode = Signal(NetworkMode._4G)
  (network.networkMode _).expects().anyNumberOfTimes.returning(networkMode)
  (network.getNetworkOperatorName _).expects().anyNumberOfTimes().returning("Network operator")

  val cloudPush = Signal(Set.empty[Uid]).disableAutowiring()

  val service = new PushServiceImpl(context, userPrefs, prefs, client, ClientId(), account, pipeline, wscService, network, sync) {
    override lazy val wakeLock: WakeLock = new FakeLock
    override lazy val cloudPushNotificationsToProcess = cloudPush
  }

  val ws = new WebSocketClient(context, AccountId(), mock[AsyncClient], Uri.parse(""), mock[AccessTokenProvider]) {
    override lazy val wakeLock: WakeLock = new FakeLock

    override def connect(): CancellableFuture[WebSocket] = CancellableFuture.successful(mock[WebSocket])
  }

  private lazy val idPref = userPrefs.preference(LastStableNotification)

  feature("/notifications") {
    scenario("get a notification from cloud messaging push") {
      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(pushNotification), hasMore = false, None) ))
      )

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      result(wsConnected.filter(_ == false).head)

      cloudPush ! Set(pushNotification.id)

      result(idPref.signal.filter(_.contains(pushNotification.id)).head)
    }

    scenario("fetch last notification when there is no local last notification id") {
      //idPref := None
      result(idPref.signal.filter(_.isEmpty).head)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(pushNotification), hasMore = false, None) ))
      )

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(pushNotification.id)).head)
      result(syncPerformed.filter(_ == 0).head)
    }

    scenario("fetch notifications and update last id") {
      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(notification2.id)).head)
      awaitAllTasks
      result(syncPerformed.filter(_ == 0).head)
    }

    scenario("request slow sync and fetch last notification if /notifications returns error") {
      clock + 10.minutes
      val time = AndroidFreeSpec.clock.instant()
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse(Response.Status.NotFound, "", "")))
      )

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(PushNotification(Uid(), Nil), PushNotification(Uid(), Nil)), hasMore = false, None) ))
      )

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(service.onHistoryLost.filter(_ == time).head)
      result(syncPerformed.filter(_ == 1).head)
    }

  }

  feature("network changes") {
    scenario("schedule a delayed retry after a failed load") {
      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )

      val backoff = mock[Backoff]
      var backoffDelayCalled = 0
      (backoff.delay _).expects(*, *).anyNumberOfTimes().onCall { _ =>
        backoffDelayCalled += 1
        1.seconds
      }

      PushService.syncHistoryBackoff = backoff
      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(notification2.id)).head)

      backoffDelayCalled shouldEqual 1
    }

    scenario("sync history on network change after a failed load") {
      val notification1 = PushNotification(Uid(), Nil)
      val notification2 = PushNotification(Uid(), Nil)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1, notification2), hasMore = false, None) ))
      )

      PushService.syncHistoryBackoff = new TestBackoff(testDelay = 1.days) // long enough so we don't have to worry about it

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      networkMode ! NetworkMode.WIFI // switching to wifi should trigger reloading
      result(networkMode.filter(_ == NetworkMode.WIFI).head)

      result(idPref.signal.filter(_.contains(notification2.id)).head)
    }

    scenario("don't sync history on network change without a failed load") {

      val notification1 = PushNotification(Uid(), Nil)
      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right( LoadNotificationsResponse(Vector(notification1), hasMore = false, None) ))
      )

      PushService.syncHistoryBackoff = new TestBackoff(testDelay = 1.days) // long enough so we don't have to worry about it

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(notification1.id)).head)

      networkMode ! NetworkMode.WIFI // switching to wifi should NOT trigger reloading
      result(networkMode.filter(_ == NetworkMode.WIFI).head)
      awaitAllTasks

      result(idPref.signal.filter(_.contains(notification1.id)).head)
    }
  }

  feature("web socket notifications") {

    scenario("receive notifications first with a fetch, then with a push") {

      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(notJson)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      (client.loadNotifications _).expects(*, *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1), hasMore = false, None)))
      )

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(notification1.id)).head)

      ws.onMessage ! JsonObjectResponse(wsJson1)

      result(idPref.signal.filter(_.contains(wsNot1.id)).head)
    }

    scenario("receive a push notification during a fetch") {
      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(notJson)
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

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)

      result(idPref.signal.filter(_.contains(notification1.id)).head)

      result(idPref.signal.filter(_.contains(wsNot1.id)).head) // processed as second even though received first
    }

    scenario("ignore a push notification while waiting for a network change") {
      val notification1 = PushNotification(Uid(), Nil)
      val wsJson1 = new JSONObject(notJson)
      val wsNot1 = PushNotification.NotificationDecoder(wsJson1)

      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      // this should be triggered by a network change and it still should use lastId, not wsNot1.id
      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1, wsNot1), hasMore = false, None)))
      )

      (client.loadNotifications _).expects(Some(wsNot1.id), *).never()

      PushService.syncHistoryBackoff = new TestBackoff(testDelay = 1.days) // long enough so we don't have to worry about it

      println("sync history")

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      result(idPref.signal.filter(_.contains(lastId)).head)

      println("initial syncHistory done (and failed)")
      println("sending a push notification")

      ws.onMessage ! JsonObjectResponse(wsJson1)

      println("changing the network mode")

      networkMode ! NetworkMode.WIFI // switching to wifi should trigger reloading, but the sinceId should stay the same (lastId)

      println("should trigger fetch retry")
      result(networkMode.filter(_ == NetworkMode.WIFI).head)

      println("should set the new lastId")

      result(idPref.signal.filter(_.contains(wsNot1.id)).head)
    }

    scenario("ignore a push notification while waiting for a delayed retry") {
      val notification1 = PushNotification(Uid(), Nil)

      idPref := Some(lastId)
      result(idPref.signal.filter(_.contains(lastId)).head)

      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Left(ErrorResponse.InternalError))
      )

      // this should be triggered by a network change and it still should use lastId, not pushNotification.id
      (client.loadNotifications _).expects(Some(lastId), *).once().returning(
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(notification1, pushNotification), hasMore = false, None)))
      )

      (client.loadNotifications _).expects(Some(pushNotification.id), *).never()

      PushService.syncHistoryBackoff = new TestBackoff(testDelay = 2.seconds)

      println("sync history")

      wsClient ! Some(ws)
      result(wsClient.filter(_.contains(ws)).head)

      wsConnected ! true
      result(wsConnected.filter(_ == true).head)
      result(idPref.signal.filter(_.contains(lastId)).head)

      println("initial syncHistory done (and failed)")
      println("sending a push notification")

      ws.onMessage ! JsonObjectResponse(notObject)

      result(idPref.signal.filter(_.contains(pushNotification.id)).head)
    }
  }

  val lastId = Uid("last-id")
  val notJson = """
      | {
      |         "id":"fbe54fb4-463e-4746-9861-c28c2961bdd0",
      |         "payload":[]
      | }
    """.stripMargin
  val notObject = new JSONObject(notJson)
  val pushNotification = PushNotification.NotificationDecoder(notObject)
}
