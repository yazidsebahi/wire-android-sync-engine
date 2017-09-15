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
import com.waz.utils.{FakeWakeLock, WakeLock}
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.Context
import com.waz.znet.{Response, ZNetClient}

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.language.postfixOps

class PushServiceSpec extends AndroidFreeSpec { test =>

  val wsConnected = Signal(false)

  val context = mock[Context]
  val pipeline = mock[EventPipeline]
  val webSocket = mock[WebSocketClientService]
  val znet = mock[ZNetClient]
  val sync = mock[SyncServiceHandle]
  val userPrefs = new TestUserPreferences
  val network = mock[NetworkModeService]

  val lastId = Uid()

  implicit val ctx = Threading.Background
  val client = mock[PushNotificationsClient]

  (webSocket.connected _).expects().anyNumberOfTimes().returning(wsConnected)
  (webSocket.client _).expects().anyNumberOfTimes().returning(Signal.const(None))
  (pipeline.apply _).expects(*).anyNumberOfTimes().returning(Future.successful({}))
  var syncPerformed = 0
  (sync.performFullSync _).expects().anyNumberOfTimes().onCall { _ =>
    Future.successful { syncPerformed += 1 }
  }

  val networkMode = Signal(NetworkMode._4G)
  (network.networkMode _).expects().anyNumberOfTimes.returning(networkMode)

  val service = new PushServiceImpl(context, userPrefs, client, ClientId(), pipeline, webSocket, sync, network) {
    override lazy val wakeLock: WakeLock = new FakeWakeLock
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
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None) ))
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
        CancellableFuture.successful(Right(LoadNotificationsResponse(Vector(PushNotification(lastId, Vector.empty)), hasMore = false, None) ))
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

      result(userPrefs.preference(LastStableNotification).signal.filter(_.isDefined).head)

      service.onPushNotification(PushNotification(Uid(), Seq(OtrMessageEvent(RConvId(), new Date, UserId(), ClientId(), ClientId(), Array.empty))))
      waitForLastNotIdChangeTo(Some(lastId))
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
    }

  }

  /*
   TODO:
   1. The last test ("request slow sync and fetch last notification if /notifications returns error") leaves an unfinished future which fails with NPE
   2. Test syncHistory on network changes
   3. Test if new web socket notifications during a fetch are scheduled after a fetch
   4. Test if new web socket notifications during a fetch are scheduled after a fetch and cancelled when the fetch fails
    */

}
