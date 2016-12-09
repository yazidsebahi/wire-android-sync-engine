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

import java.util.concurrent.{CountDownLatch, TimeUnit}

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.model._
import com.waz.service.LifecycleState._
import com.waz.service.push.NotificationService.NotificationInfo
import com.waz.service.push.PushTrackingService.NotificationsEvent
import com.waz.testutils.{DefaultPatienceConfig, MockZMessaging}
import com.waz.utils.RichInstant
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.threeten.bp.Instant
import com.waz.testutils.Matchers._
import com.waz.testutils._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

class PushTrackingServiceSpec extends FeatureSpec with Matchers with PropertyChecks with BeforeAndAfter with BeforeAndAfterAll with RobolectricTests with RobolectricUtils with DefaultPatienceConfig { test =>

  import com.waz.utils.events.EventContext.Implicits.global

//  ShadowLog.stream = System.out

  @volatile var currentNotifications = Nil: Seq[NotificationInfo]

  lazy val selfUserId = UserId()

  lazy val zms = new MockZMessaging(selfUserId = selfUserId)
  implicit def db: SQLiteDatabase = zms.db.dbHelper.getWritableDatabase

  lazy val service = zms.pushTracking
  lazy val lifeCycle = zms.lifecycle.lifecycleState


    scenario("Daily pushing of push tracking information") {

      val building = Promise[NotificationsEvent]()

      service.shouldSendEvent { v =>
        println(s"Should send event?: $v")
        if (v) building.completeWith(service.buildEvent())
      }

      //ensure last event time is more than a day ago
      service.lastEvent := Instant.now - (1.day + 1.hour)

      //put some push information in
      (1 to 3).foreach { _ =>
        //need to be pushed individually for service to register them as individual events
        zms.gcm.notificationsToProcess ! Set(Uid())
        Thread.sleep(100)
      }

      (1 to 3).foreach { _ =>
        //need to be pushed individually for service to register them as individual events
        zms.gcm.registrationRetryCount.mutate(_ + 1)
        Thread.sleep(100)
      }

      lifeCycle ! Active

      val res = Await.result(building.future, 3.seconds)
      res should beMatching {
        case NotificationsEvent(_, 3, 3, 1, _, _, _) => true
      }

      //test reset values
      Await.ready(service.reset(), 3.seconds)
      Await.result(service.successfulGcmNotifs(), 3.seconds) shouldEqual 0
      Await.result(service.failedGcmNotifs(), 3.seconds) shouldEqual 0
      Await.result(service.registrationRetries(), 3.seconds) shouldEqual 0
    }

}
