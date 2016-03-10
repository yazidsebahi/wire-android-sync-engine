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
package com.waz.service

import com.waz._
import com.waz.model._
import com.waz.sync.SyncServiceHandle
import com.waz.testutils.Matchers._
import com.waz.testutils.{EmptySyncService, MockZMessaging}
import com.waz.threading.CancellableFuture
import com.waz.utils.events.EventStream
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class VersionBlacklistSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit val timeout: FiniteDuration = 1.second
  import com.waz.utils.events.EventContext.Implicits.global

  var blacklist = VersionBlacklist()

  var zms: MockZMessaging = _
  def service = zms.blacklist

  before {
    zms = new MockZMessaging() {
      override lazy val sync: SyncServiceHandle = new EmptySyncService {
        override def syncVersionBlacklist() = {
          service.updateBlacklist(test.blacklist)
          CancellableFuture.successful(SyncId())
        }
      }
    }
    resetLastSyncTime()
  }

  feature("Evaluate blacklist") {
    scenario("If the current app version is blacklisted, we're not up-to-date") {
      blacklist = VersionBlacklist(0, Seq(0, 1, 2, zms.metadata.appVersion, Integer.MAX_VALUE))
      initializeService()
      isUpToDate should eventually(be(false))
    }

    scenario("If the current app version is older than the oldest supported one, we're not up-to-date") {
      blacklist = VersionBlacklist(zms.metadata.appVersion + 1, Seq())
      initializeService()
      isUpToDate should eventually(be(false))
    }

    scenario("If the current app version is exactly the oldest supported one, we're up-to-date") {
      blacklist = VersionBlacklist(zms.metadata.appVersion, Seq())
      initializeService()
      isUpToDate should eventually(be(true))
    }

    scenario("If recently synced, return the up-to-date state from the preferences") {
      resetLastSyncTime(System.currentTimeMillis)
      Await.ready(zms.prefs.editPreferences { _.putBoolean(VersionBlacklistService.UpToDatePref, false) }, 1.second)
      blacklist = VersionBlacklist(zms.metadata.appVersion, Seq())
      initializeService()
      isUpToDate should eventually(be(false))
    }
  }

  def initializeService(): Unit =
    withDelay {
      service.upToDate.currentValue shouldBe 'defined
    }

  def isUpToDate: CancellableFuture[Boolean] = service.upToDate.currentValue.fold(EventStream.wrap(service.upToDate).next)(CancellableFuture.successful)

  def resetLastSyncTime(to: Long = 0L): Unit = Await.ready(zms.prefs.editPreferences { _.putLong(VersionBlacklistService.LastUpToDateSyncPref, to) }, 1.second)
}
