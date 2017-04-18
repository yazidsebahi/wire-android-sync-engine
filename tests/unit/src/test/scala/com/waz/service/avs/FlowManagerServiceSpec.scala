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
package com.waz.service.avs

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.{AvsLogLevel, NetworkMode}
import com.waz.call.FlowManager
import com.waz.model.UserId
import com.waz.service.call.DefaultFlowManagerService
import com.waz.service.call.DefaultFlowManagerService.AvsLogData
import com.waz.service.push.WebSocketClientService
import com.waz.testutils.MockZMessaging
import com.waz.utils.events.{EventContext, Signal}
import org.scalatest._

import scala.concurrent.duration._

class FlowManagerServiceSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit val timeout: FiniteDuration = 2.seconds
  implicit val ev = EventContext.Global

  val selfUserId = UserId()
  val wsConnected = Signal(false)
  
  @volatile var notified = false

  lazy val zms = new MockZMessaging() {
    override lazy val flowmanager: DefaultFlowManagerService = new DefaultFlowManagerService(context, zNetClient, websocket, prefs, network) {
      override lazy val flowManager = Some(new FlowManager(context, requestHandler) {
        override def networkChanged(): Unit = notified = true
      })
    }

    override lazy val websocket: WebSocketClientService = new WebSocketClientService(context, lifecycle, zNetClient, network, global.backend, clientId, timeouts, gcm) {
      override val connected = wsConnected
    }
  }
  lazy val service = zms.flowmanager

  implicit def db: SQLiteDatabase = zms.db.dbHelper.getWritableDatabase

  @volatile var logData = Option.empty[AvsLogData]

  before { notified = false }

  feature("AVS logging and metrics") {
    scenario("Wire signal") {
      service.avsLogDataSignal { logData =>
        this.logData = Some(logData)
      }
    }

    scenario("Enable/disable logging") {
      withDelay { logData.value.loggingEnabled shouldEqual false }
      service.setLoggingEnabled(true)
      withDelay { logData.value.loggingEnabled shouldEqual true }
      service.setLoggingEnabled(false)
      withDelay { logData.value.loggingEnabled shouldEqual false }
    }

    scenario("Enable/disable metrics") {
      withDelay { logData.value.metricsEnabled shouldEqual false }
      zms.prefs.analyticsEnabledPref := true
      withDelay { logData.value.metricsEnabled shouldEqual true }
      zms.prefs.analyticsEnabledPref := false
      withDelay { logData.value.metricsEnabled shouldEqual false }
    }

    scenario("Set log level") {
      withDelay { logData.value.logLevel shouldEqual AvsLogLevel.DEBUG }
      service.setLogLevel(AvsLogLevel.ERROR)
      withDelay { logData.value.logLevel shouldEqual AvsLogLevel.ERROR }
      service.setLogLevel(AvsLogLevel.INFO)
      withDelay { logData.value.logLevel shouldEqual AvsLogLevel.INFO }
    }
  }

  feature("Notifying AVS about network changes") {
    val delay = 5.millis

    scenario("Initialize network and websocket") {
      zms.network.networkMode ! NetworkMode.WIFI
      wsConnected ! true
      withDelay { notified shouldEqual false }
    }

    scenario("Switch to 4g") {
      wsConnected ! false

      awaitUi(delay)
      zms.network.networkMode ! NetworkMode._4G
      withDelay { notified shouldEqual false }

      awaitUi(delay)
      wsConnected ! true
      withDelay { notified shouldEqual true }
    }

    scenario("Switch back to Wifi, order reversed") {
      awaitUi(delay)
      zms.network.networkMode ! NetworkMode.WIFI
      withDelay { notified shouldEqual false }

      awaitUi(delay)
      wsConnected ! false
      withDelay { notified shouldEqual false }

      awaitUi(delay)
      wsConnected ! true
      withDelay { notified shouldEqual true }
    }

    scenario("reconnect websocket without network change") {
      awaitUi(delay)
      wsConnected ! false
      withDelay { notified shouldEqual false }

      awaitUi(delay)
      wsConnected ! true
      withDelay { notified shouldEqual false }
    }
  }
}
