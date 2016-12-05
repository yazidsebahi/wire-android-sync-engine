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

import com.waz.content.KeyValueStorage
import com.waz.service.push.PushTrackingService.NotificationsEvent
import com.waz.service.{LifecycleState, PreferenceService, ZmsLifecycle}
import com.waz.threading.Threading
import com.waz.utils.RichFiniteDuration
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.duration._

class PushTrackingService(keyValueStorage: KeyValueStorage,
                          gcmService: GcmService,
                          webSocketClientService: WebSocketClientService,
                          lifecycle: ZmsLifecycle,
                          preferenceService: PreferenceService) {

  import com.waz.utils.events.EventContext.Implicits.global
  import keyValueStorage._

  private implicit val ec = Threading.Background

  /**
    * GCM tracking:
    *
    * The aim here is to track how many notifications we get from GCM successfully compared to how many times
    * we perform a fetch and realise the we missed something. Furthermore, it would also be good to know just
    * how often these failures chain together, triggering a deletion and re-registration of the GCM token and
    * a fallback to the WebSocket connection.
    */

  val successfulGcmNotifs = keyValuePref("successfulGcmNotifications", 0)
  val failedGcmNotifs     = keyValuePref("failedGcmNotifications", 0)
  //actual registration retries (after threshold)
  val registrationRetries = keyValuePref("registrationRetries", 0)

  gcmService.notificationsToProcess.onChanged.filter(_.nonEmpty) { ns => successfulGcmNotifs.mutate(_ + 1) }

  gcmService.registrationRetryCount.signal.onChanged.filter(_ > 0) { retryCount =>
    if (retryCount > GcmService.retryFailLimit)
      registrationRetries.mutate(_ + 1)
    failedGcmNotifs.mutate(_ + 1)
  }

  /**
    * WebSocket tracking:
    *
    * Here we want to track how many pings are occurring, the interval in which they occur, and how
    * many pings to the BE failed while the app is in the background. This should give us an idea of
    * how reliable the connection is.
    */

  val totalPings    = keyValuePref("totalPings", 0)
  val receivedPongs = keyValuePref("receivedPongs", 0)
  def pingInterval = preferenceService.webSocketPingInterval

  val inBackground = lifecycle.lifecycleState.map { st => st != LifecycleState.Active && st != LifecycleState.UiActive }

  webSocketClientService.client.collect { case Some(c) => c }.zip(inBackground).filter { case (_, st) => st } { case (client, _) =>
    client.onPing(_ => totalPings.mutate(_ + 1))
    client.onPong(_ => receivedPongs.mutate(_ + 1))
  }

  val lastEvent = keyValuePref("lastPushTrackingEvent", Instant.now)

  val shouldSendEvent = inBackground.flatMap {
    case true => Signal const false
    case _ => lastEvent.signal.map(time => 1.day.elapsedSince(time))
  }

  def buildEvent() = {
    for {
      sGcm <- successfulGcmNotifs()
      fGcm <- failedGcmNotifs()
      rR <- registrationRetries()
      tp <- totalPings()
      rP <- receivedPongs()
    } yield {
      NotificationsEvent(Instant.now, sGcm, fGcm, rR, tp, rP, pingInterval)
    }
  }

  def reset() = {
    successfulGcmNotifs := 0
    failedGcmNotifs := 0
    registrationRetries := 0
    totalPings := 0
    receivedPongs := 0
    lastEvent := Instant.now()
  }

}

object PushTrackingService {

  case class NotificationsEvent(time: Instant,
                                successfulGcmNotifs: Int,
                                failedGcmNotifs: Int,
                                registrationRetries: Int,
                                totalPings: Int,
                                receivedPongs: Int,
                                pingInterval: FiniteDuration
                               )

}
