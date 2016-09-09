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
package com.waz.testutils

import android.app.Application
import com.waz.api.NotificationsHandler.NotificationsHandlerFactory
import com.waz.api._
import com.waz.model.VoiceChannelData
import com.waz.service.ZMessaging
import com.waz.service.push.NotificationService.NotificationInfo

class TestApplication extends Application with NotificationsHandlerFactory {

  import TestApplication._

  override def getCallingEventsHandler: CallingEventsHandler = callingEventsHandler
  override def getTrackingEventsHandler: TrackingEventsHandler = ZMessaging.EmptyTrackingEventsHandler
}

object TestApplication {

  val callingEventsSpy = new CallingEventsSpy(Nil)
  val notificationsSpy = new NotificationsSpy(Seq.empty, None, None, true)

  private val callingEventsHandler: CallingEventsHandler = callingEventsSpy
}

class CallingEventsSpy(var events: List[CallingEvent]) extends CallingEventsHandler {
  // gets all its updates on the UI thread
  override def onCallingEvent(event: CallingEvent): Unit = events = event :: events

  def latestEvent: Option[CallingEvent] = events.headOption
}

class NotificationsSpy(
  @volatile var gcms: Seq[Seq[NotificationInfo]],
  @volatile var ongoingCall: Option[VoiceChannelData],
  @volatile var incomingCall: Option[VoiceChannelData],
  @volatile var uiActive: Boolean)
