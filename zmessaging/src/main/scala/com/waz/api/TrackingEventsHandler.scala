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
package com.waz.api

import com.waz.service.HandlesTrackingService.HandlesValidationTrackingEvent
import com.waz.service.call.AvsMetrics
import com.waz.service.push.PushTrackingService.NotificationsEvent
import org.threeten.bp.Duration

trait TrackingEventsHandler {
  def onNotificationsEvent(ev: NotificationsEvent): Unit
  def onTrackingEvent(event: TrackingEvent): Unit
  def onAvsMetricsEvent(avsMetrics: AvsMetrics): Unit
  def onHandleValidation(event: HandlesValidationTrackingEvent): Unit
}

trait TrackingEvent {
  def getKind: KindOfTrackingEvent
  def getAssetSizeInBytes: Opt[java.lang.Long]
  def getAssetMimeType: Opt[String]
  def getDuration: Opt[Duration]
  def getConversationType: Opt[IConversation.Type]
  def isInConversationWithOtto: Opt[Boolean]
  def getErrorResponse: Opt[ErrorResponse]
  def getEphemeralExpiration: EphemeralExpiration
}
