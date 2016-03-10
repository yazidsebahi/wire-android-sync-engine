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
package com.waz.api.impl

import com.waz.ZLog._
import com.waz.api
import com.waz.service.tracking.TrackingStats
import com.waz.ui.{SignalLoading, UiModule}

class TrackingData(implicit ui: UiModule) extends api.TrackingData with UiObservable with SignalLoading {
  private implicit val logTag: LogTag = logTagFor[TrackingData]

  private var maybeData = Option.empty[TrackingStats]
  private def data = maybeData getOrElse TrackingStats.Empty

  addLoader(_.tracking.trackingSignal, TrackingStats.Empty) { loaded =>
    if (maybeData.forall(_ != loaded)) {
      this.maybeData = Some(loaded)
      notifyChanged()
    }
  }

  def getGroupConversationCount: Int = data.groups
  def getArchivedConversationCount: Int = data.archived
  def getMutedConversationCount: Int = data.muted

  def getNotBlockedContactCount: Int = data.contacts
  def getBlockedContactCount: Int = data.blocked
  def getAutoConnectedContactCount: Int = data.autoConnected

  def getVoiceCallCount: Int = data.voiceCalls
  def getVideoCallCount: Int = data.videoCalls

  def getSentTextMessageCount: Int = data.textsSent
  def getSentImagesCount: Int = data.imagesSent
  def hasInteractedWithBot: Boolean = data.botInteractions > 0

  def isInitialized: Boolean = maybeData.nonEmpty
}
