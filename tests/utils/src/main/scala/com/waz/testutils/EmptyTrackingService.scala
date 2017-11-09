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

import com.waz.model.AccountId
import com.waz.service.ZMessaging
import com.waz.service.tracking.{ContributionEvent, TrackingEvent, TrackingService}
import com.waz.utils.events.EventStream

class EmptyTrackingService extends TrackingService {
  override def events: EventStream[(Option[ZMessaging], TrackingEvent)] = EventStream[(Option[ZMessaging], TrackingEvent)]()

  override def track(event: TrackingEvent): Unit = {}
  override def track(event: TrackingEvent, accountId: AccountId): Unit = {}
  override def loggedOut(reason: String, accountId: AccountId): Unit = {}
  override def optIn(): Unit = {}
  override def optOut(): Unit = {}
  override def contribution(action: ContributionEvent.Action): Unit = {}
  override def exception(e: Throwable, description: String): Unit = {}
  override def exception(e: Throwable, description: String, accountId: AccountId): Unit = {}
  override def crash(e: Throwable): Unit = {}
}
