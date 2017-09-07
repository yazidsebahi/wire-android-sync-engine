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
package com.waz.service.notifications

import com.waz.content._
import com.waz.model.{AccountId, UserId}
import com.waz.service.{Timeouts, ZmsLifeCycle}
import com.waz.service.push.{NotificationService, PushService}
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.TestUserPreferences

class NotificationsServiceSpec2 extends AndroidFreeSpec {


  val account   = AccountId()
  val self      = UserId()
  val messages  = mock[MessagesStorage]
  val lifeCycle = mock[ZmsLifeCycle]
  val storage   = mock[NotificationStorage]
  val users     = mock[UsersStorage]
  val convs     = mock[ConversationStorage]
  val reactions = mock[ReactionsStorage]
  val userPrefs = new TestUserPreferences
  val timeouts  = new Timeouts
  val push      = mock[PushService]


  scenario("Basic test") {

    println(await(service.notifications.head))

  }


  def service = new NotificationService(null, account, self, messages, lifeCycle, storage, users, convs, reactions, userPrefs, timeouts, push)

}
