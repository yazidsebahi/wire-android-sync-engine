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
package com.waz.service.conversation

import java.util.Date

import com.waz.RobolectricUtils
import com.waz.model.GenericContent.Reaction
import com.waz.model._
import com.waz.testutils.MockZMessaging
import org.scalatest._

class ConversationEventsServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with OptionValues with RobolectricTests with RobolectricUtils { test =>

  lazy val zms = new MockZMessaging()
  lazy val service = zms.convEvents

  scenario("Ignore like events") {
    service.filterConvOrderEvents(Seq(GenericMessageEvent(RConvId(), new Date, UserId(), GenericMessage(Uid(), Reaction(MessageId(), Liking.Action.Like))))) shouldBe empty
  }
}
