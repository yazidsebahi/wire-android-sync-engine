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
package com.waz.service.tracking

import com.waz.model.AccountId
import com.waz.service.ZMessaging
import com.waz.service.tracking.TrackingService.NoReporting
import com.waz.specs.AndroidFreeSpec

import scala.concurrent.{Future, Promise, TimeoutException}

class TrackingServiceSpec extends AndroidFreeSpec {

  val stubZmsProvider = new TrackingService.ZmsProvider {
    override def apply(accountId: AccountId): Future[Option[ZMessaging]] = Future.successful(None)
    override def current: Future[Option[ZMessaging]] = Future.successful(None)
  }

  val tracking = new TrackingServiceImpl(stubZmsProvider)

  feature("registering event") {
    scenario("contribution event") {
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) if event.name == "contributed" && event.props.nonEmpty =>
          if (event.props.get.getString("action") == ContributionEvent.Action.Text.name)
            p.completeWith(Future.successful({}))
      }

      tracking.contribution(ContributionEvent.Action.Text)

      result(p.future)
    }

    scenario("logged out event") {
      val reason = "no reason"
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) if event.name == "account.logged_out" && event.props.nonEmpty =>
          if (event.props.get.getString("reason") == reason) p.completeWith(Future.successful({}))
      }

      tracking.loggedOut(reason, AccountId())

      result(p.future)
    }

    scenario("opt in event") {
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) if event.name == "settings.opted_in_tracking" && event.props.isEmpty =>
          p.completeWith(Future.successful({}))
      }

      tracking.optIn()

      result(p.future)
    }

    scenario("opt out event") {
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) if event.name == "settings.opted_out_tracking" && event.props.isEmpty =>
          p.completeWith(Future.successful({}))
      }

      tracking.optOut()

      result(p.future)
    }

    scenario("exception event") {
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) if event.name == "debug.exception" && event.props.nonEmpty =>
          if (event.props.get.getString("exceptionType") == "IllegalArgumentException" &&
            event.props.get.getString("description") == "bar")
          p.completeWith(Future.successful({}))
      }

      tracking.exception(new IllegalArgumentException("foo"), "bar")

      result(p.future)
    }

    scenario("exception event with NoReporting") {
      val p = Promise[Unit]()

      tracking.events {
        case (_, event) => p.completeWith(Future.successful({}))
      }

      tracking.exception(new IllegalArgumentException("foo") with NoReporting, "bar")

      intercept[TimeoutException]{
        result(p.future)
      }
    }
  }


}
