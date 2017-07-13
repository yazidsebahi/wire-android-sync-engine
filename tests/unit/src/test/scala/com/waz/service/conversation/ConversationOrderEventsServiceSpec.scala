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

import com.waz.model._
import com.waz.service.{EventPipeline, EventScheduler}
import com.waz.service.EventScheduler.{Interleaved, Parallel, Sequential, Stage}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.SerialDispatchQueue
import org.threeten.bp.Instant

import scala.concurrent.Future

class ConversationOrderEventsServiceSpec extends AndroidFreeSpec {

  //  scenario("Ignore like events") {
  //    service.filterConvOrderEvents(Seq(GenericMessageEvent(RConvId(), new Date, UserId(), GenericMessage(Uid(), Reaction(MessageId(), Liking.Action.Like))))) shouldBe empty
  //  }


  scenario("All batched conversation events go to the order event service before any other conv-related service") {

    implicit val outputDispatcher = new SerialDispatchQueue(name = "OutputWriter")

    val output = new StringBuffer()

    var convOrderProcessedCount = 0
    val convOrderStage = EventScheduler.Stage[ConversationOrderEvent] { (convId, es) =>
      Future {
        output.append(s"A$convOrderProcessedCount")
        convOrderProcessedCount += 1
      }
    }

    var convStateProcessedCount = 0
    val convStateStage = EventScheduler.Stage[ConversationStateEvent] { (convId, es) =>
      Future {
        output.append(s"B$convStateProcessedCount")
        convStateProcessedCount += 1
      }
    }

    val messagesEventStage = EventScheduler.Stage[MessageEvent] { (convId, events) => Future.successful({}) }

    val genericMessageEventStage = EventScheduler.Stage[GenericMessageEvent] { (convId, events) => Future.successful({}) }


    val scheduler = new EventScheduler(
      Stage(Sequential)(
        convOrderStage,
        Stage(Parallel)(
          convStateStage,
          Stage(Interleaved)(
            messagesEventStage,
            genericMessageEventStage
          )
        )
      )
    )

    val pipeline = new EventPipeline(Vector.empty, scheduler.enqueue)

    val convId = RConvId()
    val events = (1 to 10).map { _ =>
      RenameConversationEvent(convId, new Date(Instant.now.getEpochSecond), UserId(), "blah")
    }

    result(pipeline.apply(events).map(_ => println(output.toString)))
  }

}
