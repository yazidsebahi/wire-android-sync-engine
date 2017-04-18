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

import com.waz.ZLog._
import com.waz.model.GenericContent.{MsgDeleted, MsgEdit, MsgRecall, Reaction, Receipt}
import com.waz.model._
import com.waz.service.messages.MessagesService
import com.waz.service.{EventPipeline, EventScheduler, UserService}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._

import scala.concurrent.Future

class ConversationEventsService(convs: DefaultConversationsContentUpdater, messages: MessagesService, users: UserService, sync: SyncServiceHandle, pipeline: EventPipeline) {

  private implicit val tag: LogTag = logTagFor[ConversationEventsService]
  private implicit val dispatcher = new SerialDispatchQueue(name = "ConversationEventsDispatcher")

  val selfUserId = users.selfUserId
  val conversationEventsStage = EventScheduler.Stage[ConversationOrderEvent] { (convId, es) =>
    processPushedConversationEvents(convId, filterConvOrderEvents(es))
  }

  def handlePostConversationEvent(event: ConversationEvent) = {
    debug(s"handlePostConversationEvent($event)")
    Future.sequence(Seq(
      event match {
        case ev: MessageEvent => pipeline(Seq(ev.withCurrentLocalTime())) // local time is required for the hot knock mechanism
        case _ => Future.successful(())
      },

      convs.convByRemoteId(event.convId) flatMap {
        case Some(conv) =>
          convs.updateConversationLastRead(conv.id, event.time.instant) map { _ => Future.successful(()) }
        case _ => Future.successful(())
      }
    )) map { _ => () }
  }

  // some generic message events should not update conversation order, will filter them out here
  // TODO: we should reconsider this implementation completely, in future there are going to be more content types that should not affect ordering
  // we may want to decouple lastEventTime and conv ordering, order should be based on last visible change / last message, events are no longer relevant
  private[service] def filterConvOrderEvents(events: Seq[ConversationOrderEvent]) =
    events filter {
      case GenericMessageEvent(_, _, _, GenericMessage(_, _ : MsgEdit | _ : MsgDeleted | _: MsgRecall | _: Receipt | _: Reaction)) => false
      case _ => true
    }

  private def processPushedConversationEvents(convId: RConvId, es: Seq[ConversationOrderEvent]) =
    if (es.isEmpty) Future.successful(())
    else convs.processConvWithRemoteId(convId, retryAsync = true) { conv =>
      verbose(s"updateLastEvent($conv, $es)")
      val lastTime = es.maxBy(_.time).time
      val fromSelf = es.filter(_.from == selfUserId)
      val lastRead = if (fromSelf.isEmpty) None else Some(fromSelf.maxBy(_.time).time.instant)

      for {
        _ <- convs.updateLastEvent(conv.id, lastTime.instant)
        _ <- lastRead match {
          case None => Future successful None
          case Some(time) => convs.updateConversationLastRead(conv.id, time)
        }
      } yield ()
    }
}
