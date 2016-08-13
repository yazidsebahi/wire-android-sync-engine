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
import com.waz.model._
import com.waz.service.messages.MessagesService
import com.waz.service.push.PushService
import com.waz.service.{EventPipeline, EventScheduler, UserService}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.SerialDispatchQueue
import com.waz.utils._

import scala.concurrent.Future

class ConversationEventsService(push: PushService, convs: ConversationsContentUpdater, messages: MessagesService, users: UserService, sync: SyncServiceHandle, pipeline: EventPipeline) {

  private implicit val tag: LogTag = logTagFor[ConversationEventsService]
  private implicit val dispatcher = new SerialDispatchQueue(name = "ConversationEventsDispatcher")

  val conversationEventsStage = EventScheduler.Stage[ConversationOrderEvent](processPushedConversationEvents)

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

  private def processPushedConversationEvents(convId: RConvId, es: Seq[ConversationOrderEvent]) =
    if (es.isEmpty) Future.successful(())
    else convs.processConvWithRemoteId(convId, retryAsync = true) { conv =>
      users.withSelfUserFuture { selfUserId =>
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
}
