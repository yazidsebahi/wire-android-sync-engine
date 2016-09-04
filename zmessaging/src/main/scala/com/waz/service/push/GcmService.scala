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
package com.waz.service.push

import com.waz.ZLog._
import com.waz.model._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.service.push.GcmGlobalService.GcmRegistration
import com.waz.service._
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.PushNotification
import com.waz.utils.events.EventContext

import scala.concurrent.Future
import scala.collection.breakOut

class GcmService(accountId: AccountId, gcmGlobalService: GcmGlobalService, convsContent: ConversationsContentUpdater, eventPipeline: EventPipeline, sync: SyncServiceHandle, lifecycle: ZmsLifecycle) {

  implicit val dispatcher = gcmGlobalService.dispatcher

  private implicit val tag: LogTag = logTagFor[GcmService]
  private implicit val ev = EventContext.Global

  def gcmSenderId = gcmGlobalService.gcmSenderId

  def ensureGcmRegistered(): Future[Any] =
    gcmGlobalService.getGcmRegistration.future map {
      case r @ GcmRegistration(_, userId, _) if userId == accountId => verbose(s"ensureGcmRegistered() - already registered: $r")
      case _ => sync.registerGcm()
    }

  ensureGcmRegistered()

  lifecycle.lifecycleState.map(_ == LifecycleState.Stopped) {
    case false => ensureGcmRegistered()
    case true =>
      // lifecycle got to Stopped, means that were logged out
      gcmGlobalService.getGcmRegistration map {
        case GcmRegistration(token, userId, _) if userId == accountId =>
          gcmGlobalService.clearGcmRegistrationUser(accountId)
          sync.deleteGcmToken(GcmId(token))
        case _ => // do nothing
      }
  }

  val eventProcessingStage = EventScheduler.Stage[GcmTokenRemoveEvent] { (convId, events) =>
    gcmGlobalService.getGcmRegistration.future map { reg =>
      events find (reg.token == _.token) foreach { _ =>
        gcmGlobalService.unregister() flatMap (_ => ensureGcmRegistered())
      }
    }
  }

  def register(post: GcmRegistration => Future[Boolean]): Future[Option[GcmRegistration]] =
    gcmGlobalService.registerGcm(accountId).future flatMap {
      case Some(reg) => post(reg) flatMap {
        case true => gcmGlobalService.updateRegisteredUser(reg.token, accountId).future map (Some(_))
        case false => Future.successful(Some(reg))
      }
      case None => Future.successful(None)
    }

  def handleNotification(n: PushNotification): Future[Any] = {
    // TODO: mark GCM active
    verbose(s"handleNotification($n")
    val (callStateEvents, otherEvents) = n.events.partition(_.isInstanceOf[CallStateEvent])

    // call state events can not be directly dispatched like the other events because they might be stale
    syncCallStateForConversations(callStateEvents.map(_.withCurrentLocalTime()))

    eventPipeline(otherEvents.map(_.withCurrentLocalTime()))
  }

  private def syncCallStateForConversations(events: Seq[Event]): Unit = {
    val convs: Set[RConvId] = events .collect { case e: CallStateEvent => e.convId } (breakOut)
    Future.traverse(convs) { convId =>
      convsContent.processConvWithRemoteId(convId, retryAsync = false) { conv =>
        sync.syncCallState(conv.id, fromFreshNotification = true)
      }
    }
  }
}
