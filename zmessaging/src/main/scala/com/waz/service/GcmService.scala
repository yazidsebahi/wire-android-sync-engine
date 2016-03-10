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
package com.waz.service

import com.waz.ZLog._
import com.waz.model._
import com.waz.service.GcmGlobalService.GcmRegistration
import com.waz.sync.SyncServiceHandle
import com.waz.utils.events.EventContext

import scala.concurrent.Future
class GcmService(user: ZUserId, gcmGlobalService: GcmGlobalService, sync: SyncServiceHandle, zuser: ZUserService) {

  implicit val dispatcher = gcmGlobalService.dispatcher

  private implicit val tag: LogTag = logTagFor[GcmService]
  private implicit val ev = EventContext.Global

  def gcmSenderId = gcmGlobalService.gcmSenderId

  def ensureGcmRegistered(): Future[Any] =
    gcmGlobalService.getGcmRegistration.future map {
      case r @ GcmRegistration(_, userId, _) if userId == user => verbose(s"ensureGcmRegistered() - already registered: $r")
      case _ => sync.registerGcm()
    }

  ensureGcmRegistered()

  zuser.onVerifiedLogin { _ foreach (_ => ensureGcmRegistered()) }

  zuser.onLogout { _ =>
    gcmGlobalService.getGcmRegistration map {
      case GcmRegistration(token, userId, _) if userId == user =>
        gcmGlobalService.clearGcmRegistrationUser(user)
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
    gcmGlobalService.registerGcm(user).future flatMap {
      case Some(reg) => post(reg) flatMap {
        case true => gcmGlobalService.updateRegisteredUser(reg.token, user).future map (Some(_))
        case false => Future.successful(Some(reg))
      }
      case None => Future.successful(None)
    }
}
