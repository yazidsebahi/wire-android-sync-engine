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

import com.waz.content.ConversationStorageImpl
import com.waz.model.GenericContent._
import com.waz.model._
import com.waz.service.{EventScheduler, UserServiceImpl}
import com.waz.threading.Threading
import com.waz.utils._

object UnarchivingEventProcessingStage {

  def apply(users: UserServiceImpl, storage: ConversationStorageImpl) = EventScheduler.Stage[UnarchivingEvent] { case (convId, events) =>
    import Threading.Implicits.Background

    users.withSelfUserFuture { selfUserId =>

      def unarchiveTime(es: Traversable[UnarchivingEvent]) = {
        val all = es.filter(shouldUnarchive(selfUserId, _))
        if (all.isEmpty) None
        else Some((all.maxBy(_.time).time.instant, all exists unarchiveMuted))
      }

      val convs = events.groupBy(_.convId).mapValues(unarchiveTime).filter(_._2.isDefined)

      storage.getByRemoteIds(convs.keys) flatMap { convIds =>
        storage.updateAll2(convIds, { conv =>
          convs.get(conv.remoteId).flatten match {
            case Some((time, unarchiveMuted)) if conv.archiveTime.isBefore(time) && (!conv.muted || unarchiveMuted) =>
              conv.copy(archived = false, archiveTime = time)
            case time =>
              conv
          }
        })
      }
    }
  }

  private def shouldUnarchive(selfUserId: UserId, event: Event): Boolean = event match {
    case MemberLeaveEvent(_, _, _, leaving) if leaving contains selfUserId => false
    case GenericMessageEvent(_, _, _, GenericMessage(_, content)) =>
      content match {
        case _: Text        => true
        case _: ImageAsset  => true
        case _: Knock       => true
        case _: Ephemeral   => true
        case _: Asset       => true
        case _: Location    => true
        case _              => false
      }
    case _: UnarchivingEvent => true
    case _ => false
  }

  private def unarchiveMuted(event: Event): Boolean = event match {
    case GenericMessageEvent(_, _, _, GenericMessage(_, _: Knock)) => true
    case _ => false
  }
}
