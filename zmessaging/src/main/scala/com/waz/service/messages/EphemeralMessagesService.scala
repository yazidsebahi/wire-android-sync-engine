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
package com.waz.service.messages

import com.waz.api.{EphemeralExpiration, Message}
import com.waz.content.{MessagesStorage, ZmsDatabase}
import com.waz.model.AssetStatus.{UploadDone, UploadFailed}
import com.waz.model.GenericContent.{Asset, ImageAsset}
import com.waz.model.MessageData.MessageDataDao
import com.waz.model._
import com.waz.sync.SyncServiceHandle
import com.waz.threading.CancellableFuture
import com.waz.utils._
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._

// TODO: obfuscate sent messages when they expire
class EphemeralMessagesService(selfUserId: UserId, messages: MessagesContentUpdater, storage: MessagesStorage, db: ZmsDatabase, sync: SyncServiceHandle) {
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global
  
  private val nextExpiryTime = Signal[Instant](Instant.MAX)

  val init = removeExpired()

  nextExpiryTime {
    case Instant.MAX => // nothing to expire
    case time => CancellableFuture.delayed((time.toEpochMilli - Instant.now.toEpochMilli).millis) { removeExpired() }
  }

  storage.onAdded { msgs =>
    updateNextExpiryTime(msgs.filter(_.userId != selfUserId).flatMap(_.expiryTime))
  }

  storage.onUpdated { updates =>
    updateNextExpiryTime(updates.filter(_._2.userId != selfUserId).flatMap(_._2.expiryTime))
  }

  private def updateNextExpiryTime(times: Seq[Instant]) = if (times.nonEmpty) {
    val time = times.min
    nextExpiryTime.mutate(_ min time)
  }

  private def removeExpired() = Serialized.future(this, "removeExpired") {
    nextExpiryTime ! Instant.MAX
    db.read { implicit db =>
      val time = Instant.now
      MessageDataDao.findExpiring(selfUserId) acquire { msgs =>
        val (toRemove, rest) = msgs.toStream.span(_.expiryTime.exists(_ <= time))
        rest.headOption.flatMap(_.expiryTime) foreach { time =>
          nextExpiryTime.mutate(_ min time)
        }
        toRemove.toVector
      }
    } flatMap { expired =>
      messages.deleteOnUserRequest(expired.map(_.id)) flatMap { _ =>
        // recalling message, this informs the sender that message is already expired
        Future.traverse(expired) { m => sync.postRecalled(m.convId, MessageId(), m.id) }
      }
    }
  }

  // start expiration timer for ephemeral message
  def onMessageRead(id: MessageId) = storage.update(id, { msg =>
    if (shouldStartTimer(msg)) msg.copy(expiryTime = msg.ephemeral.expiryFromNow())
    else msg
  })

  private def shouldStartTimer(msg: MessageData) = {
    if (msg.ephemeral == EphemeralExpiration.NONE || msg.expiryTime.isDefined) false // timer already started
    else msg.msgType match {
      case MessageData.IsAsset() | Message.Type.ASSET =>
        // check if asset was fully uploaded
        msg.protos.exists {
          case GenericMessage(_, Asset(_, _, UploadDone(_) | UploadFailed)) => true
          case GenericMessage(_, ImageAsset(ImageData.Tag.Medium, _, _, _, _, _, _, _, _)) => true
          case _ => false
        }
      case _ => true
    }
  }
}
