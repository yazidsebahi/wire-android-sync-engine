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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.api.{EphemeralExpiration, Message}
import com.waz.content.{MessagesStorage, ZmsDatabase}
import com.waz.model.AssetStatus.{UploadDone, UploadFailed}
import com.waz.model.GenericContent.{Asset, Ephemeral, Location, Text}
import com.waz.model.MessageData.MessageDataDao
import com.waz.model._
import com.waz.model.sync.ReceiptType
import com.waz.sync.SyncServiceHandle
import com.waz.threading.CancellableFuture
import com.waz.utils._
import com.waz.utils.events.Signal
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

// TODO: obfuscate sent messages when they expire
class EphemeralMessagesService(selfUserId: UserId, messages: MessagesContentUpdater, storage: MessagesStorage, db: ZmsDatabase, sync: SyncServiceHandle) {
  import EphemeralMessagesService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global
  
  private val nextExpiryTime = Signal[Instant](Instant.MAX)

  val init = removeExpired()

  nextExpiryTime {
    case Instant.MAX => // nothing to expire
    case time => CancellableFuture.delayed((time.toEpochMilli - Instant.now.toEpochMilli).millis) { removeExpired() }
  }

  storage.onAdded { msgs =>
    updateNextExpiryTime(msgs.flatMap(_.expiryTime))
  }

  storage.onUpdated { updates =>
    updateNextExpiryTime(updates.flatMap(_._2.expiryTime))
  }

  private def updateNextExpiryTime(times: Seq[Instant]) = if (times.nonEmpty) {
    val time = times.min
    nextExpiryTime.mutate(_ min time)
  }

  private def removeExpired() = Serialized.future(this, "removeExpired") {
    verbose(s"removeExpired")
    nextExpiryTime ! Instant.MAX
    db.read { implicit db =>
      val time = Instant.now
      MessageDataDao.findExpiring() acquire { msgs =>
        val (expired, rest) = msgs.toStream.span(_.expiryTime.exists(_ <= time))
        rest.headOption.flatMap(_.expiryTime) foreach { time =>
          nextExpiryTime.mutate(_ min time)
        }
        expired.toVector
      }
    } flatMap { expired =>
      val (toObfuscate, toRemove) = expired.partition(_.userId == selfUserId)
      for {
        _ <- messages.deleteOnUserRequest(toRemove.map(_.id))
        // recalling message, this informs the sender that message is already expired
        _ <- Future.traverse(toRemove) { m => sync.postReceipt(m.convId, m.id, m.userId, ReceiptType.EphemeralExpired) }
        _ <- messages.messagesStorage.updateAll2(toObfuscate.map(_.id), obfuscate)
      } yield ()
    }
  }

  private def obfuscate(msg: MessageData): MessageData = {
    import Message.Type._
    verbose(s"obfuscate($msg)")

    def obfuscate(text: String) = text.map { c =>
      if (c.isWhitespace) c else randomChars.next
    }

    msg.msgType match {
      case TEXT | TEXT_EMOJI_ONLY =>
        msg.copy(expired = true, content = Nil, protos = Seq(GenericMessage(msg.id.uid, Text(obfuscate(msg.contentString)))))
      case RICH_MEDIA =>
        val content = msg.content map { ct =>
          ct.copy(content = obfuscate(ct.content), openGraph = None) //TODO: asset and rich media
        }
        msg.copy(expired = true, content = content, protos = Seq(GenericMessage(msg.id.uid, Text(obfuscate(msg.contentString))))) // TODO: obfuscate links
      // TODO: delete assets, should be enough to replace remote id
//      case ASSET =>
//        ???
//      case ANY_ASSET =>
//        ???
      case LOCATION =>
        val (name, zoom) = msg.location.fold(("", 14)) { l => (obfuscate(l.getName), l.getZoom) }
        msg.copy(expired = true, content = Nil, protos = Seq(GenericMessage(msg.id.uid, Location(0, 0, name, zoom))))
      case _ =>
        msg.copy(expired = true)
    }
  }

  // start expiration timer for ephemeral message
  def onMessageRead(id: MessageId) = storage.update(id, { msg =>
    if (shouldStartTimer(msg)) msg.copy(expiryTime = msg.ephemeral.expiryFromNow())
    else msg
  })

  private def shouldStartTimer(msg: MessageData) = {
    if (msg.ephemeral == EphemeralExpiration.NONE || msg.expiryTime.isDefined) false // timer already started
    else if (msg.userId == selfUserId) false // timer for own messages is started in MessagesService.messageSent
    else msg.msgType match {
      case MessageData.IsAsset() | Message.Type.ASSET =>
        // check if asset was fully uploaded
        msg.protos.exists {
          case GenericMessage(_, Ephemeral(_, Asset(AssetData.WithStatus(UploadDone | UploadFailed), _))) => true
          case _ => false
        }
      case _ => true
    }
  }
}

object EphemeralMessagesService {

  val randomChars = {
    val cs = ('a' to 'z') ++ ('A' to 'Z')
    Iterator continually cs(Random.nextInt(cs.size))
  }
}
