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

import java.nio.{ByteBuffer, ByteOrder}
import java.util.UUID

import android.content.Context
import com.waz.ZLog._
import com.waz.model.UserId
import com.waz.soundlink.{SoundLink, SoundLinkListener}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events._

import scala.concurrent.duration._
import scala.util.Try

class AudioLinkService(val context: Context, metadata: MetaDataService, userId: UserId, lifecycle: ZmsLifecycle) {
  private implicit val logTag: LogTag = logTagFor[AudioLinkService]
  private implicit val ev = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "AudioLinkService")

  import com.waz.service.AudioLinkService._

  lazy val audioLink: AudioLink = AudioLink(this)

  private var sending = false
  private var detectedUsers = Map.empty[UserId, Long]

  
  val nearbyUsers = new SourceSignal[Seq[UserId]](Some(Nil)) {
    override protected def onWire(): Unit = {
      debug("start listening")
      audioLink.startListening()
    }

    override protected def onUnwire(): Unit = {
      debug("stop listening")
      audioLink.stopListening()
    }
  }

  private val transmittedId = lifecycle.lifecycleState map {
    case LifecycleState.UiActive if metadata.audioLinkEnabled => Some(userId)
    case _ => None
  }

  transmittedId.on(dispatcher) {
    case None =>
      if (sending) {
        debug("stop sending")
        audioLink.stopSending()
      }
    case Some(id) =>
      debug(s"start sending: '$id'")
      sending = true
      audioLink.startSending(encode(id))
  }

  private[service] def messageReceived(bytes: Array[Byte]) = dispatcher {
    decode(bytes).fold {
      warn(s"Received invalid message, expecting 16 Bytes got: ${bytes.toSeq}")
    } { id =>
      debug(s"Received userId: $id")

      updateUsers(Some(id))
      CancellableFuture.delayed(NearbyUserTimeout) { updateUsers(None) }
    }
  }

  private def updateUsers(add: Option[UserId]): Unit = {
    val time = System.currentTimeMillis()
    val timeout = time - NearbyUserTimeout.toMillis
    val size = detectedUsers.size
    detectedUsers = detectedUsers.filter(_._2 > timeout)
    add.foreach(id => detectedUsers += id -> time)
    if (detectedUsers.size != size) nearbyUsers ! detectedUsers.keys.toVector
  }
}

object AudioLinkService {

  val NearbyUserTimeout = 20.seconds

  def decode(bytes: Array[Byte]): Option[UserId] =
    if (bytes.length != 16) None
    else {
      val buffer = ByteBuffer.wrap(bytes)
      buffer.order(ByteOrder.BIG_ENDIAN)
      Some(UserId(new UUID(buffer.getLong, buffer.getLong).toString))
    }


  def encode(id: UserId): Array[Byte] = {
    Try(UUID.fromString(id.str)).map { uuid =>
      ByteBuffer.allocate(16)
        .order(ByteOrder.BIG_ENDIAN)
        .putLong(uuid.getMostSignificantBits)
        .putLong(uuid.getLeastSignificantBits)
        .array()
    } .getOrElse(Array.empty[Byte])
  }
}

trait AudioLink {
  def startListening(): Unit
  def stopListening(): Unit
  def startSending(msg: Array[Byte]): Unit
  def stopSending(): Unit
}

object AudioLink {
  private implicit val logTag: LogTag = logTagFor[AudioLink]

  def apply(service: AudioLinkService): AudioLink = try {
    soundLink(service)
  } catch {
    case e: Throwable =>
      error("SoundLink initialization failed", e)
      new AudioLink {
        override def startSending(msg: Array[Byte]): Unit = ()
        override def stopSending(): Unit = ()
        override def startListening(): Unit = ()
        override def stopListening(): Unit = ()
      }
  }

  def soundLink(service: AudioLinkService): AudioLink = new AudioLink {
    val soundLink = new SoundLink(service.context)
    soundLink.setListener(new SoundLinkListener {
      override def onMessage(bytes: Array[Byte]): Unit = service.messageReceived(bytes)
      override def handleError(err: Int): Unit = {
        warn(s"SoundLink reported error: $err") // TODO: should we do anything about that?
      }
    })

    override def startListening(): Unit = soundLink.startListen()
    override def stopListening(): Unit = soundLink.stopListen()
    override def stopSending(): Unit = soundLink.stopSend()
    override def startSending(msg: Array[Byte]): Unit = soundLink.startSend(msg)
  }
}
