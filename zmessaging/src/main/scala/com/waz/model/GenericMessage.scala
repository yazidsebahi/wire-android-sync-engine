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
package com.waz.model

import android.util.Base64
import com.google.protobuf.nano.MessageNano
import com.waz.model.nano.Messages
import com.waz.model.nano.Messages.ImageAsset
import com.waz.model.otr.{ClientId, OtrKey, Sha256}
import com.waz.utils.returning
import org.threeten.bp.Instant

import scala.collection.breakOut

trait ProtoMsg {
  def encode(): MessageNano
  def toByteArray = MessageNano.toByteArray(encode())
  def encodeToString = Base64.encodeToString(toByteArray, Base64.DEFAULT)
}

case class GenericMessage(id: Uid, content: GenericMessage.Content) extends ProtoMsg {
  import GenericMessage._

  lazy val encode = returning(new Messages.GenericMessage) { m =>
    m.messageId = id.str
    content match {
      case t: GenericMessage.Text       => m.setText(t.encode)
      case i: GenericMessage.Image      => m.setImage(i.encode)
      case k: GenericMessage.Knock      => m.setKnock(k.encode)
      case l: GenericMessage.LikeAction => m.setLiking(l.encode)
      case lr: GenericMessage.LastRead  => m.setLastRead(lr.encode)
      case e: GenericMessage.External   => m.setExternal(e.encode)
      case c: GenericMessage.Cleared    => m.setCleared(c.encode)
      case SessionReset                 => m.setClientAction(Messages.RESET_SESSION)
      case _: GenericMessage.OtrError => throw new Exception(s"Trying to encode OtrError GenericMessage: $this")
      case GenericMessage.Empty =>       throw new Exception(s"Trying to encode empty GenericMessage: $this")
      case GenericMessage.Duplicate =>   throw new Exception(s"Trying to encode empty GenericMessage: $this")
    }
  }
}

object GenericMessage {

  def apply(id: MessageId, content: GenericMessage.Content): GenericMessage = new GenericMessage(Uid(id.str), content)

  def apply(data: Array[Byte]): GenericMessage = apply(Messages.GenericMessage.parseFrom(data))

  def decode(data: Array[Byte]): GenericMessage = apply(Messages.GenericMessage.parseFrom(data))

  def apply(str: String): GenericMessage = apply(Base64.decode(str, Base64.DEFAULT))

  def apply(proto: Messages.GenericMessage): GenericMessage = {
    val id = Uid(proto.messageId)
    val content = proto.getContentCase match {
      case Messages.GenericMessage.TEXT_FIELD_NUMBER          => Text(proto.getText)
      case Messages.GenericMessage.IMAGE_FIELD_NUMBER         => Image(proto.getImage)
      case Messages.GenericMessage.KNOCK_FIELD_NUMBER         => Knock(proto.getKnock)
      case Messages.GenericMessage.LIKING_FIELD_NUMBER        => LikeAction(proto.getLiking)
      case Messages.GenericMessage.LASTREAD_FIELD_NUMBER      => LastRead(proto.getLastRead)
      case Messages.GenericMessage.EXTERNAL_FIELD_NUMBER      => External(proto.getExternal)
      case Messages.GenericMessage.CLEARED_FIELD_NUMBER       => Cleared(proto.getCleared)
      case Messages.GenericMessage.CLIENTACTION_FIELD_NUMBER  =>
        proto.getClientAction match {
          case Messages.RESET_SESSION => SessionReset
          case _                      => Empty
        }
      case _ => Empty
    }
    GenericMessage(id, content)
  }

  sealed trait Content

  case object Duplicate extends Content

  case object Empty extends Content

  case object SessionReset extends Content

  case class OtrError(msg: String, from: UserId, sender: ClientId) extends Content

  case class Text(content: String, mentions: Map[UserId, String]) extends Content {

    def encode: Messages.Text = returning(new Messages.Text) { text =>
      text.content = content
      text.mention = mentions.map { case (id, name) =>
        returning(new Messages.Mention) { m =>
          m.userId = id.str
          m.userName = name
        }
      } (breakOut)
    }
  }

  object Text {
    def apply(proto: Messages.Text): Text =
      new Text(proto.content, proto.mention.map(m => UserId(m.userId) -> m.userName)(breakOut))
  }

  case class Knock(hotKnock: Boolean = false) extends Content {
    def encode = returning(new Messages.Knock)(_.hotKnock = hotKnock)
  }

  object Knock {
    def apply(proto: Messages.Knock): Knock = new Knock(proto.hotKnock)
  }

  case class Image(tag: String, width: Int, height: Int, origWidth: Int, origHeight: Int, mime: String, size: Int, otrKey: Option[OtrKey], sha256: Option[Sha256]) extends Content {

    def encode: Messages.ImageAsset = returning(new ImageAsset) { img =>
      img.tag = tag
      img.width = width
      img.height = height
      img.originalWidth = origWidth
      img.originalHeight = origHeight
      img.mimeType = mime
      img.size = size
      otrKey foreach { key => img.otrKey = key.bytes }
      sha256 foreach { sha => img.sha256 = sha.bytes }
    }
  }

  object Image {
    def apply(proto: Messages.ImageAsset): Image = {
      val key = Option(proto.otrKey).map(OtrKey(_))
      val sha = Option(proto.sha256).map(Sha256(_))

      new Image(proto.tag, proto.width, proto.height, proto.originalWidth, proto.originalHeight, proto.mimeType, proto.size, key, sha)
    }
  }

  case class LikeAction(action: Liking.Action) extends Content {
    def encode = action match {
      case Liking.Action.Like => Messages.LIKE
      case Liking.Action.Unlike => Messages.UNLIKE
    }
  }
  object LikeAction {
    def apply(protoAction: Int): LikeAction = LikeAction(protoAction match {
      case Messages.LIKE => Liking.Action.Like
      case Messages.UNLIKE => Liking.Action.Unlike
    })
  }

  case class Cleared(conv: RConvId, time: Instant) extends Content {
    def encode = returning(new Messages.Cleared) { c =>
      c.conversationId = conv.str
      c.clearedTimestamp = time.toEpochMilli
    }
  }
  object Cleared {
    def apply(proto: Messages.Cleared): Cleared = new Cleared(RConvId(proto.conversationId), Instant.ofEpochMilli(proto.clearedTimestamp))
  }

  case class LastRead(conv: RConvId, time: Instant) extends Content {
    def encode = returning(new Messages.LastRead) { l =>
      l.conversationId = conv.str
      l.lastReadTimestamp = time.toEpochMilli
    }
  }
  object LastRead {
    def apply(proto: Messages.LastRead): LastRead = new LastRead(RConvId(proto.conversationId), Instant.ofEpochMilli(proto.lastReadTimestamp))
  }

  case class External(key: OtrKey, sha: Option[Sha256]) extends Content {
    def encode = returning(new Messages.External) { ext =>
      ext.otrKey = key.bytes
      sha foreach { s => ext.sha256 = s.bytes }
    }
  }
  object External {
    def apply(proto: Messages.External): External =
      new External(OtrKey(proto.otrKey), Option(proto.sha256).map(Sha256(_)))
  }
}
