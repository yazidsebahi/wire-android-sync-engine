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
package com.waz

import android.util.Base64
import com.google.protobuf.nano.{CodedInputByteBufferNano, MessageNano}
import com.waz.model.nano.Messages
import com.waz.utils.{JsonDecoder, JsonEncoder, returning}
import org.json.JSONObject

package object model {

  trait ProtoDecoder[A <: MessageNano] {
    def apply(data: Array[Byte]): A
    def apply(in: CodedInputByteBufferNano): A
  }

  val Proto = GenericContent

  type GenericMessage = Messages.GenericMessage
  object GenericMessage {
    import GenericContent._

    def apply[A: GenericContent](id: Uid, content: A): GenericMessage =
      returning(new Messages.GenericMessage()) { msg =>
        msg.messageId = id.str
        implicitly[GenericContent[A]].set(msg)(content)
      }

    def apply[A: GenericContent](id: MessageId, content: A): GenericMessage = apply(Uid(id.str), content)

    def apply(bytes: Array[Byte]): GenericMessage = Messages.GenericMessage.parseFrom(bytes)

    def unapply(msg: GenericMessage): Option[(Uid, Any)] = Some((Uid(msg.messageId), content(msg)))

    def toByteArray(msg: GenericMessage) = MessageNano.toByteArray(msg)

    def content(msg: GenericMessage) = {
      import Messages.{GenericMessage => GM}
      msg.getContentCase match {
        case GM.ASSET_FIELD_NUMBER        => msg.getAsset
        case GM.CALLING_FIELD_NUMBER      => msg.getCalling
        case GM.CLEARED_FIELD_NUMBER      => msg.getCleared
        case GM.CLIENTACTION_FIELD_NUMBER => ClientAction(msg.getClientAction)
        case GM.DELETED_FIELD_NUMBER      => msg.getDeleted
        case GM.EXTERNAL_FIELD_NUMBER     => msg.getExternal
        case GM.IMAGE_FIELD_NUMBER        => msg.getImage
        case GM.KNOCK_FIELD_NUMBER        => msg.getKnock
        case GM.LASTREAD_FIELD_NUMBER     => msg.getLastRead
        case GM.LIKING_FIELD_NUMBER       => LikingAction(msg.getLiking)
        case GM.TEXT_FIELD_NUMBER         => msg.getText
        case _                            => Unknown
      }
    }

    object TextMessage {

      def apply(text: String, mentions: Map[UserId, String]): GenericMessage = GenericMessage(Uid(), Text(text, mentions))(Text)

      def apply(msg: MessageData): GenericMessage = GenericMessage(Uid(msg.id.str), Text(msg.contentString, msg.content.flatMap(_.mentions).toMap))(Text)

      def unapply(msg: GenericMessage): Option[(String, Map[UserId, String])] = msg match {
        case GenericMessage(_, Text(content, mentions)) =>
          Some((content, mentions))
        case _ =>
          None
      }
    }

    implicit object JsDecoder extends JsonDecoder[GenericMessage] {
      override def apply(implicit js: JSONObject): GenericMessage = GenericMessage(Base64.decode(js.getString("proto"), Base64.DEFAULT))
    }

    implicit object JsEncoder extends JsonEncoder[GenericMessage] {
      override def apply(v: GenericMessage): JSONObject = JsonEncoder { o =>
        o.put("proto", Base64.encodeToString(MessageNano.toByteArray(v), Base64.NO_WRAP))
      }
    }

    implicit object MessageDecoder extends ProtoDecoder[GenericMessage] {
      override def apply(data: Array[Byte]): GenericMessage = GenericMessage(data)
      override def apply(in: CodedInputByteBufferNano): GenericMessage = Messages.GenericMessage.parseFrom(in)
    }
  }
}
