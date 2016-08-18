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

import android.database.Cursor
import com.waz.api.NotificationsHandler.GcmNotification
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.utils.{EnumCodec, JsonDecoder, JsonEncoder}
import org.json.JSONObject
import org.threeten.bp.Instant

case class NotificationData(id: String, msg: String, conv: ConvId, user: UserId, msgType: GcmNotification.Type, serverTime: Instant,
                            localTime: Instant = Instant.now, hotKnock: Boolean = false, userName: Option[String] = None,
                            mentions: Seq[UserId] = Seq.empty, referencedMessage: Option[MessageId] = None)

object NotificationData {

  implicit lazy val Decoder: JsonDecoder[NotificationData] = new JsonDecoder[NotificationData] {
    import JsonDecoder._

    override def apply(implicit js: JSONObject): NotificationData = NotificationData('id, 'message, 'conv, 'user,
      GcmNotificationCodec.decode('msgType), 'serverTime, decodeISOInstant('timestamp), 'hotKnock, 'userName,
      decodeUserIdSeq('mentions), decodeOptId[MessageId]('referencedMessage))
  }

  implicit lazy val Encoder: JsonEncoder[NotificationData] = new JsonEncoder[NotificationData] {
    override def apply(v: NotificationData): JSONObject = JsonEncoder { o =>
      o.put("id", v.id)
      o.put("message", v.msg)
      o.put("conv", v.conv.str)
      o.put("user", v.user.str)
      o.put("msgType", GcmNotificationCodec.encode(v.msgType))
      o.put("timestamp", JsonEncoder.encodeISOInstant(v.localTime))
      o.put("serverTime", v.serverTime.toEpochMilli)
      o.put("hotKnock", v.hotKnock)
      v.userName foreach (o.put("userName", _))
      if (v.mentions.nonEmpty) o.put("mentions", JsonEncoder.arrString(v.mentions.map(_.str)))
      v.referencedMessage foreach (o.put("referencedMessage", _))
    }
  }

  implicit object NotificationDataDao extends Dao[NotificationData, String] {
    val Id = text('_id, "PRIMARY KEY")(_.id)
    val Data = text('data)(JsonEncoder.encodeString(_))

    override val idCol = Id
    override val table = Table("NotificationData", Id, Data)

    override def apply(implicit cursor: Cursor): NotificationData = JsonDecoder.decode(cursor.getString(1))
  }

  implicit val NotificationOrdering: Ordering[NotificationData] = Ordering.by((data: NotificationData) => (data.localTime, data.id))

  implicit lazy val GcmNotificationCodec: EnumCodec[GcmNotification.Type, String] = EnumCodec.injective {
    case GcmNotification.Type.CONNECT_REQUEST => "ConnectRequest"
    case GcmNotification.Type.CONNECT_ACCEPTED => "ConnectAccepted"
    case GcmNotification.Type.CONTACT_JOIN => "ContactJoin"
    case GcmNotification.Type.ASSET => "Asset"
    case GcmNotification.Type.ANY_ASSET => "AnyAsset"
    case GcmNotification.Type.VIDEO_ASSET => "VideoAsset"
    case GcmNotification.Type.AUDIO_ASSET => "AudioAsset"
    case GcmNotification.Type.TEXT => "Text"
    case GcmNotification.Type.MEMBER_JOIN => "MemberJoin"
    case GcmNotification.Type.MEMBER_LEAVE => "MemberLeave"
    case GcmNotification.Type.RENAME => "Rename"
    case GcmNotification.Type.KNOCK => "Knock"
    case GcmNotification.Type.MISSED_CALL => "MissedCall"
    case GcmNotification.Type.LIKE => "Like"
    case GcmNotification.Type.LOCATION => "Location"
    case GcmNotification.Type.MESSAGE_SENDING_FAILED => "MessageSendingFailed"
  }
}
