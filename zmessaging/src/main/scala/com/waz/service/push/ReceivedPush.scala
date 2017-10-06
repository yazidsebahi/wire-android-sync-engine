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

import com.waz.api.NetworkMode
import com.waz.content.Preferences.Preference.PrefCodec
import com.waz.model.Uid
import com.waz.utils.returning
import org.json
import org.json.JSONObject
import org.threeten.bp.{Duration, Instant}

/**
  * @param receivedAt instant the push notification was received
  * @param toFetch time elapsed until we successfully fetched the notification
  */
case class ReceivedPush(sinceSent:       Duration,
                        receivedAt:      Instant,
                        networkMode:     NetworkMode,
                        networkOperator: String,
                        toFetch:         Option[Duration] = None,
                        id:              Option[Uid]      = None)

object ReceivedPush {

  implicit lazy val ReceivedPushPrefCodec = new PrefCodec[ReceivedPush] {
    override def encode(v: ReceivedPush) = returning(new JSONObject()) { o =>
      o.put("since_sent", v.sinceSent.toMillis)
      o.put("received_at", v.receivedAt.toEpochMilli)
      o.put("network_mode", v.networkMode)
      o.put("network_operator", v.networkOperator)
      v.toFetch.foreach(d => o.put("to_fetch", d.toMillis))
      v.id.foreach(id => o.put("id", id))
    }.toString

    override def decode(str: String) = {
      import com.waz.utils.JsonDecoder._
      implicit val js = new json.JSONObject(str)
      ReceivedPush(
        decodeDuration('since_sent),
        decodeInstant('received_at),
        NetworkMode.valueOf(decodeString('network_mode)),
        decodeString('network_operator),
        decodeOptDuration('to_fetch),
        decodeOptId[Uid]('id)
      )
    }

    override val default = null
  }

}

case class MissedPushes(time: Instant, countMissed: Int, networkMode: NetworkMode, networkOperator: String)
