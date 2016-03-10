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
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.utils.{Json, JsonDecoder, JsonEncoder}
import org.json.JSONObject


case class CommonConnectionsData(user: UserId, totalCount: Int, connections: Seq[UserId], timestamp: Long = System.currentTimeMillis())

object CommonConnectionsData {

  implicit lazy val Encoder: JsonEncoder[CommonConnectionsData] = new JsonEncoder[CommonConnectionsData] {
    override def apply(v: CommonConnectionsData) = JsonEncoder { o =>
      o.put("user", v.user.str)
      o.put("totalCount", v.totalCount)
      o.put("connections", JsonEncoder.array(v.connections)((arr, u) => arr.put(u.str)))
      o.put("timestamp", v.timestamp)
    }
  }

  implicit lazy val Decoder: JsonDecoder[CommonConnectionsData] = new JsonDecoder[CommonConnectionsData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject) = CommonConnectionsData('user, 'totalCount, 'connections, 'timestamp)
  }

  implicit object CommonConnectionsDataDao extends Dao[CommonConnectionsData, UserId] {
    val Id = id[UserId]('_id, "PRIMARY KEY").apply(_.user)
    val Data = text('data, JsonEncoder.encodeString[CommonConnectionsData], JsonDecoder.decode[CommonConnectionsData]).apply(identity)
    val Timestamp = long('timestamp)(_.timestamp)

    override val idCol = Id
    override val table = Table("CommonConnections", Id, Data, Timestamp)

    override def apply(implicit cursor: Cursor): CommonConnectionsData = Data
  }
}
