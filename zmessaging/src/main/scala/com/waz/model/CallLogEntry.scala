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

import android.database.DatabaseUtils._
import com.waz.api.KindOfCallingEvent
import com.waz.api.KindOfCallingEvent._
import com.waz.db.BaseDao
import com.waz.db.Col._
import com.waz.utils.EnumCodec.injective
import com.waz.utils.wrappers.{DB, DBCursor}
import org.threeten.bp.Instant

case class CallLogEntry(event: KindOfCallingEvent, session: Option[CallSessionId], conv: ConvId, timestamp: Instant, isVideo: Boolean)

object CallLogEntry extends ((KindOfCallingEvent, Option[CallSessionId], ConvId, Instant, Boolean) => CallLogEntry) {

  implicit object CallLogEntryDao extends BaseDao[CallLogEntry] {
    val Event = int('event, eventCodec.encode, eventCodec.decode)(_.event)
    val Session = opt(id[CallSessionId]('session)).apply(_.session)
    val Conv = id[ConvId]('conv).apply(_.conv)
    val Timestamp = timestamp('timestamp)(_.timestamp)
    val IsVideo = bool('is_video)(_.isVideo)

    override val table = Table("CallLog", Event, Session, Conv, Timestamp, IsVideo)
    override def apply(implicit c: DBCursor): CallLogEntry = CallLogEntry(Event, Session, Conv, Timestamp, IsVideo)

    def countEstablished(video: Boolean)(implicit db: DB) =
      queryNumEntries(db, table.name, s"${Event.name} = ${Event(CALL_ESTABLISHED)} AND ${IsVideo.name} = ${IsVideo(video)}").toInt
  }

  val eventCodec = injective[KindOfCallingEvent, Int] {
    case RINGING_STARTED  => 0
    case RINGING_STOPPED  => 1
    case CALL_JOINED      => 2
    case CALL_ESTABLISHED => 3
    case CALL_ENDED       => 4
    case CALL_DROPPED     => 5
    case CALL_TRANSFERRED => 6
  }
}
