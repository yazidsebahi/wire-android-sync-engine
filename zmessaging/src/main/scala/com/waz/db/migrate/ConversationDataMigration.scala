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
package com.waz.db.migrate

import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import com.waz.api.Verification
import com.waz.db.Col._
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.SearchKey
import com.waz.utils._
import org.threeten.bp.Instant

object ConversationDataMigration {

  lazy val v64 = { db: SQLiteDatabase =>

    val moveConvs = new TableMigration(TableDesc("Conversations", Columns.v63.all), TableDesc("Conversations_tmp", Columns.v64.all)) {
      import Columns.{v63 => src, v64 => dst}
      
      override val bindings: Seq[Binder] = Seq(
        dst.Id := src.Id,
        dst.RemoteId := src.RemoteId,
        dst.Name := src.Name,
        dst.Creator := src.Creator,
        dst.ConvType := src.ConvType,
        dst.LastEventTime := src.LastEventTime.andThen(_.instant),
        dst.LastEvent := src.LastEvent,
        dst.Status := src.Status,
        dst.StatusTime := src.StatusTime.andThen(_.instant),
        dst.LastRead := { c: Cursor =>
          src.LastReadTime(c).getOrElse(src.LastEventTime(c).instant)
        },
        dst.Muted := src.Muted,
        dst.MutedTime := src.MutedTime.andThen(_.fold(Instant.EPOCH)(_.instant)),
        dst.Archived := src.Archived.andThen(_.isDefined),
        dst.ArchivedTime := src.ArchivedTime.andThen(_.getOrElse(Instant.EPOCH)),
        dst.Cleared := src.ClearedTime.andThen(_.getOrElse(Instant.EPOCH)),
        dst.GeneratedName := src.GeneratedName,
        dst.SKey := src.SKey,
        dst.UnreadCount := src.UnreadCount,
        dst.FailedCount := src.FailedCount,
        dst.HasVoice := src.HasVoice,
        dst.UnjoinedCall := src.UnjoinedCall,
        dst.MissedCall := src.MissedCall,
        dst.IncomingKnock := src.IncomingKnock,
        dst.RenameEvent := src.RenameEvent,
        dst.VoiceMuted := src.VoiceMuted,
        dst.Hidden := src.Hidden,
        dst.Verified := src.Verified
      )
    }

    db.execSQL("DROP TABLE IF EXISTS Conversations_tmp")
    moveConvs.migrate(db)
    db.execSQL("ALTER TABLE Conversations RENAME TO Conversations_old")
    db.execSQL("ALTER TABLE Conversations_tmp RENAME TO Conversations")
    db.execSQL("DROP TABLE Conversations_old")
  }

  object Columns {

    object v63 {
      val Id = id[ConvId]('_id, "PRIMARY KEY")
      val RemoteId = id[RConvId]('remote_id)
      val Name = opt(text('name))
      val Creator = id[UserId]('creator)
      val ConvType = int[ConversationType]('conv_type, _.id, ConversationType(_))
      val LastEventTime = date('last_event_time)
      val LastEvent = eid('last_event)
      val Status = int('status)
      val StatusTime = date('status_time)
      val StatusRef = eid('status_ref)
      val LastRead = opt(eid('last_read))
      val LastReadTime = opt(timestamp('last_read_time))
      val Muted = bool('muted)
      val MutedTime = opt(date('mute_time))
      val Archived = opt(eid('archived))
      val ArchivedTime = opt(timestamp('archived_time))
      val Cleared = opt(eid('cleared))
      val ClearedTime = opt(timestamp('cleared_time))
      val GeneratedName = text('generated_name)
      val SKey = opt(text[SearchKey]('search_key, _.asciiRepresentation, SearchKey.unsafeRestore))
      val UnreadCount = int('unread_count)
      val FailedCount = int('unsent_count)
      val HasVoice = bool('has_voice)
      val UnjoinedCall = bool('unjoined_call)
      val MissedCall = opt(id[MessageId]('missed_call))
      val IncomingKnock = opt(id[MessageId]('incoming_knock))
      val RenameEvent = opt(eid('rename_event))
      val VoiceMuted = bool('voice_muted)
      val Hidden = bool('hidden)
      val Verified = text[Verification]('verified, _.name, Verification.valueOf)

      val all = Seq(Id, RemoteId, Name, Creator, ConvType, LastEventTime, LastEvent, Status, StatusTime, StatusRef, LastRead, LastReadTime, Muted, MutedTime, Archived, ArchivedTime, Cleared, ClearedTime, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, VoiceMuted, Hidden, MissedCall, IncomingKnock, RenameEvent, UnjoinedCall, Verified)
    }

    object v64 {
      val Id = id[ConvId]('_id, "PRIMARY KEY")
      val RemoteId = id[RConvId]('remote_id)
      val Name = opt(text('name))
      val Creator = id[UserId]('creator)
      val ConvType = int[ConversationType]('conv_type, _.id, ConversationType(_))
      val LastEventTime = timestamp('last_event_time)
      val LastEvent = eid('last_event)
      val Status = int('status)
      val StatusTime = timestamp('status_time)
      val LastRead = timestamp('last_read)
      val Muted = bool('muted)
      val MutedTime = timestamp('mute_time)
      val Archived = bool('archived)
      val ArchivedTime = timestamp('archive_time)
      val Cleared = timestamp('cleared)
      val GeneratedName = text('generated_name)
      val SKey = opt(text[SearchKey]('search_key, _.asciiRepresentation, SearchKey.unsafeRestore))
      val UnreadCount = int('unread_count)
      val FailedCount = int('unsent_count)
      val HasVoice = bool('has_voice)
      val UnjoinedCall = bool('unjoined_call)
      val MissedCall = opt(id[MessageId]('missed_call))
      val IncomingKnock = opt(id[MessageId]('incoming_knock))
      val RenameEvent = opt(eid('rename_event))
      val VoiceMuted = bool('voice_muted)
      val Hidden = bool('hidden)
      val Verified = text[Verification]('verified, _.name, Verification.valueOf)

      val all = Seq(Id, RemoteId, Name, Creator, ConvType, LastEventTime, LastEvent, Status, StatusTime, LastRead, Muted, MutedTime, Archived, ArchivedTime, Cleared, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, VoiceMuted, Hidden, MissedCall, IncomingKnock, RenameEvent, UnjoinedCall, Verified)
    }
  }
}
