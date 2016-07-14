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
import android.database.sqlite.SQLiteDatabase
import com.waz.api.{IConversation, Verification}
import com.waz.db.Col._
import com.waz.db.{Dao, Dao2}
import com.waz.model.ConversationData.{ConversationStatus, ConversationType}
import com.waz.service.SearchKey
import com.waz.utils.{JsonDecoder, JsonEncoder, _}
import org.json.JSONObject
import org.threeten.bp.Instant

case class ConversationData(id: ConvId,
                            remoteId: RConvId,
                            name: Option[String],
                            creator: UserId,
                            convType: ConversationType,
                            lastEventTime: Instant = Instant.EPOCH,
                            lastEvent: EventId = EventId.Zero,
                            status: Int = 0,
                            statusTime: Instant = Instant.EPOCH,
                            lastRead: Instant = Instant.EPOCH,
                            muted: Boolean = false,
                            muteTime: Instant = Instant.EPOCH,
                            archived: Boolean = false,
                            archiveTime: Instant = Instant.EPOCH,
                            cleared: Instant = Instant.EPOCH,
                            generatedName: String = "",
                            searchKey: Option[SearchKey] = None,
                            unreadCount: Int = 0,
                            failedCount: Int = 0,
                            hasVoice: Boolean = false,
                            unjoinedCall: Boolean = false,
                            missedCallMessage: Option[MessageId] = None,
                            incomingKnockMessage: Option[MessageId] = None,
                            renameEvent: Option[EventId] = None,
                            voiceMuted: Boolean = false,
                            hidden: Boolean = false,
                            verified: Verification = Verification.UNKNOWN) {

  def activeMember = status == ConversationStatus.Active.value

  def displayName = if (convType == ConversationType.Group) name.getOrElse(generatedName) else generatedName

  def withFreshSearchKey = copy(searchKey = freshSearchKey)
  def savedOrFreshSearchKey = searchKey.orElse(freshSearchKey)
  def freshSearchKey = if (convType == ConversationType.Group) name map SearchKey else None

  lazy val completelyCleared = ! cleared.isBefore(lastEventTime)

  def withLastRead(time: Instant) = copy(lastRead = lastRead max time)

  def withCleared(time: Instant) = copy(cleared = cleared max time)

  def isOtto = convType == ConversationType.OneToOne && id.str == UserId.ofOtto.str

  def updated(d: ConversationData): Option[ConversationData] = {
    val ct = if (ConversationType.isOneToOne(convType) && d.convType != ConversationType.OneToOne) convType else d.convType
    val st = if (d.statusTime.isAfter(statusTime)) d.status else status
    val nameSource = (renameEvent, d.renameEvent) match {
      case (None, Some(a)) => d
      case (Some(a), Some(b)) if b > a => d
      case _ => this
    }

    val updated = copy(remoteId = d.remoteId, name = nameSource.name, creator = d.creator, convType = ct,
      lastEventTime = lastEventTime max d.lastEventTime, lastEvent = d.lastEvent max lastEvent, status = st, statusTime = statusTime max d.statusTime,
      lastRead = lastRead max d.lastRead, muted = d.muted,
      muteTime = d.muteTime, archived = d.archived, cleared = cleared max d.cleared,
      renameEvent = nameSource.renameEvent, searchKey = nameSource.searchKey)

    if (updated == this) None else Some(updated)
  }
}

/**
 * Conversation user binding.
 *
 * @param active - false if user left the conversation
 */
case class ConversationMemberData(userId: UserId, convId: ConvId, active: Boolean = true, eventId: EventId = EventId.Zero)

object ConversationData {

  sealed trait ConversationStatus {
    val value: Int
  }
  object ConversationStatus {
    case object Active extends ConversationStatus {
      override val value: Int = 0
    }
    case object Inactive extends ConversationStatus {
      override val value: Int = 1
    }
  }

  val Empty = ConversationData(ConvId(), RConvId(), None, UserId(), IConversation.Type.UNKNOWN)

  // total (!) ordering for use in ordered sets; handwritten (instead of e.g. derived from tuples) to avoid allocations
  implicit val ConversationDataOrdering: Ordering[ConversationData] = new Ordering[ConversationData] {
    override def compare(b: ConversationData, a: ConversationData): Int =
      if (a.id == b.id) 0
      else if (a.hasVoice == b.hasVoice || a.unjoinedCall == b.unjoinedCall) {
        val c = a.lastEventTime.compareTo(b.lastEventTime)
        if (c != 0) c
        else a.id.str.compareTo(b.id.str)
      }
      else if (a.hasVoice) 1 else if (b.hasVoice) -1 else if (a.unjoinedCall) 1 else -1
  }

  type ConversationType = IConversation.Type
  object ConversationType {
    val Unknown = IConversation.Type.UNKNOWN
    val Group = IConversation.Type.GROUP
    val OneToOne = IConversation.Type.ONE_TO_ONE
    val Self = IConversation.Type.SELF
    val WaitForConnection = IConversation.Type.WAIT_FOR_CONNECTION
    val Incoming = IConversation.Type.INCOMING_CONNECTION

    def apply(id: Int) = IConversation.Type.withId(id)

    def isOneToOne(tp: IConversation.Type) = tp == OneToOne || tp == WaitForConnection || tp == Incoming

    def values = Set(Unknown, Group, OneToOne, Self, WaitForConnection, Incoming)
  }

  implicit lazy val Decoder: JsonDecoder[ConversationData] = new JsonDecoder[ConversationData] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): ConversationData = ConversationData(
      id = 'id, remoteId = 'remoteId, name = decodeOptString('name), creator = 'creator, convType = ConversationType('convType),
      lastEventTime = decodeInstant('lastEventTime), lastEvent = decodeEid('lastEvent),
      status = 'status, statusTime = decodeInstant('statusTime),
      lastRead = decodeInstant('lastReadTime),
      muted = 'muted, muteTime = decodeInstant('muteTime), archived = 'archived,
      archiveTime = decodeInstant('archiveTime), cleared = decodeInstant('cleared),
      generatedName = 'generatedName, searchKey = decodeOptString('name) map SearchKey,
      unreadCount = 'unreadCount, failedCount = 'failedCount, hasVoice = 'hasVoice, unjoinedCall = 'unjoinedCall,
      missedCallMessage = decodeOptMessageId('missedCallMessage), incomingKnockMessage = decodeOptMessageId('incomingKnockMessage),
      renameEvent = decodeOptEid('renameEvent), voiceMuted = 'voiceMuted, hidden = 'hidden, verified = decodeOptString('verified).fold(Verification.UNKNOWN)(Verification.valueOf))
  }

  implicit lazy val Encoder: JsonEncoder[ConversationData] = new JsonEncoder[ConversationData] {
    override def apply(v: ConversationData): JSONObject = JsonEncoder { o =>
      o.put("id", v.id.str)
      o.put("remoteId", v.remoteId.str)
      v.name foreach (o.put("name", _))
      o.put("creator", v.creator.str)
      o.put("convType", v.convType.id)
      o.put("lastEventTime", v.lastEventTime.toEpochMilli)
      o.put("lastEvent", v.lastEvent.str)
      o.put("status", v.status)
      o.put("statusTime", v.statusTime.toEpochMilli)
      o.put("lastReadTime", v.lastRead.toEpochMilli)
      o.put("muted", v.muted)
      o.put("muteTime", v.muteTime.toEpochMilli)
      o.put("archived", v.archived)
      o.put("archiveTime", v.archiveTime.toEpochMilli)
      o.put("cleared", v.cleared.toEpochMilli)
      o.put("generatedName", v.generatedName)
      o.put("unreadCount", v.unreadCount)
      o.put("failedCount", v.failedCount)
      o.put("hasVoice", v.hasVoice)
      o.put("unjoinedCall", v.unjoinedCall)
      v.missedCallMessage foreach (id => o.put("missedCallMessage", id.str))
      v.incomingKnockMessage foreach (id => o.put("incomingKnockMessage", id.str))
      v.renameEvent foreach (id => o.put("renameEvent", id.str))
      o.put("voiceMuted", v.voiceMuted)
      o.put("hidden", v.hidden)
      o.put("trusted", v.verified)
    }
  }

  implicit object ConversationDataDao extends Dao[ConversationData, ConvId] {
    val Id            = id[ConvId]('_id, "PRIMARY KEY").apply(_.id)
    val RemoteId      = id[RConvId]('remote_id).apply(_.remoteId)
    val Name          = opt(text('name))(_.name.filterNot(_.isEmpty))
    val Creator       = id[UserId]('creator).apply(_.creator)
    val ConvType      = int[ConversationType]('conv_type, _.id, ConversationType(_))(_.convType)
    val LastEventTime = timestamp('last_event_time)(_.lastEventTime)
    val LastEvent     = eid('last_event)(_.lastEvent)
    val Status        = int('status)(_.status)
    val StatusTime    = timestamp('status_time)(_.statusTime)
    val LastRead      = timestamp('last_read)(_.lastRead)
    val Muted         = bool('muted)(_.muted)
    val MutedTime     = timestamp('mute_time)(_.muteTime)
    val Archived      = bool('archived)(_.archived)
    val ArchivedTime  = timestamp('archive_time)(_.archiveTime)
    val Cleared       = timestamp('cleared)(_.cleared)
    val GeneratedName = text('generated_name)(_.generatedName)
    val SKey          = opt(text[SearchKey]('search_key, _.asciiRepresentation, SearchKey.unsafeRestore))(_.searchKey)
    val UnreadCount   = int('unread_count)(_.unreadCount)
    val FailedCount   = int('unsent_count)(_.failedCount)
    val HasVoice      = bool('has_voice)(_.hasVoice)
    val UnjoinedCall  = bool('unjoined_call)(_.unjoinedCall)
    val MissedCall    = opt(id[MessageId]('missed_call))(_.missedCallMessage)
    val IncomingKnock = opt(id[MessageId]('incoming_knock))(_.incomingKnockMessage)
    val RenameEvent   = opt(eid('rename_event))(_.renameEvent)
    val VoiceMuted    = bool('voice_muted)(_.voiceMuted)
    val Hidden        = bool('hidden)(_.hidden)
    val Verified      = text[Verification]('verified, _.name, Verification.valueOf)(_.verified)

    override val idCol = Id
    override val table = Table("Conversations", Id, RemoteId, Name, Creator, ConvType, LastEventTime, LastEvent, Status, StatusTime, LastRead, Muted, MutedTime, Archived, ArchivedTime, Cleared, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, VoiceMuted, Hidden, MissedCall, IncomingKnock, RenameEvent, UnjoinedCall, Verified)
    override def apply(implicit cursor: Cursor): ConversationData = ConversationData(Id, RemoteId, Name, Creator, ConvType, LastEventTime, LastEvent, Status, StatusTime, LastRead, Muted, MutedTime, Archived, ArchivedTime, Cleared, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, UnjoinedCall, MissedCall, IncomingKnock, RenameEvent, VoiceMuted, Hidden, Verified)

    import com.waz.model.ConversationData.ConversationType._

    def establishedConversations(implicit db: SQLiteDatabase) = iterating(db.rawQuery(
      s"""SELECT *
         |  FROM ${table.name}
         | WHERE (${ConvType.name} = ${ConvType(ConversationType.OneToOne)} OR ${ConvType.name} = ${ConvType(ConversationType.Group)})
         |   AND ${Status.name} = ${ConversationStatus.Active.value}
         |   AND ${Hidden.name} = 0
      """.stripMargin, null))

    def allConversations(implicit db: SQLiteDatabase) =
      db.rawQuery(s"SELECT *, ${ConvType.name} = ${Self.id} as is_self, ${ConvType.name} = ${Incoming.id} as is_incoming, ${Archived.name} = 1 as is_archived FROM ${table.name} WHERE ${Hidden.name} = 0 ORDER BY is_self DESC, ${HasVoice.name} DESC, is_archived ASC, is_incoming DESC, ${LastEventTime.name} DESC", null)

    import ConversationMemberData.{ConversationMemberDataDao => CM}
    import UserData.{UserDataDao => U}

    def search(prefix: SearchKey, self: UserId)(implicit db: SQLiteDatabase) = list(db.rawQuery(
      s"""SELECT DISTINCT c.*
         |  FROM ${table.name} c, ${CM.table.name} cm, ${U.table.name} u
         | WHERE cm.${CM.ConvId.name} = c.${Id.name}
         |   AND cm.${CM.UserId.name} = u.${U.Id.name}
         |   AND c.${ConvType.name} = ${ConvType(ConversationType.Group)}
         |   AND c.${Hidden.name} = ${Hidden(false)}
         |   AND u.${U.Id.name} != '${U.Id(self)}'
         |   AND (c.${Cleared.name} < c.${LastEventTime.name} OR c.${Status.name} = ${ConversationStatus.Active.value})
         |   AND (
         |        c.${SKey.name}   LIKE '${SKey(Some(prefix))}%'
         |     OR c.${SKey.name}   LIKE '% ${SKey(Some(prefix))}%'
         |     OR u.${U.SKey.name} LIKE '${U.SKey(prefix)}%'
         |     OR u.${U.SKey.name} LIKE '% ${U.SKey(prefix)}%'
         |   )
       """.stripMargin, null))
  }
}

object ConversationMemberData {

  implicit object ConversationMemberDataDao extends Dao2[ConversationMemberData, UserId, ConvId] {
    val UserId = id[UserId]('user_id).apply(_.userId)
    val ConvId = id[ConvId]('conv_id).apply(_.convId)
    val Active = bool('active)(_.active)
    val EventId = eid('event_id).apply(_.eventId)

    override val idCol = (UserId, ConvId)
    override val table = Table("ConversationMembers", UserId, ConvId, Active, EventId)
    override def apply(implicit cursor: Cursor): ConversationMemberData = ConversationMemberData(UserId, ConvId, Active, EventId)

    override def onCreate(db: SQLiteDatabase): Unit = {
      super.onCreate(db)
      db.execSQL(s"CREATE INDEX IF NOT EXISTS ConversationMembers_conv on ConversationMembers (${ConvId.name})")
    }

    def listForConv(convId: ConvId)(implicit db: SQLiteDatabase) = list(find(ConvId, convId))
    def listForUser(userId: UserId)(implicit db: SQLiteDatabase) = list(find(UserId, userId))
    def findForConv(convId: ConvId)(implicit db: SQLiteDatabase) = iterating(find(ConvId, convId))
    def findForUser(userId: UserId)(implicit db: SQLiteDatabase) = iterating(find(UserId, userId))

    def findActiveForConv(convId: ConvId)(implicit db: SQLiteDatabase) = iterating {
      db.query(table.name, null, s"${Active.name} = 1 AND ${ConvId.name} = '$convId'", null, null, null, null)
    }

    def findActiveForUser(userId: UserId)(implicit db: SQLiteDatabase) = iterating {
      db.query(table.name, null, s"${Active.name} = 1 AND ${UserId.name} = '$userId'", null, null, null, null)
    }

    def get(convId: ConvId, userId: UserId)(implicit db: SQLiteDatabase) = single(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} = ?", Array(convId.toString, userId.toString), null, null, null))

    def listMembers(convId: ConvId, users: Seq[UserId])(implicit db: SQLiteDatabase) = list(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} in (${users.mkString("'", "','", "'")})", Array(convId.toString), null, null, null))

    def isActiveMember(convId: ConvId, user: UserId)(implicit db: SQLiteDatabase) = single(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} = ? AND ${Active.name} = 1", Array(convId.toString, user.toString), null, null, null)).isDefined

    def deleteForConv(id: ConvId)(implicit db: SQLiteDatabase) = delete(ConvId, id)
  }
}
