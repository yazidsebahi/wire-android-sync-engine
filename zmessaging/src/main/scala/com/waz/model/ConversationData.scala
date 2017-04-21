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
import com.waz.api.{EphemeralExpiration, IConversation, Verification}
import com.waz.db.Col._
import com.waz.db.{Dao, Dao2}
import com.waz.model.ConversationData.{ConversationStatus, ConversationType}
import com.waz.service.SearchKey
import com.waz.utils.{JsonDecoder, JsonEncoder, _}
import org.json.JSONObject
import org.threeten.bp.Instant

case class ConversationData(id:                   ConvId,
                            remoteId:             RConvId,
                            name:                 Option[String],
                            creator:              UserId,
                            convType:             ConversationType,
                            lastEventTime:        Instant = Instant.EPOCH,
                            status:               Option[ConversationStatus] = None,
                            lastRead:             Instant = Instant.EPOCH,
                            muted:                Boolean = false,
                            muteTime:             Instant = Instant.EPOCH,
                            archived:             Boolean = false,
                            archiveTime:          Instant = Instant.EPOCH,
                            cleared:              Instant = Instant.EPOCH,
                            generatedName:        String = "",
                            searchKey:            Option[SearchKey] = None,
                            unreadCount:          Int = 0,
                            failedCount:          Int = 0,
                            hasVoice:             Boolean = false,
                            unjoinedCall:         Boolean = false,
                            missedCallMessage:    Option[MessageId] = None,
                            incomingKnockMessage: Option[MessageId] = None,
                            renameEvent:          Instant = Instant.EPOCH,
                            voiceMuted:           Boolean = false,
                            hidden:               Boolean = false,
                            verified:             Verification = Verification.UNKNOWN,
                            ephemeral:            EphemeralExpiration = EphemeralExpiration.NONE) {

  def activeMember = status.contains(ConversationStatus.Active)

  def displayName = if (convType == ConversationType.Group) name.getOrElse(generatedName) else generatedName

  def withFreshSearchKey = copy(searchKey = freshSearchKey)
  def savedOrFreshSearchKey = searchKey.orElse(freshSearchKey)
  def freshSearchKey = if (convType == ConversationType.Group) name map SearchKey else None

  lazy val completelyCleared = ! cleared.isBefore(lastEventTime)

  def withLastRead(time: Instant) = copy(lastRead = lastRead max time)

  def withCleared(time: Instant) = copy(cleared = cleared max time)

  def updated(d: ConversationData): Option[ConversationData] = {
    val ct = if (ConversationType.isOneToOne(convType) && d.convType != ConversationType.OneToOne) convType else d.convType
    val st = if (d.status.isDefined) d.status else status
    val nameSource = if (d.renameEvent.isAfter(renameEvent)) d else this

    val updated = copy(remoteId = d.remoteId, name = nameSource.name, creator = d.creator, convType = ct,
      lastEventTime = lastEventTime max d.lastEventTime, status = st,
      lastRead = lastRead max d.lastRead, muted = d.muted,
      muteTime = d.muteTime, archived = d.archived, cleared = cleared max d.cleared,
      renameEvent = nameSource.renameEvent, searchKey = nameSource.searchKey)

    if (updated == this) None else Some(updated)
  }
}

/**
 * Conversation user binding.
 */
case class ConversationMemberData(userId: UserId, convId: ConvId)

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

    def apply(value: Int): ConversationStatus = if (value == 0) Active else Inactive
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
      lastEventTime = decodeInstant('lastEventTime),
      status = decodeOptInt('status).map(ConversationStatus(_)),
      lastRead = decodeInstant('lastReadTime),
      muted = 'muted, muteTime = decodeInstant('muteTime), archived = 'archived,
      archiveTime = decodeInstant('archiveTime), cleared = decodeInstant('cleared),
      generatedName = 'generatedName, searchKey = decodeOptString('name) map SearchKey,
      unreadCount = 'unreadCount, failedCount = 'failedCount, hasVoice = 'hasVoice, unjoinedCall = 'unjoinedCall,
      missedCallMessage = decodeOptMessageId('missedCallMessage), incomingKnockMessage = decodeOptMessageId('incomingKnockMessage),
      renameEvent = decodeInstant('renameEventTime), voiceMuted = 'voiceMuted, hidden = 'hidden,
      verified = decodeOptString('verified).fold(Verification.UNKNOWN)(Verification.valueOf),
      ephemeral = EphemeralExpiration.getForMillis(decodeLong('ephemeral))
    )
  }

  implicit lazy val Encoder: JsonEncoder[ConversationData] = new JsonEncoder[ConversationData] {
    override def apply(v: ConversationData): JSONObject = JsonEncoder { o =>
      o.put("id", v.id.str)
      o.put("remoteId", v.remoteId.str)
      v.name foreach (o.put("name", _))
      o.put("creator", v.creator.str)
      o.put("convType", v.convType.id)
      o.put("lastEventTime", v.lastEventTime.toEpochMilli)
      v.status.foreach(status => o.put("status", status.value))
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
      o.put("renameEventTime", v.renameEvent.toEpochMilli)
      o.put("voiceMuted", v.voiceMuted)
      o.put("hidden", v.hidden)
      o.put("trusted", v.verified)
      o.put("ephemeral", v.ephemeral.milliseconds)
    }
  }

  implicit object ConversationDataDao extends Dao[ConversationData, ConvId] {
    val Id            = id[ConvId]('_id, "PRIMARY KEY").apply(_.id)
    val RemoteId      = id[RConvId]('remote_id).apply(_.remoteId)
    val Name          = opt(text('name))(_.name.filterNot(_.isEmpty))
    val Creator       = id[UserId]('creator).apply(_.creator)
    val ConvType      = int[ConversationType]('conv_type, _.id, ConversationType(_))(_.convType)
    val LastEventTime = timestamp('last_event_time)(_.lastEventTime)
    val Status        = opt(int[ConversationStatus]('status, _.value, ConversationStatus(_)))(_.status)
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
    val RenameEvent   = timestamp('rename_event_time)(_.renameEvent)
    val VoiceMuted    = bool('voice_muted)(_.voiceMuted)
    val Hidden        = bool('hidden)(_.hidden)
    val Verified      = text[Verification]('verified, _.name, Verification.valueOf)(_.verified)
    val Ephemeral     = long[EphemeralExpiration]('ephemeral, _.milliseconds, EphemeralExpiration.getForMillis)(_.ephemeral)

    override val idCol = Id
    override val table = Table("Conversations", Id, RemoteId, Name, Creator, ConvType, LastEventTime, Status, LastRead, Muted, MutedTime, Archived, ArchivedTime, Cleared, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, VoiceMuted, Hidden, MissedCall, IncomingKnock, RenameEvent, UnjoinedCall, Verified, Ephemeral)
    override def apply(implicit cursor: Cursor): ConversationData = ConversationData(Id, RemoteId, Name, Creator, ConvType, LastEventTime, Status, LastRead, Muted, MutedTime, Archived, ArchivedTime, Cleared, GeneratedName, SKey, UnreadCount, FailedCount, HasVoice, UnjoinedCall, MissedCall, IncomingKnock, RenameEvent, VoiceMuted, Hidden, Verified, Ephemeral)

    import com.waz.model.ConversationData.ConversationType._

    override def onCreate(db: SQLiteDatabase): Unit = {
      super.onCreate(db)
      db.execSQL(s"CREATE INDEX IF NOT EXISTS Conversation_search_key on Conversations (${SKey.name})")
    }

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

    def search(prefix: SearchKey, self: UserId, handleOnly: Boolean)(implicit db: SQLiteDatabase) ={
      val select =
        s"""SELECT DISTINCT c.*
            |  FROM ${table.name} c, ${CM.table.name} cm, ${U.table.name} u
            | WHERE cm.${CM.ConvId.name} = c.${Id.name}
            |   AND cm.${CM.UserId.name} = u.${U.Id.name}
            |   AND c.${ConvType.name} = ${ConvType(ConversationType.Group)}
            |   AND c.${Hidden.name} = ${Hidden(false)}
            |   AND u.${U.Id.name} != '${U.Id(self)}'
            |   AND (c.${Cleared.name} < c.${LastEventTime.name} OR c.${Status.name} = ${ConversationStatus.Active.value})""".stripMargin
      val handleCondition =
        if (handleOnly){
          s"""AND u.${U.Handle.name} LIKE '%${prefix.asciiRepresentation}%'""".stripMargin
        } else {
          s"""AND (    c.${SKey.name}   LIKE '${SKey(Some(prefix))}%'
              |     OR c.${SKey.name}   LIKE '% ${SKey(Some(prefix))}%'
              |     OR u.${U.SKey.name} LIKE '${U.SKey(prefix)}%'
              |     OR u.${U.SKey.name} LIKE '% ${U.SKey(prefix)}%'
              |     OR u.${U.Handle.name} LIKE '%${prefix.asciiRepresentation}%')""".stripMargin
        }
      list(db.rawQuery(select + " " + handleCondition, null))
    }
  }
}

object ConversationMemberData {

  implicit object ConversationMemberDataDao extends Dao2[ConversationMemberData, UserId, ConvId] {
    val UserId = id[UserId]('user_id).apply(_.userId)
    val ConvId = id[ConvId]('conv_id).apply(_.convId)

    override val idCol = (UserId, ConvId)
    override val table = Table("ConversationMembers", UserId, ConvId)
    override def apply(implicit cursor: Cursor): ConversationMemberData = ConversationMemberData(UserId, ConvId)

    override def onCreate(db: SQLiteDatabase): Unit = {
      super.onCreate(db)
      db.execSQL(s"CREATE INDEX IF NOT EXISTS ConversationMembers_conv on ConversationMembers (${ConvId.name})")
      db.execSQL(s"CREATE INDEX IF NOT EXISTS ConversationMembers_userid on ConversationMembers (${UserId.name})")
    }

    def listForConv(convId: ConvId)(implicit db: SQLiteDatabase) = list(find(ConvId, convId))
    def listForUser(userId: UserId)(implicit db: SQLiteDatabase) = list(find(UserId, userId))
    def findForConv(convId: ConvId)(implicit db: SQLiteDatabase) = iterating(find(ConvId, convId))
    def findForUser(userId: UserId)(implicit db: SQLiteDatabase) = iterating(find(UserId, userId))

    def get(convId: ConvId, userId: UserId)(implicit db: SQLiteDatabase) = single(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} = ?", Array(convId.toString, userId.toString), null, null, null))

    def listMembers(convId: ConvId, users: Seq[UserId])(implicit db: SQLiteDatabase) = list(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} in (${users.mkString("'", "','", "'")})", Array(convId.toString), null, null, null))

    def isActiveMember(convId: ConvId, user: UserId)(implicit db: SQLiteDatabase) = single(db.query(table.name, null, s"${ConvId.name} = ? AND ${UserId.name} = ?", Array(convId.toString, user.toString), null, null, null)).isDefined

    def deleteForConv(id: ConvId)(implicit db: SQLiteDatabase) = delete(ConvId, id)
  }
}
