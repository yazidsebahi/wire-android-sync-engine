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
import com.waz.ZLog._
import com.waz.api.{ContentSearchQuery, Message}
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.model.MessageData.MessageDataDao
import org.threeten.bp.Instant

case class MessageContentIndexEntry(messageId: MessageId, convId: ConvId, content: String, time: Instant)

object MessageContentIndexDao extends Dao[MessageContentIndexEntry, MessageId] {
  import MessageContentIndex._
  private implicit val logTag: LogTag = "MessageContentIndex"

  val MessageId = id[MessageId]('message_id).apply(_.messageId)
  val Conv = id[ConvId]('conv_id).apply(_.convId)
  val Content = text('content)(_.content)
  val Time = timestamp('time)(_.time)

  override def onCreate(db: SQLiteDatabase) = {
    db.execSQL(table.createFtsSql)
  }
  override val idCol = MessageId
  override val table = Table("MessageContentIndex", MessageId, Conv, Content, Time)

  override def apply(implicit c: Cursor): MessageContentIndexEntry =
    MessageContentIndexEntry(MessageId, Conv, Content, Time)

  private val IndexColumns = Array(MessageId.name, Time.name)

  def findContent(contentSearchQuery: ContentSearchQuery, convId: Option[ConvId])(implicit db: SQLiteDatabase): Cursor ={
    if (UsingFTS) {
      findContentFts(contentSearchQuery.toFtsQuery, convId)
    } else {
      findContentSimple(contentSearchQuery.elements, convId)
    }
  }

  def findContentFts(queryText: String, convId: Option[ConvId])(implicit db: SQLiteDatabase): Cursor ={
    convId match {
      case Some(conv) =>
        db.query(table.name, IndexColumns, s"${Conv.name} = '$conv' AND ${Content.name} MATCH '$queryText'", null, null, null, s"${Time.name} DESC", SearchLimit)
      case _ =>
        db.query(table.name, IndexColumns, s"${Content.name} MATCH '$queryText'", null, null, null, s"${Time.name} DESC", SearchLimit)
    }
  }

  def findContentSimple(queries: Set[String], convId: Option[ConvId])(implicit db: SQLiteDatabase): Cursor ={
    val likeQuery = queries.map(q => s"${Content.name} LIKE '%$q%'").mkString("(", " AND ", ")")
    convId match {
      case Some(conv) =>
        db.query(table.name, IndexColumns, s"${Conv.name} = '$conv' AND $likeQuery", null, null, null, s"${Time.name} DESC", SearchLimit)
      case _ =>
        db.query(table.name, IndexColumns, s"$likeQuery", null, null, null, s"${Time.name} DESC", SearchLimit)
    }
  }

  def addMessages(added: Seq[MessageData])(implicit db: SQLiteDatabase): Unit ={
    insertOrReplace(added.map(messageData => MessageContentIndexEntry(messageData.id, messageData.convId, normalizeContent(messageData.contentString), messageData.time)))
  }

  def removeMessages(removed: Seq[MessageId])(implicit db: SQLiteDatabase): Unit ={
    deleteEvery(removed)
  }

  def updateOldMessages()(implicit db: SQLiteDatabase): Boolean ={
    val queryString =
      s"SELECT ${MessageDataDao.table.name}.* FROM ${MessageDataDao.table.name} " +
        s"WHERE " +
        TextMessageTypes.map(t => s"${MessageDataDao.table.name}.${MessageDataDao.Type.name} = '${MessageDataDao.Type(t)}'").mkString("(", "  OR ", ")") +
        s"AND ${MessageDataDao.table.name}.${MessageDataDao.Id.name} NOT IN " +
        s"(SELECT ${MessageId.name} FROM ${table.name}) " +
        s"ORDER BY ${Time.name} DESC " +
        s"LIMIT 200"
    val cursor = db.rawQuery(queryString, null)
    MessageDataDao.iterating(cursor).acquire(msgs => addMessages(msgs.toSeq))
    cursor.getCount == 0
  }

  private def normalizeContent(text: String): String = {
    ContentSearchQuery.transliterated(text)
  }

}

object MessageContentIndex {
  val MaxSearchResults = 1024 // don't want to read whole db on common search query
  val SearchLimit = MaxSearchResults.toString
  val UsingFTS = true
  val TextMessageTypes = Set(Message.Type.TEXT, Message.Type.TEXT_EMOJI_ONLY, Message.Type.RICH_MEDIA)
}
