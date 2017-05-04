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
package com.waz.service.call

import com.waz.api.KindOfCallingEvent
import com.waz.content.ZmsDatabase
import com.waz.model.CallLogEntry.CallLogEntryDao
import com.waz.model.{CallLogEntry, CallSessionId, ConvId}
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.events.EventStream
import com.waz.ZLog._
import org.threeten.bp.Instant.now

import scala.concurrent.Future
import scala.util.Success

trait CallLogService {
  def addEstablishedCall(session: Option[CallSessionId], conv: ConvId, isVideo: Boolean): Future[Unit]
}

class DefaultCallLogService(storage: ZmsDatabase) extends CallLogService {
  import Threading.Implicits.Background

  val callLogEntryAdded = EventStream[CallLogEntry]

  override def addEstablishedCall(session: Option[CallSessionId], conv: ConvId, isVideo: Boolean): Future[Unit] = {
    val entry = CallLogEntry(KindOfCallingEvent.CALL_ESTABLISHED, session, conv, now, isVideo)
    storage(CallLogEntryDao.insertOrReplace(entry)(_)).future.andThen { case Success(e) => callLogEntryAdded ! e }.recoverWithLog()(logTagFor[DefaultCallLogService])
  }

  def numberOfEstablishedVoiceCalls: Future[Int] = storage.read(CallLogEntryDao.countEstablished(false)(_))
  def numberOfEstablishedVideoCalls: Future[Int] = storage.read(CallLogEntryDao.countEstablished(true)(_))
}
