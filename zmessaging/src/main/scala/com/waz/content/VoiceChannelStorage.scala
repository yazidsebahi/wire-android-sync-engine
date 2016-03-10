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
package com.waz.content

import android.content.Context
import com.waz.ZLog._
import com.waz.model.VoiceParticipantData.VoiceParticipantDataDao
import com.waz.model._
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.ThrottledProcessingQueue

import scala.collection.mutable

class VoiceChannelStorage(context: Context, storage: ZStorage) {
  import com.waz.content.VoiceChannelStorage._
  private implicit val logTag: LogTag = logTagFor[VoiceChannelStorage]
  implicit val dispatcher = new SerialDispatchQueue(name = "VoiceChannelStorage")

  val participants = new mutable.HashMap[ConvId, Seq[VoiceParticipantData]] // FIXME: this is not thread safe, and we are accessing it from multiple threads

  val participantsSaveQueue =  new ThrottledProcessingQueue[ConvId](SaveThrottling, { ids =>
    storage.withTransaction { implicit db =>
      ids.toSet foreach { (id: ConvId) =>
        VoiceParticipantDataDao.deleteForChannel(id)
        participants.get(id) foreach VoiceParticipantDataDao.insertOrReplace
      }
    }
  })

  def listParticipants(id: ConvId) =
    participants.get(id).fold {
      storage { VoiceParticipantDataDao.listForChannel(id)(_) } map { ps =>
         participants.getOrElseUpdate(id, ps)
      }
    } { res =>
      CancellableFuture.successful(res)
    }

  def setParticipants(id: ConvId, ps: Seq[VoiceParticipantData]) = {
    participants(id) = ps
    participantsSaveQueue ! id
  }
}

object VoiceChannelStorage {
  import scala.concurrent.duration._
  val SaveThrottling = 1.second
}
