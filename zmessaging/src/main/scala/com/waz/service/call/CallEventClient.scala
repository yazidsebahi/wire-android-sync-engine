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

import com.waz.HockeyApp
import com.waz.ZLog.{error, verbose, warn}
import com.waz.api.Message
import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.ErrorResponse.internalError
import com.waz.model.{ConvId, ConversationData, MessageData, MessageId}
import com.waz.service.{NetworkModeService, Timeouts}
import com.waz.sync.SyncResult
import com.waz.sync.queue.ConvLock
import com.waz.threading.CancellableFuture.CancelException
import com.waz.znet.ZNetClient
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.Future.successful
import com.waz.utils.RichInstant
import com.waz.utils.RichFiniteDuration
import com.waz.utils.events.EventStream

class CallEventClient(zNetClient: ZNetClient, network: NetworkModeService, timeouts: Timeouts) {

  val messageToSend = EventStream[String]()

//  def postCallEvent(convId: ConvId): Future[SyncResult] = {

//    def shouldGiveUpSending(msg: MessageData) = network.isOfflineMode || timeouts.messages.sendingTimeout.elapsedSince(msg.time)
//
//    storage.getMessage(id) flatMap { message =>
//      message.fold(successful(None: Option[ConversationData]))(msg => convs.convById(msg.convId)) map { conv =>
//        (message, conv)
//      }
//    } flatMap {
//      case (Some(msg), Some(conv)) =>
//        postCallEvent(conv, msg, editTime)
//          .recover {
//            case c: CancelException =>
//              SyncResult(ErrorResponse.Cancelled)
//            case e: Throwable =>
//              error(s"postMessage($conv, $msg) failed", e)
//              SyncResult.Failure(Some(internalError(e.getMessage)), shouldRetry = false)
//          }
//          .flatMap {
//            case SyncResult.Success => service.messageSent(conv.id, msg) map (_ => SyncResult.Success)
//            case res@SyncResult.Failure(Some(ErrorResponse.Cancelled), _) =>
//              verbose(s"postMessage($msg) was cancelled")
//              msgContent.updateMessage(id)(_.copy(state = Message.Status.FAILED_READ)) map { _ => res }
//            case res@SyncResult.Failure(error, shouldRetry) =>
//              val shouldGiveUp = shouldGiveUpSending(msg)
//              warn(s"postMessage failed with res: $res, shouldRetry: $shouldRetry, shouldGiveUp: $shouldGiveUp, offline: ${network.isOfflineMode}, msg.localTime: ${msg.localTime}")
//              if (!shouldRetry || shouldGiveUp)
//                service.messageDeliveryFailed(conv.id, msg, error.getOrElse(internalError(s"shouldRetry: $shouldRetry, shouldGiveUp: $shouldGiveUp, offline: ${network.isOfflineMode}"))).map(_ => SyncResult.Failure(error, shouldRetry = false))
//              else successful(res)
//
//          }
//      case (Some(msg), None) =>
//        HockeyApp.saveException(new Exception("postMessage failed, couldn't find conversation for msg"), s"msg: $msg")
//        service.messageDeliveryFailed(msg.convId, msg, internalError("conversation not found")) map (_ => SyncResult.aborted())
//
//      case (msg, conv) =>
//        HockeyApp.saveException(new Exception("postMessage failed, couldn't find a message nor conversation"), s"msg id: $id")
//        successful(SyncResult.aborted())
//    }
//  }

}
