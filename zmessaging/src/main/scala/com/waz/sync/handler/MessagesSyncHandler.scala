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
package com.waz.sync.handler

import android.content.Context
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.api.Message
import com.waz.api.impl.ErrorResponse
import com.waz.content.MessagesStorage
import com.waz.model.GenericMessage.Knock
import com.waz.model._
import com.waz.model.messages.TextMessage
import com.waz.service.conversation.{ConversationEventsService, ConversationsContentUpdater, ConversationsService}
import com.waz.service.images.ImageAssetService
import com.waz.service.messages.{MessagesContentUpdater, MessagesService}
import com.waz.service.otr.OtrService
import com.waz.service.{MetaDataService, NetworkModeService, PreferenceService}
import com.waz.sync.client.MessagesClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.sync.queue.ConvLock
import com.waz.sync.{SyncResult, SyncServiceHandle}
import com.waz.utils._
import com.waz.znet.Response.Status
import com.waz.znet.ZNetClient.ErrorOr
import org.threeten.bp.Instant

import scala.concurrent.Future

class MessagesSyncHandler(context: Context, service: MessagesService, msgContent: MessagesContentUpdater, convEvents: ConversationEventsService, client: MessagesClient,
                          otr: OtrService, otrSync: OtrSyncHandler, convs: ConversationsContentUpdater, storage: MessagesStorage,
                          assetService: ImageAssetService, assetSync: ImageAssetSyncHandler, network: NetworkModeService,
                          metadata: MetaDataService, prefs: PreferenceService, sync: SyncServiceHandle) {

  import com.waz.threading.Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[MessagesSyncHandler]

  def postRequestDropped(convId: ConvId, id: MessageId) = storage.update(id) { msg =>
    if (msg.isLocal) msg.copy(state = Message.Status.FAILED) else msg
  }

  def postMessage(convId: ConvId, id: MessageId)(implicit convLock: ConvLock): Future[SyncResult] = {
    def isPartiallySent(msg: MessageData) =
      msg.assetId.fold(Future.successful(false)) { assetId =>
        assetService.getImageAsset(assetId) map { _.exists(_.versions.exists(_.sent)) }
      }

    def shouldGiveUpSending(msg: MessageData) = isPartiallySent(msg) map { partiallySent =>
      !partiallySent && (network.isOfflineMode || ConversationsService.SendingTimeout.elapsedSince(msg.time))
    }

    storage.getMessage(id) flatMap { message =>
      message.fold(Future.successful(None: Option[ConversationData]))(msg => convs.convById(msg.convId)) map { conv =>
        (message, conv)
      }
    } flatMap {
      case (Some(msg), Some(conv)) =>
        postMessage(conv, msg)
          .recover {
            case e: Throwable =>
              error(s"postMessage($conv, $msg) failed", e)
              SyncResult.Failure(Some(ErrorResponse.internalError(e.getMessage)), shouldRetry = false)
          }
          .flatMap {
            case SyncResult.Success => service.messageSent(conv.id, msg.id) map (_ => SyncResult.Success )
            case res @ SyncResult.Failure(error, shouldRetry) =>
              shouldGiveUpSending(msg) flatMap { shouldGiveUp =>
                warn(s"postMessage failed with res: $res, shouldRetry: $shouldRetry, shouldGiveUp: $shouldGiveUp, offline: ${network.isOfflineMode}, msg.localTime: ${msg.localTime}")
                if (!shouldRetry || shouldGiveUp) service.messageDeliveryFailed(conv.id, msg.id) map (_ => SyncResult.Failure(error, shouldRetry = false))
                else Future.successful(res)
              }
          }
      case (Some(msg), None) =>
        HockeyApp.saveException(new Exception("postMessage failed, couldn't find conversation for msg"), s"msg: $msg")
        service.messageDeliveryFailed(msg.convId, msg.id) map (_ => SyncResult.failed())

      case (msg, conv) =>
        HockeyApp.saveException(new Exception("postMessage failed, couldn't find a message nor conversation"), s"msg id: $id")
        Future.successful(SyncResult.failed())
    }
  }

  private def postMessage(conv: ConversationData, msg: MessageData)(implicit convLock: ConvLock): Future[SyncResult] = {

    def postAssetMessage() = msg.assetId match {
      case Some(assetId) =>
        assetSync.withImageAsset(conv.remoteId, assetId) { asset =>
          Future.traverse(asset.versions) { im =>
            assetSync.postOtrImageData(conv.remoteId, assetId, im) map {
              case Right(time) =>
                convLock.release()
                Right(time)
              case res => res
            }
          } map { results =>
            results.find(_.isLeft).getOrElse(results.head)
          }
        }
      case _ =>
        Future.successful(Left(ErrorResponse.internalError(s"MessageEntry has no assetId: $msg")))
    }

    def post: ErrorOr[Instant] = msg.msgType match {
      case Message.Type.ASSET => postAssetMessage().map(_.right.map(_.fold(msg.time)(_.instant)))
      case Message.Type.KNOCK => otrSync.postOtrMessage(conv, GenericMessage(msg.id, new Knock(msg.hotKnock))).map(_.right.map(_.instant))
      case Message.Type.TEXT | Message.Type.RICH_MEDIA => otrSync.postOtrMessage(conv, TextMessage(msg)).map(_.right.map(_.instant))
      case tpe => Future.successful(Left(ErrorResponse.internalError(s"Unsupported message type in postOtrMessage: $tpe")))
    }

    post flatMap {
      case Right(time) =>
        verbose(s"postOtrMessage($msg) successful $time")
        messageSent(conv.id, msg, time) map  { _ => SyncResult.Success }
      case Left(error @ ErrorResponse(Status.Forbidden, _, "unknown-client")) =>
        verbose(s"postOtrMessage($msg), failed: $error")
        otr.clients.onCurrentClientRemoved() map { _ => SyncResult(error) }
      case Left(error) =>
        verbose(s"postOtrMessage($msg), failed: $error")
        Future.successful(SyncResult(error))
    }
  }

  private def updateLocalTimes(conv: ConvId, prevTime: Instant, time: Instant) =
    msgContent.updateLocalMessageTimes(conv, prevTime, time) flatMap { updated =>
      val prevLastTime = updated.lastOption.fold(prevTime)(_._1.time)
      val lastTime = updated.lastOption.fold(time)(_._2.time)
      convs.storage.update(conv,
        c => if (!c.lastRead.isAfter(prevLastTime)) c.copy(lastRead = lastTime) else c
      ) flatMap {
        case Some((_, c)) => sync.postLastRead(c.id, c.lastRead)
        case None => Future.successful(())
      }
    }

  private[waz] def messageSent(convId: ConvId, msg: MessageData, time: Instant) = {
    debug(s"otrMessageSent($convId. $msg, $time)")

    for {
      _ <- updateLocalTimes(convId, msg.time, time)
      _ <- storage.update(msg.id) { m => m.copy(otr = true) }
      _ <- convs.updateLastEvent(convId, time)
    } yield ()
  }
}
