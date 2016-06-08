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
package com.waz.sync.otr

import java.util.Date

import com.waz.ZLog._
import com.waz.api.Verification
import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.ErrorResponse.internalError
import com.waz.cache.{CacheService, LocalData}
import com.waz.content.ConversationStorage
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.assets.AssetService
import com.waz.service.conversation.ConversationsService
import com.waz.service.messages.MessagesService
import com.waz.service.otr.OtrService
import com.waz.service.{ErrorsService, UserService}
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient.{OtrAssetMetadata, OtrAssetResponse}
import com.waz.sync.client.MessagesClient.OtrMessage
import com.waz.sync.client.OtrClient.{ClientMismatch, EncryptedContent, MessageResponse}
import com.waz.sync.client.{AssetClient, MessagesClient, OtrClient}
import com.waz.threading.CancellableFuture
import com.waz.utils._
import com.waz.utils.crypto.AESUtils
import com.waz.znet.ZNetClient.ErrorOrResponse

import scala.concurrent.Future.successful
import scala.concurrent.{Future, Promise}

class OtrSyncHandler(client: OtrClient, msgClient: MessagesClient, assetClient: AssetClient, service: OtrService, assets: AssetService,
                     convs: ConversationsService, convStorage: ConversationStorage, users: UserService, messages: MessagesService,
                     errors: ErrorsService, clientsSyncHandler: OtrClientsSyncHandler, cache: CacheService) {
  import OtrSyncHandler._
  import com.waz.threading.Threading.Implicits.Background

  def postOtrMessage(conv: ConversationData, message: GenericMessage): Future[Either[ErrorResponse, Date]] =
    postOtrMessage(conv.id, conv.remoteId, message)

  def postOtrMessage(convId: ConvId, remoteId: RConvId, message: GenericMessage): Future[Either[ErrorResponse, Date]] =
    service.clients.getSelfClient flatMap {
      case Some(otrClient) =>
        postEncryptedMessage(convId, message) {
          case (content, retry) if content.estimatedSize < MaxContentSize => msgClient.postMessage(remoteId, OtrMessage(otrClient.id, content), ignoreMissing(retry))
          case (content, retry) =>
            verbose(s"Message content too big, will post as External. Estimated size: ${content.estimatedSize}")
            postExternalMessage(otrClient.id, convId, remoteId, message)
        }
      case None =>
        successful(Left(internalError("Client is not registered")))
    }

  // will retry 3 times, at first we try to send message in normal way,
  // when it fails we will try sending empty messages to contacts for which we can not encrypt the message
  // in last try we will use 'ignore_missing' flag
  private def postEncryptedMessage(convId: ConvId, message: GenericMessage, retry: Int = 0, previous: EncryptedContent = EncryptedContent.Empty)(f: (EncryptedContent, Int) => ErrorOrResponse[MessageResponse]): Future[Either[ErrorResponse, Date]] =
    convStorage.get(convId) flatMap {
      case Some(conv) if conv.verified == Verification.UNVERIFIED =>
        // refusing to send messages to 'degraded' conversation, UI should show error and ask user to verify devices (or ignore it - which will change state to UNKNOWN)
        errors.addConvUnverifiedError(convId, MessageId(message.messageId)) map { _ => Left(ErrorResponse.Unverified) }
      case _ =>
        service.encryptMessage(convId, message, retry > 0, previous) flatMap { content =>
          f(content, retry).future flatMap {
            case Right(MessageResponse.Success(ClientMismatch(redundant, _, deleted, time))) =>
              // XXX: we are ignoring redundant clients, we rely on members list to encrypt messages, so if user left the conv then we won't use his clients on next message
              service.deleteClients(deleted) map { _ => Right(time) }
            case Right(MessageResponse.Failure(ClientMismatch(redundant, missing, deleted, _))) =>
              service.deleteClients(deleted) flatMap { _ =>
                if (retry > 2)
                  successful(Left(internalError(s"postEncryptedMessage failed with missing clients after several retries: $missing")))
                else
                  clientsSyncHandler.syncSessions(missing) flatMap {
                    case None =>
                      // XXX: encrypt relies on conv members list, we only add clients for users in conv,
                      // if members list is broken then we will always end up with missing clients,
                      // maybe we should update members list in this place ???
                      postEncryptedMessage(convId, message, retry + 1, content)(f)
                    case Some(err) if retry < 3 =>
                      error(s"syncSessions for missing clients failed: $err")
                      postEncryptedMessage(convId, message, retry + 1, content)(f)
                    case Some(err) =>
                      successful(Left(err))
                  }
              }
            case Left(err) =>
              error(s"postOtrMessage failed with error: $err")
              successful(Left(err))
          }
        }
    }

  private def ignoreMissing(retry: Int) = retry > 1

  private def postExternalMessage(clientId: ClientId, convId: ConvId, remoteId: RConvId, message: GenericMessage): ErrorOrResponse[MessageResponse] = {
    val key = AESKey()
    val (sha, data) = AESUtils.encrypt(key, GenericMessage.toByteArray(message))

    CancellableFuture.lift {
      postEncryptedMessage(convId, GenericMessage(Uid(message.messageId), Proto.External(key, sha))) { (content, retry) =>
        msgClient.postMessage(remoteId, OtrMessage(clientId, content, Some(data)), ignoreMissing(retry))
      } map { // that's a bit of a hack, but should be harmless
        case Right(time) => Right(MessageResponse.Success(ClientMismatch(Map.empty, Map.empty, Map.empty, time)))
        case Left(err) => Left(err)
      }
    }
  }

  def postOtrImageData(conv: ConversationData, assetId: AssetId, asset: ImageData, data: LocalData, nativePush: Boolean = true): Future[Either[ErrorResponse, Date]] = {
    val key = asset.otrKey.getOrElse(AESKey())
    def message(sha: Sha256) = GenericMessage(Uid(assetId.str), Proto.ImageAsset(asset.tag, asset.width, asset.height, asset.origWidth, asset.origHeight, asset.mime, asset.size, Some(key), Some(sha)))
    postAssetData(conv, assetId, key, message, data, nativePush).future flatMap {
      case Right((AssetKey(id, _, sha), time)) =>
        val updated = asset.copy(remoteId = Some(id), otrKey = Some(key), sha256 = Some(sha), sent = true)
        cache.addStream(updated.cacheKey, data.inputStream) flatMap { _ =>
          assets.updateImageAsset(assetId, conv.remoteId, updated) .map { data =>
            verbose(s"OTR image uploaded: $data")
            Right(time)
          }
        }
      case Left(err) => Future successful Left(err)
    }
  }

  def postAssetData(conv: ConversationData, assetId: AssetId, key: AESKey, createMsg: Sha256 => GenericMessage, data: LocalData, nativePush: Boolean = true): CancellableFuture[Either[ErrorResponse, (AssetKey, Date)]] = {
    val promise = Promise[Either[ErrorResponse,(AssetKey, Date)]]()

    val future = service.clients.getSelfClient flatMap {
      case Some(otrClient) =>
        service.encryptAssetData(assetId, key, data) flatMap { case (sha, encrypted) =>
          val inline = encrypted.length < MaxInlineSize
          var imageId = Option.empty[RAssetDataId]
          var assetKey = Option.empty[AssetKey]
          val message = createMsg(sha)
          postEncryptedMessage(conv.id, message) { (content, retry) =>
            val meta = new OtrAssetMetadata(otrClient.id, content, nativePush, inline)
            val upload = imageId match {
              case Some(imId) if !inline =>
                // asset data has already been uploaded on previous try and we don't need to send it inline, will only resend metadata
                // see https://github.com/wearezeta/backend-api-docs/wiki/API-Conversations-Assets#upload-otr-retry
                assetClient.postOtrAssetMetadata(imId, conv.remoteId, meta, ignoreMissing(retry)).map {
                  case Right(OtrAssetResponse(id, msgResp)) => Right(msgResp)
                  case Left(error) => Left(error)
                }
              case _ =>
                assetClient.postOtrAsset(conv.remoteId, meta, encrypted, ignoreMissing(retry)) map {
                  case Right(OtrAssetResponse(id, msgResponse)) =>
                    imageId = Some(id)
                    assetKey = Some(AssetKey(id, key, sha))
                    Right(msgResponse)
                  case Left(err) =>
                    Left(err)
                }
            }
            promise.future.onComplete { _ => upload.cancel() }
            upload
          } map {
            case Right(time) => assetKey.fold2(Left(internalError("assetKey is empty, but upload was successful")), k => Right((k, time)))
            case Left(err) => Left(err)
          }
        }
      case None =>
        successful(Left(internalError("Client is not registered")))
    }

    promise.tryCompleteWith(future)
    new CancellableFuture(promise)
  }

  def postSessionReset(convId: ConvId, user: UserId, client: ClientId) = {

    val msg = GenericMessage(Uid(), Proto.ClientAction.SessionReset)

    val convData = convStorage.get(convId) flatMap {
      case None => convStorage.get(ConvId(user.str))
      case conv => successful(conv)
    }

    def msgContent = service.encryptTargetedMessage(user, client, msg) flatMap {
      case Some(ct) => successful(Some(ct))
      case None =>
        clientsSyncHandler.syncSessions(Map(user -> Seq(client))) flatMap { _ =>
          service.encryptTargetedMessage(user, client, msg)
        }
    }

    convData flatMap {
      case None => successful(SyncResult(internalError(s"conv not found: $convId, for user: $user in postSessionReset")))
      case Some(conv) =>
        service.clients.getSelfClient flatMap {
          case None => successful(SyncResult(internalError(s"client not registered")))
          case Some(otrClient) =>
            msgContent flatMap {
              case None => successful(SyncResult(internalError(s"session not found for $user, $client")))
              case Some(content) =>
                msgClient.postMessage(conv.remoteId, OtrMessage(otrClient.id, content), ignoreMissing = true).future map {
                  case Right(_) => SyncResult.Success
                  case Left(err) => SyncResult(err)
                }
            }
        }
    }
  }
}

object OtrSyncHandler {
  private implicit val tag: LogTag = logTagFor[OtrSyncHandler]

  val MaxInlineSize = 10 * 1024
  val MaxContentSize = 256 * 1024  // backend accepts 256KB for otr messages, but we would prefer to send less
}
