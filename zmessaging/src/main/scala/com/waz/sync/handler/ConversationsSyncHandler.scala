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

import java.util.Date

import com.waz.ZLog._
import com.waz.api.ErrorType
import com.waz.api.impl.ErrorResponse
import com.waz.content.MessagesStorage
import com.waz.model._
import com.waz.service._
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationEventsService, DefaultConversationsContentUpdater, ConversationsService}
import com.waz.service.messages.MessagesService
import com.waz.sync.SyncResult
import com.waz.sync.client.ConversationsClient
import com.waz.sync.client.ConversationsClient.ConversationResponse.ConversationsResult
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.EventContext
import com.waz.utils._

import scala.concurrent.Future

object ConversationsSyncHandler {
  val PostMembersLimit = 64
}

class ConversationsSyncHandler(assetSync: AssetSyncHandler,
                               userService: UserService, messagesStorage: MessagesStorage, messagesService: MessagesService,
                               convService: ConversationsService, convs: DefaultConversationsContentUpdater, convEvents: ConversationEventsService,
                               errorsService: ErrorsService, assetService: AssetService, conversationsClient: ConversationsClient, genericMessages: GenericMessageService) {

  import Threading.Implicits.Background
  import com.waz.sync.handler.ConversationsSyncHandler._
  private implicit val tag: LogTag = logTagFor[ConversationsSyncHandler]
  private implicit val ec = EventContext.Global

  def syncConversations(ids: Seq[ConvId]): Future[SyncResult] = {
    Future.sequence(ids.map(convs convById _)).flatMap { convs =>
      val remoteIds = convs.collect { case Some(conv) => conv.remoteId }
      if (remoteIds.size != convs.size) {
        error(s"syncConversations($ids) - some conversations were not found in local db, skipping")
      }
      conversationsClient.loadConversations(remoteIds).future flatMap {
        case Right(resps) =>
          debug(s"syncConversations received ${resps.size}")
          convService.updateConversations(resps).map(_ => SyncResult.Success)
        case Left(error) =>
          warn(s"ConversationsClient.syncConversations($ids) failed with error: $error")
          Future.successful(SyncResult(error))
      }
    }
  }

  def syncConversations(start: Option[RConvId] = None): Future[SyncResult] = {
    conversationsClient.loadConversations(start).future flatMap {
      case Right(ConversationsResult(convs, hasMore)) =>
        debug(s"syncConversations received ${convs.size}")
        val future = convService.updateConversations(convs)
        if (hasMore) syncConversations(convs.lastOption.map(_.conversation.remoteId)) flatMap { res => future.map(_ => res) }
        else future.map(_ => SyncResult.Success)
      case Left(error) =>
        warn(s"ConversationsClient.loadConversations($start) failed with error: $error")
        CancellableFuture.successful(SyncResult(error))
    }
  }

  def postConversationName(id: ConvId, name: String): Future[SyncResult] =
    postConv(id) { conv => conversationsClient.postName(conv.remoteId, name).future }

  def postConversationMemberJoin(id: ConvId, members: Seq[UserId]): Future[SyncResult] = withConversation(id) { conv =>
    def post(users: Seq[UserId]) = conversationsClient.postMemberJoin(conv.remoteId, users).future flatMap {
      case Left(resp @ ErrorResponse(403, _, "not-connected")) =>
        convService.onMemberAddFailed(id, users, ErrorType.CANNOT_ADD_UNCONNECTED_USER_TO_CONVERSATION, resp) map (_ => SyncResult.Failure(Some(resp), shouldRetry = false))
      case Left(resp @ ErrorResponse(403, _, "too-many-members")) =>
        convService.onMemberAddFailed(id, users, ErrorType.CANNOT_ADD_USER_TO_FULL_CONVERSATION, resp) map (_ => SyncResult.Failure(Some(resp), shouldRetry = false))
      case resp =>
        postConvRespHandler(resp)
    }

    Future.traverse(members.grouped(PostMembersLimit))(post) map { _.find(!_.isSuccess).getOrElse(SyncResult.Success) }
  }

  def postConversationMemberLeave(id: ConvId, user: UserId): Future[SyncResult] = userService.withSelfUserFuture { selfUserId =>
    if (user != selfUserId) postConv(id) { conv => conversationsClient.postMemberLeave(conv.remoteId, user) }
    else withConversation(id) { conv =>
      conversationsClient.postMemberLeave(conv.remoteId, user).future flatMap {
        case Right(Some(event: MemberLeaveEvent)) =>
          event.localTime = new Date
          conversationsClient.postConversationState(conv.remoteId, ConversationState(archived = Some(true), archiveTime = Some(event.time.instant))).future flatMap {
            case Right(resp) =>
              verbose(s"postConversationState finished: $resp")
              convEvents.handlePostConversationEvent(event).map(_ => SyncResult.Success)
            case Left(error) =>
              Future.successful(SyncResult(error))
          }
        case Right(None) =>
          debug(s"member $user already left, just updating the conversation state")
          conversationsClient.postConversationState(conv.remoteId, ConversationState(archived = Some(true), archiveTime = Some(conv.lastEventTime))).future map { _ => SyncResult.Success }

        case Left(error) =>
          Future.successful(SyncResult(error))
      }
    }
  }

  def postConversationState(id: ConvId, state: ConversationState): Future[SyncResult] = withConversation(id) { conv =>
    conversationsClient.postConversationState(conv.remoteId, state).future map (_.fold(SyncResult(_), SyncResult(_)))
  }

  def postConversation(convId: ConvId, users: Seq[UserId], name: Option[String]): Future[SyncResult] = {
    debug(s"postConversation($convId, $users, $name)")
    val (toCreate, toAdd) = users.splitAt(PostMembersLimit)
    conversationsClient.postConversation(toCreate, name).future.flatMap {
      case Right(response) =>
        convService.updateConversations(Seq(response.copy(conversation = response.conversation.copy(id = convId)))) flatMap { _ =>
          if (toAdd.nonEmpty) postConversationMemberJoin(convId, toAdd)
          else Future.successful(SyncResult.Success)
        }
      case Left(resp@ErrorResponse(403, msg, "not-connected")) =>
        warn(s"got error: $resp")
        errorsService.addErrorWhenActive(ErrorData(ErrorType.CANNOT_CREATE_GROUP_CONVERSATION_WITH_UNCONNECTED_USER, resp, convId)) map (_ => SyncResult.Failure(Some(resp), shouldRetry = false))
      case Left(error) =>
        warn(s"unexpected error: $error")
        Future.successful(SyncResult(error))
    }
  }

  private def postConv(id: ConvId)(post: ConversationData => Future[Either[ErrorResponse, Option[ConversationEvent]]]): Future[SyncResult] =
    withConversation(id) { post(_) flatMap postConvRespHandler }

  private val postConvRespHandler: (Either[ErrorResponse, Option[ConversationEvent]] => Future[SyncResult]) = {
    case Right(Some(event)) =>
      event.localTime = new Date
      convEvents.handlePostConversationEvent(event) map { _ => SyncResult.Success }
    case Right(None) =>
      debug(s"postConv got success response, but no event")
      Future successful SyncResult.Success
    case Left(error) => Future successful SyncResult(error)
  }

  private def withConversation(id: ConvId)(body: ConversationData => Future[SyncResult]): Future[SyncResult] =
    convs.convById(id) flatMap {
      case Some(conv) => body(conv)
      case _ =>
        error(s"No conversation found for id: $id")
        Future.successful(SyncResult.Failure(None, shouldRetry = true)) // XXX: does it make sense to retry ?
    }
}
