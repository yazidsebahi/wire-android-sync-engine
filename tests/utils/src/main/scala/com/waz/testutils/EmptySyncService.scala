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
package com.waz.testutils

import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.model.sync.SyncJob.Priority
import com.waz.sync._
import org.threeten.bp.Instant

import scala.concurrent.Future

class EmptySyncService extends EmptySyncServiceTrait

object EmptySyncService extends EmptySyncService

trait EmptySyncServiceTrait extends SyncServiceHandle {
  override def syncSearchQuery(cache: SearchQueryCache) = Future.successful(SyncId())
  override def syncConversation(id: ConvId, dependsOn: Option[SyncId] = None) = Future.successful(SyncId())
  override def syncConversations(dependsOn: Option[SyncId] = None) = Future.successful(SyncId())
  override def syncSelfUser() = Future.successful(SyncId())
  override def deleteAccount() = Future.successful(SyncId())
  override def syncUsers(ids: UserId*) = Future.successful(SyncId())
  override def syncConnectedUsers() = Future.successful(SyncId())
  override def syncConnections(dependsOn: Option[SyncId] = None) = Future.successful(SyncId())
  override def syncCommonConnections(id: UserId) = Future.successful(SyncId())
  override def syncCallState(id: ConvId, fromFreshNotification: Boolean, priority: Int = Priority.Normal) = Future.successful(SyncId())
  override def syncVersionBlacklist() = Future.successful(SyncId())
  override def syncRichMedia(id: MessageId, priority: Int = Priority.MinPriority): Future[SyncId] = Future.successful(SyncId())

  override def postConnection(user: UserId, name: String, message: String) = Future.successful(SyncId())
  override def postConnectionStatus(userId: UserId, status: ConnectionStatus) = Future.successful(SyncId())
  override def postSelfUser(u: UserInfo) = Future.successful(SyncId())
  override def postMessage(id: MessageId, conv: ConvId) = Future.successful(SyncId())
  override def postSelfPicture(picture: Option[AssetId]) = Future.successful(SyncId())
  override def postConversationName(id: ConvId, n: String) = Future.successful(SyncId())
  override def postConversationMemberJoin(id: ConvId, members: Seq[UserId]) = Future.successful(SyncId())
  override def postConversationMemberLeave(id: ConvId, member: UserId) = Future.successful(SyncId())
  override def postConversationState(id: ConvId, s: ConversationState) = Future.successful(SyncId())
  override def postConversation(id: ConvId, u: Seq[UserId], n: Option[String]) = Future.successful(SyncId())
  override def postLastRead(id: ConvId, time: Instant) = Future.successful(SyncId())
  override def postCleared(id: ConvId, time: Instant): Future[SyncId] = Future.successful(SyncId())
  override def postAddressBook(ab: AddressBook) = Future.successful(SyncId())
  override def postInvitation(i: Invitation) = Future.successful(SyncId())
  override def postTypingState(id: ConvId, t: Boolean) = Future.successful(SyncId())
  override def postExcludePymk(id: UserId): Future[SyncId] = Future.successful(SyncId())

  override def registerGcm() = Future.successful(SyncId())
  override def deleteGcmToken(token: GcmId) = Future.successful(SyncId())

  override def syncSelfClients(): Future[SyncId] = Future.successful(SyncId())
  override def postLiking(id: ConvId, liking: Liking): Future[SyncId] = Future.successful(SyncId())
  override def postClientLabel(id: ClientId, label: String): Future[SyncId] = Future.successful(SyncId())
  override def syncClients(user: UserId): Future[SyncId] = Future.successful(SyncId())
  override def syncClientsLocation(): Future[SyncId] = Future.successful(SyncId())
  override def syncPreKeys(user: UserId, clients: Set[ClientId]): Future[SyncId] = Future.successful(SyncId())
  override def postSessionReset(conv: ConvId, user: UserId, client: ClientId): Future[SyncId] = Future.successful(SyncId())
}
