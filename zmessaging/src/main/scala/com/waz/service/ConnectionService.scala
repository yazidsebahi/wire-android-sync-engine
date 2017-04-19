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
package com.waz.service

import com.waz.ZLog._
import com.waz.content.{DefaultMembersStorage, MessagesStorage, UsersStorage}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service.conversation.DefaultConversationsContentUpdater
import com.waz.service.messages.MessagesService
import com.waz.service.push.PushService
import com.waz.sync.SyncServiceHandle
import com.waz.threading.Threading
import com.waz.utils.RichFuture
import com.waz.utils.events.EventContext

import scala.collection.breakOut
import scala.concurrent.Future

class ConnectionService(push: PushService, convs: DefaultConversationsContentUpdater, members: DefaultMembersStorage,
                        messages: MessagesService, messagesStorage: MessagesStorage, users: UserService, usersStorage: UsersStorage,
                        sync: SyncServiceHandle, scheduler: => EventScheduler) {

  private implicit val logTag: LogTag = logTagFor[ConnectionService]
  import Threading.Implicits.Background
  private implicit val ec = EventContext.Global
  import convs._
  import messages._
  import users._

  val convStorage = convs.storage

  val connectionEventsStage = EventScheduler.Stage[UserConnectionEvent]((c, e) => handleUserConnectionEvents(e))

  val contactJoinEventsStage = EventScheduler.Stage[ContactJoinEvent] { (c, es) =>
    RichFuture.processSequential(es) { e =>
      getOrCreateUser(e.user) flatMap { _ =>
        // update user name if it was just created (has empty name)
        updateUserData(e.user, u => u.copy(name = if (u.name == "") e.name else u.name))
      }
    }
  }

  def syncConversationInitiallyAfterCreation(convId: RConvId, selfUserId: UserId, userId: UserId) =
    getOneToOneConversation(userId, selfUserId, Some(convId), ConversationType.WaitForConnection) flatMap { conv =>
      sync.syncConversation(conv.id)
    }

  def handleUserConnectionEvents(events: Seq[UserConnectionEvent]) = {
    verbose(s"handleUserConnectionEvents: $events")
    def updateOrCreate(event: UserConnectionEvent)(user: Option[UserData]): UserData =
      user.fold {
        UserData(event.to, UserService.defaultUserName, None, None, connection = event.status, conversation = Some(event.convId), connectionMessage = event.message, searchKey = SearchKey(UserService.defaultUserName), connectionLastUpdated = event.lastUpdated,
          handle = None)
      } {
        _.copy(conversation = Some(event.convId)).updateConnectionStatus(event.status, Some(event.lastUpdated), event.message)
      }

    val lastEvents = events.groupBy(_.to).map { case (_, es) => es.maxBy(_.lastUpdated) }

    val fromSync: Set[UserId] = lastEvents.filter(_.localTime == Event.UnknownDateTime).map(_.to)(breakOut)
    usersStorage.updateOrCreateAll(lastEvents.map { ev => ev.to -> updateOrCreate(ev) _ } (breakOut)) map ((_, fromSync))
  } flatMap { case (users, fromSync) =>
    val toSync = users filter { u => u.connection == ConnectionStatus.Accepted || u.connection == ConnectionStatus.PendingFromOther || u.connection == ConnectionStatus.PendingFromUser }
    sync.syncUsersIfNotEmpty(toSync.map(_.id)(breakOut)) flatMap { _ =>
      withSelfUserFuture { selfUser =>
        RichFuture.processSequential(users.grouped(16).toSeq) { us =>
          Future.traverse(us)(u => updateConversationForConnection(u, selfUser, fromSync = fromSync(u.id)))
        }
      }
    }
  }

  def updateConversationForConnection(user: UserData, selfUserId: UserId, fromSync: Boolean) = {
    verbose(s"updateConversationForConnection: $user")
    val convType = user.connection match {
      case ConnectionStatus.PendingFromUser | ConnectionStatus.Cancelled => ConversationType.WaitForConnection
      case ConnectionStatus.PendingFromOther | ConnectionStatus.Ignored => ConversationType.Incoming
      case _ => ConversationType.OneToOne
    }
    val hidden = user.connection == ConnectionStatus.Ignored || user.connection == ConnectionStatus.Blocked || user.connection == ConnectionStatus.Cancelled
    getOneToOneConversation(user.id, selfUserId, user.conversation, convType) flatMap { conv =>
      members.add(conv.id, Seq(selfUserId, user.id): _*) flatMap { members =>
        convStorage.update(conv.id, _.copy(convType = convType, hidden = hidden)) flatMap { updated =>
          messagesStorage.getLastMessage(conv.id) flatMap {
            case None if convType == ConversationType.Incoming =>
              addConnectRequestMessage(conv.id, user.id, selfUserId, user.connectionMessage.getOrElse(""), user.getDisplayName, fromSync = fromSync)
            case None if convType == ConversationType.OneToOne =>
              messages.addDeviceStartMessages(Seq(conv), selfUserId)
            case _ =>
              Future.successful(())
          } map { _ =>
            if (conv.hidden && !hidden) sync.syncConversation(conv.id)
            updated.fold(conv)(_._2)
          }
        }
      }
    }
  }

  /**
   * Connects to user and creates one-to-one conversation if needed. Returns existing conversation if user is already connected.
   */
  def connectToUser(userId: UserId, message: String, name: String): Future[Option[ConversationData]] = {

    def sanitizedName = if (name.isEmpty) "_" else if (name.length >= 256) name.substring(0, 256) else name

    def connectIfUnconnected() = getOrCreateUser(userId) flatMap { user =>
      if (user.isConnected) {
        info(s"User already connected: $user")
        Future successful None
      } else {
        updateConnectionStatus(user.id, ConnectionStatus.PendingFromUser) flatMap {
          case Some(u) => sync.postConnection(userId, sanitizedName, message) map (_ => Some(u))
          case _ => Future.successful(None)
        }
      }
    }

    withSelfUserFuture { selfUserId =>
      connectIfUnconnected() flatMap {
        case Some(_) =>
          getOneToOneConversation(userId, selfUserId, convType = ConversationType.WaitForConnection) flatMap { conv =>
            verbose(s"connectToUser, conv: $conv")
            convStorage.update(conv.id, _.copy(convType = ConversationType.WaitForConnection, hidden = false)) flatMap { _ =>
              addConnectRequestMessage(conv.id, selfUserId, userId, message, name) map { _ => Some(conv) }
            }
          }
        case None => //already connected
          convById(ConvId(userId.str))
      }
    }
  }

  def acceptConnection(userId: UserId): Future[ConversationData] = withSelfUserFuture { selfUserId =>
    updateConnectionStatus(userId, ConnectionStatus.Accepted) map {
      case Some(_) =>
        sync.postConnectionStatus(userId, ConnectionStatus.Accepted) map { syncId =>
          sync.syncConversation(ConvId(userId.str), Some(syncId))
        }
      case _ =>
    } flatMap { _ =>
      getOneToOneConversation(userId, selfUserId, convType = ConversationType.OneToOne) flatMap { conv =>
        updateConversation(conv.id, Some(ConversationType.OneToOne), hidden = Some(false)) flatMap { updated =>
          messagesStorage.getLastMessage(conv.id) flatMap  {
            case Some(_) =>
              Future successful updated.fold(conv)(_._2)
            case _ =>
              addMemberJoinMessage(conv.id, selfUserId, Set(selfUserId), firstMessage = true) map { _ =>
                updated.fold(conv)(_._2)
              }
          }
        }
      }
    }
  }

  def ignoreConnection(userId: UserId): Future[Option[UserData]] = {
    withSelfUserFuture { selfUserId =>
      updateConnectionStatus(userId, ConnectionStatus.Ignored) flatMap { user =>
        user.foreach { _ => sync.postConnectionStatus(userId, ConnectionStatus.Ignored) }
        hideIncomingConversation(userId) map { _ => user }
      }
    }
  }

  def blockConnection(userId: UserId): Future[Option[UserData]] = {
    withSelfUserFuture { selfUserId =>
      hideConversation(ConvId(userId.str)) flatMap { _ =>
        updateConnectionStatus(userId, ConnectionStatus.Blocked) map { user =>
          user foreach { _ => sync.postConnectionStatus(userId, ConnectionStatus.Blocked) }
          user
        }
      }
    }
  }

  def unblockConnection(userId: UserId): Future[ConversationData] = {
    withSelfUserFuture { selfUserId =>
      updateConnectionStatus(userId, ConnectionStatus.Accepted) map { user =>
        user foreach { _ =>
          sync.postConnectionStatus(userId, ConnectionStatus.Accepted) map { syncId =>
            sync.syncConversation(ConvId(userId.str), Some(syncId)) // sync conversation after syncing connection state (conv is locked on backend while connection is blocked) TODO: we could use some better api for that
          }
        }
        user
      } flatMap { _ =>
        getOneToOneConversation(userId, selfUserId, convType = ConversationType.OneToOne) flatMap { conv =>
          updateConversation(conv.id, Some(ConversationType.OneToOne), hidden = Some(false)) map { _.fold(conv)(_._2) } // TODO: what about messages
        }
      }
    }
  }

  def cancelConnection(userId: UserId): Future[Option[UserData]] = {
    updateUserData(userId, { user =>
      if (user.connection == ConnectionStatus.PendingFromUser) user.copy(connection = ConnectionStatus.Cancelled)
      else {
        warn(s"can't cancel connection for user in wrong state: $user")
        user
      }
    }) flatMap {
      case Some((prev, user)) if prev != user =>
        sync.postConnectionStatus(userId, ConnectionStatus.Cancelled)
        hideConversationOfUser(userId) map { _ => Some(user) }
      case None => Future successful None
    }
  }
}
