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
package com.waz.service.conversation

import com.waz.ZLog._
import com.waz.api.{ImageAsset, MessageContent, User}
import com.waz.content.{ConversationStorage, UsersStorage, ZStorage}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service.call.VoiceChannelService
import com.waz.service.images.ImageAssetService
import com.waz.service.messages.MessagesService
import com.waz.service.{SearchKey, UserService}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils._
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future
import scala.language.higherKinds

class ConversationsUiService(assets: ImageAssetService, users: UserService, usersStorage: UsersStorage,
                             storage: ZStorage, messages: MessagesService, members: MembersContentUpdater,
                             convsContent: ConversationsContentUpdater, convStorage: ConversationStorage,
                             convs: ConversationsService, voice: VoiceChannelService, sync: SyncServiceHandle) {

  private implicit val tag: LogTag = logTagFor[ConversationsUiService]
  import Threading.Implicits.Background
  import assets._
  import convsContent._
  import members._
  import messages._
  import users._

  def sendMessage[A](convId: ConvId, content: MessageContent[A]): Future[MessageData] = {

    def mentionsMap(us: Array[User]): Future[Map[UserId, String]] =
      users.getUsers(us.map { u => UserId(u.getId) }) map { uss =>
        uss.map(u => u.id -> u.getDisplayName)(breakOut)
      }

    def sendTextMessage(m: String, mentions: Map[UserId, String] = Map.empty) =
      for {
        msg <- addTextMessage(convId, m, mentions)
        _   <- updateLastRead(msg)
        _   <- sync.postMessage(msg.id, convId)
      } yield msg

    def sendAssetMessage(img: ImageAsset, conv: ConversationData) = {
      verbose(s"sendAssetMessage($img, $conv)")
      if (img.getWidth > 0 && img.getHeight > 0) createMessageThenAsset(AssetId(img.getId), img, conv)
      else createAssetThenMessage(img, conv)
    }

    def createMessageThenAsset(assetId: AssetId, img: ImageAsset, conv: ConversationData) =
      for {
        msg     <- addAssetMessage(convId, assetId, img.getWidth, img.getHeight)
        _       <- updateLastRead(msg)
        _       <- addImageAsset(assetId, img, conv.remoteId, isSelf = false)
        _       <- sync.postMessage(msg.id, convId)
      } yield msg

    def createAssetThenMessage(img: ImageAsset, conv: ConversationData) =
      for {
        data    <- addImageAsset(AssetId(), img, conv.remoteId, isSelf = false)
        msg     <- addAssetMessage(convId, data.id, data.width, data.height)
        _       <- updateLastRead(msg)
        _       <- sync.postMessage(msg.id, convId)
      } yield msg

    content match {
      case m: MessageContent.Text =>
        debug(s"send text message ${m.getContent}")
        if (m.getMentions.isEmpty) sendTextMessage(m.getContent)
        else mentionsMap(m.getMentions) flatMap { ms => sendTextMessage(m.getContent, ms) }
      case m: MessageContent.Asset =>
        convById(convId) flatMap {
          case Some(conv) => sendAssetMessage(m.getContent, conv)
          case None       => Future.failed(new IllegalArgumentException(s"No conversation found for $convId"))
        }
      case _ =>
        error(s"sendMessage($content) not supported yet")
        Future.failed(new IllegalArgumentException(s"MessageContent: $content is not supported yet"))
    }
  }

  private def updateLastRead(msg: MessageData) = updateConversationLastRead(msg.convId, msg.time)

  def setConversationArchived(id: ConvId, archived: Boolean): Future[Option[ConversationData]] = convs.setConversationArchived(id, archived)

  def setConversationMuted(id: ConvId, muted: Boolean): Future[Option[ConversationData]] =
    updateConversationMuted(id, muted) map {
      case Some((_, conv)) => sync.postConversationState(id, ConversationState(muted = Some(conv.muted), muteTime = Some(conv.muteTime))); Some(conv)
      case None => None
    }

  def setConversationName(id: ConvId, name: String): Future[Option[ConversationData]] = withSelfUserFuture { selfUserId =>
    verbose(s"setConversationName($id, $name)")
    updateConversationName(id, name) flatMap {
      case Some((_, conv)) if conv.name.contains(name) =>
        sync.postConversationName(id, conv.name.getOrElse(""))
        addRenameConversationMessage(id, selfUserId, name) map (_ => Some(conv))
      case conv =>
        warn(s"Conversation name could not be changed for: $id, conv: $conv")
        CancellableFuture.successful(None)
    }
  }

  def addConversationMembers(conv: ConvId, users: Seq[UserId]): Future[Seq[ConversationMemberData]] =
    withSelfUserFuture { selfUserId =>
      ifAbleToModifyMembers(conv, selfUserId, Seq.empty[ConversationMemberData]) {
        addUsersToConversation(conv, users) flatMap { members =>
          if (members.nonEmpty) {
            addMemberJoinMessage(conv, selfUserId, members.map(_.userId).toSet) map { _ =>
              sync.postConversationMemberJoin(conv, members.map(_.userId).toSeq)
              members.toSeq
            }
          } else Future.successful(members.toSeq)
        }
      }
    }

  def removeConversationMember(conv: ConvId, user: UserId): Future[Option[SyncId]] =
    withSelfUserFuture { selfUserId =>
      ifAbleToModifyMembers(conv, selfUserId, Option.empty[SyncId]) {
        removeUsersFromConversation(conv, Seq(user)) flatMap { members =>
          if (members.isEmpty) Future.successful(None)
          else addMemberLeaveMessage(conv, selfUserId, members.head.userId) flatMap { _ =>
            sync.postConversationMemberLeave(conv, members.head.userId).map(Some(_))
          }
        }
      }
    }

  def leaveConversation(conv: ConvId): Future[Option[ConversationData]] = {
    verbose(s"leaveConversation($conv)")

    for {
      updated   <- setConversationStatusInactive(conv)
      _         <- withSelfUserFuture { removeConversationMember(conv, _) }
      archived  <- updateConversationArchived(conv, archived = true)
      _         <- voice.leaveVoiceChannel(conv)
    } yield archived.map(_._2).orElse(updated.map(_._2))
  }

  def ifAbleToModifyMembers[A, M[_]](conv: ConvId, user: UserId, zero: M[A])(f: => Future[M[A]]): Future[M[A]] = {
    val isGroup = convById(conv).map(_.exists(_.convType == ConversationType.Group))
    val isActiveMember = membersStorage.isActiveMember(conv, user)
    for {
      p1  <- isGroup
      p2  <- isActiveMember
      res <- if (p1 && p2) f else Future.successful(zero)
    } yield res
  }

  def clearConversation(id: ConvId): Future[Option[ConversationData]] = convById(id) flatMap {
    case Some(conv) if conv.convType == ConversationType.Group || conv.convType == ConversationType.OneToOne =>
      verbose(s"clearConversation($conv)")

      updateConversationCleared(conv.id, conv.lastEventTime) flatMap {
        case Some((_, c)) =>
          for {
            _ <- updateConversationLastRead(c.id, c.cleared)
            _ <- updateConversationArchived(c.id, archived = true)
            _ <- sync.postCleared(c.id, c.cleared)
          } yield Some(c)
        case None =>
          verbose("updateConversationCleared did nothing - already cleared")
          Future successful None
      }
    case Some(conv) =>
      warn(s"conversation of type ${conv.convType} can not be cleared")
      Future successful None
    case None =>
      warn(s"conversation to be cleared not found: $id")
      Future successful None
  }

  def getOrCreateOneToOneConversation(toUser: UserId): Future[ConversationData] =
    convById(ConvId(toUser.str)) flatMap {
      case Some(conv) => Future.successful(conv)
      case _ => withSelfUserFuture { selfUserId => createOneToOneConversation(toUser, selfUserId) }
    }

  /**
   * Creates conversation with already connected user.
   */
  def createOneToOneConversation(toUser: UserId, selfUserId: UserId) =
    usersStorage.get(toUser).flatMap {
      case Some(u) if u.connection == ConnectionStatus.Ignored =>
        createConversationWithMembers(ConvId(toUser.str), u.conversation.getOrElse(RConvId()), ConversationType.Incoming, toUser, Seq(selfUserId), hidden = true) flatMap { conv =>
          addMemberJoinMessage(conv.id, toUser, Set(selfUserId)) flatMap { _ =>
            u.connectionMessage.fold { Future.successful(conv) } { msg =>
              addConnectRequestMessage(conv.id, toUser, selfUserId, msg, u.name) map  { _ => conv }
            }
          }
        }
      case _ =>
        sync.postConversation(ConvId(toUser.str), Seq(toUser), None)
        createConversationWithMembers(ConvId(toUser.str), RConvId(), ConversationType.OneToOne, selfUserId, Seq(toUser)) flatMap { conv =>
          addMemberJoinMessage(conv.id, selfUserId, Set(toUser)) map (_ => conv)
        }
    }

  def createGroupConversation(id: ConvId, members: Seq[UserId]): Future[ConversationData] = withSelfUserFuture { selfUserId =>
    debug(s"createGroupConversation, id: $id, members: $members")
    createConversationWithMembers(id, ConversationsService.generateTempConversationId((selfUserId +: members).distinct: _*), ConversationType.Group, selfUserId, members) flatMap { conv =>
      debug(s"created: $conv")
      sync.postConversation(id, members, conv.name)
      addMemberJoinMessage(conv.id, selfUserId, members.toSet) map (_ => conv)
    }
  }

  def findGroupConversations(prefix: SearchKey, limit: Int): Future[Seq[ConversationData]] =
    withSelfUserFuture(id => convStorage.search(prefix, id)).map(_.sortBy(_.displayName)(currentLocaleOrdering).take(limit))

  def knock(id: ConvId): Future[Option[MessageData]] = withSelfUserFuture { selfUserId =>
    Serialized.future("knock", id) {
      getActiveKnockMessage(id, selfUserId) flatMap {
        case Some(msg) if msg.hotKnock => CancellableFuture.successful(None) // ignore - hot knock not expired
        case Some(msg) => // change to hot knock
          sync.postMessage(msg.id, id)
          updateKnockToHotKnock(msg.id)
        case _ =>
          addKnockMessage(id, selfUserId) map { msg =>
            sync.postMessage(msg.id, id)
            Some(msg)
          }
      }
    }
  }

  def setLastRead(convId: ConvId, time: Instant, lastRead: EventId): Future[Option[ConversationData]] = {
    updateConversationLastRead(convId, time) map {
      case Some((_, conv)) =>
        sync.postLastRead(convId, conv.lastRead)
        Some(conv)
      case _ => None
    }
  }
}
