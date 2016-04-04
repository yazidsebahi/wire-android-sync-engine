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
package com.waz.service.messages

import com.waz.ZLog._
import com.waz.api.Message
import com.waz.api.Message.Status
import com.waz.content.LikingsStorage
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericMessage._
import com.waz.model._
import com.waz.service._
import com.waz.service.conversation.{ConversationsContentUpdater, ConversationsService}
import com.waz.service.images.ImageAssetService
import com.waz.service.otr.VerificationStateUpdater.{ClientUnverified, VerificationChange}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.RichFuture.traverseSequential
import com.waz.utils._
import com.waz.utils.events.{EventContext, Signal}
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.concurrent.Future.{traverse, successful}
import collection.breakOut

class MessagesService(val content: MessagesContentUpdater, assets: ImageAssetService, users: UserService, convs: ConversationsContentUpdater, likings: LikingsStorage, network: NetworkModeService, sync: SyncServiceHandle) {
  import Threading.Implicits.Background
  private implicit val tag: LogTag = logTagFor[MessagesService]
  private implicit val ec = EventContext.Global

  import assets._
  import content._
  import convs._
  import users._

  val messageEventProcessingStage = EventScheduler.Stage[MessageEvent] { (convId, events) =>
    verbose(s"got events to process: $events")
    processConvWithRemoteId(convId, retryAsync = true) { conv =>
      verbose(s"processing events for conv: $conv, events: $events")
      processEvents(conv, events)
    }
  }

  private[service] def processEvents(conv: ConversationData, events: Seq[MessageEvent]): Future[Set[MessageData]] = {
    val filtered = events.filter(e => conv.cleared.isBefore(e.time.instant))

    for {
      _ <- updateAssets(filtered)
      msgs = filtered map { createMessage(conv, _) } filter (_ != MessageData.Empty)
      _ = verbose(s"messages from events: $msgs")
      ms <- fixLegacyHotKnockIds(conv.id, msgs)
      res <- content.addMessages(conv.id, ms)
    } yield res
  }

  private def updateAssets(events: Seq[MessageEvent]) = Future.sequence(events.collect {
    case AssetAddEvent(_, remoteId, eventId, time, from, assetId, asset: ImageData) => updateImageAsset(assetId, remoteId, asset)
  })

  private def createMessage(conv: ConversationData, event: MessageEvent) = {
    val id = MessageId(event.id)
    val convId = conv.id

    event match {
      case MessageAddEvent(_, _, eventId, time, from, text) =>
        val (tpe, ct) = MessageData.messageContent(text)
        MessageData(id, convId, eventId, eventId, tpe, from, ct, time = time.instant, localTime = event.localTime.instant)
      case ConnectRequestEvent(_, _, eventId, time, from, text, recipient, name, email) =>
        MessageData(id, convId, eventId, eventId, Message.Type.CONNECT_REQUEST, from, MessageData.textContent(text), recipient = Some(recipient), email = email, name = Some(name), time = time.instant, localTime = event.localTime.instant)
      case RenameConversationEvent(_, _, eventId, time, from, name) =>
        MessageData(id, convId, eventId, eventId, Message.Type.RENAME, from, name = Some(name), time = time.instant, localTime = event.localTime.instant)
      case KnockEvent(_, _, eventId, time, from, text) =>
        MessageData(id, convId, eventId, eventId, Message.Type.KNOCK, from, MessageData.textContent(text), time = time.instant, localTime = event.localTime.instant)
      case HotKnockEvent(_, _, eventId, time, from, text, ref) =>
        MessageData(id, convId, ref, eventId, Message.Type.KNOCK, from, MessageData.textContent(text), hotKnock = true, time = time.instant, localTime = event.localTime.instant)
      case MemberJoinEvent(_, _, eventId, time, from, userIds) =>
        MessageData(id, convId, eventId, eventId, Message.Type.MEMBER_JOIN, from, members = userIds.toSet, time = time.instant, localTime = event.localTime.instant)
      case MemberLeaveEvent(_, _, eventId, time, from, userIds) =>
        MessageData(id, convId, eventId, eventId, Message.Type.MEMBER_LEAVE, from, members = userIds.toSet, time = time.instant, localTime = event.localTime.instant)
      case AssetAddEvent(_, _, eventId, time, from, assetId, asset: ImageData) =>
        MessageData(MessageId(assetId.str), convId, eventId, eventId, Message.Type.ASSET, from, content = MessageData.imageContent(assetId, asset.origWidth, asset.origHeight), time = time.instant, localTime = event.localTime.instant, otr = asset.otrKey.isDefined)
      case MissedCallEvent(_, _, eventId, time, from) =>
        MessageData(id, convId, eventId, eventId, Message.Type.MISSED_CALL, from, time = time.instant, localTime = event.localTime.instant)
      case _: VoiceChannelDeactivateEvent =>
        MessageData.Empty // don't add any message, interesting case is handled with MissedCallEvent extractor
      case GenericMessageEvent(_, _, eventId, time, from, msg, otr) =>
        val id = MessageId(msg.id.str)
        msg.content match {
          case Text(text, mentions) =>
            val (tpe, content) = MessageData.messageContent(text, mentions)
            MessageData(id, conv.id, eventId, EventId.Zero, tpe, from, content, time = time.instant, localTime = event.localTime.instant, otr = otr)
          case Knock(hotKnock) =>
            MessageData(id, conv.id, eventId, EventId.Zero, Message.Type.KNOCK, from, time = time.instant, localTime = event.localTime.instant, hotKnock = hotKnock, otr = otr)
          case LikeAction(action) => MessageData.Empty
          case LastRead(remoteId, timestamp) => MessageData.Empty
          case Cleared(remoteId, timestamp) => MessageData.Empty
          case OtrError(_, _, _) =>
            MessageData(id, conv.id, eventId, EventId.Zero, Message.Type.OTR_ERROR, from, time = time.instant, localTime = event.localTime.instant, otr = otr)
          case msgContent =>
            error(s"unexpected generic message content: $msgContent")
            // TODO: save original protobuf content for later - future app version could understand this message better
            MessageData(id, conv.id, eventId, EventId.Zero, Message.Type.UNKNOWN, from, time = time.instant, localTime = event.localTime.instant, otr = otr)
        }
      case _ =>
        warn(s"Unexpected event for addMessage: $event")
        MessageData.Empty
    }
  }

  // messages generated by knock and hotknock events should have the same id (if they refer to the same message)
  // but some clients send those events with different nonces, so we need to do the matching here
  private def fixLegacyHotKnockIds(convId: ConvId, msgs: Seq[MessageData]) = {
    val hotKnocks = msgs.filter(_.hotKnock)
    if (hotKnocks.isEmpty) successful(msgs)
    else {
      val knocks = msgs.filter(m => m.msgType == Message.Type.KNOCK && !m.hotKnock).map(m => m.source -> m.id).toMap
      val unmatched = hotKnocks.filter(m => !knocks.contains(m.source))
      val eventMap =
        if (unmatched.isEmpty) successful(knocks)
        else messagesStorage.getMessages(convId, unmatched.map(_.source)) map { ms => knocks ++ ms.map(m => m.source -> m.id)}

      eventMap map { evMap =>
        msgs map { m =>
          if (m.hotKnock) m.copy(id = evMap.getOrElse(m.source, m.id))
          else m
        }
      }
    }
  }

  def updateKnockToHotKnock(msgId: MessageId) = messagesStorage.update(msgId) { msg =>
    if (msg.hotKnock) msg
    else msg.copy(hotKnock = true, localTime = Instant.now)
  }

  /**
   * Return last message if it's KNOCK message added by self user.
   */
  def getActiveKnockMessage(convId: ConvId, selfUserId: UserId): Future[Option[MessageData]] = messagesStorage.getLastMessage(convId) map { lastMsg =>
    lastMsg.filter(m => m.userId == selfUserId && m.msgType == Message.Type.KNOCK && !ConversationsService.knockExpired(m))
  }

  def activeKnockMessage(convId: ConvId) = messagesStorage.lastMessage(convId) flatMap {
    case Some(m) if m.msgType == Message.Type.KNOCK && !ConversationsService.knockExpired(m) =>
      Signal.future(users.getSelfUserId.map(_.flatMap { id => Some(m).filter(_.userId == id) }))
    case _ => Signal.const(Option.empty[MessageData])
  }

  def getIncomingMessages = messagesStorage.getIncomingMessages flatMap { msgs =>
    Signal.future {
      convs.storage.getAll(msgs.map(_.convId).distinct) map { cs =>
        val muted = cs.flatten.filter(_.muted).map(_.id).toSet
        msgs filterNot { msg => muted(msg.convId) }
      }
    }
  }

  def addTextMessage(convId: ConvId, content: String, mentions: Map[UserId, String] = Map.empty): Future[MessageData] = withSelfUserFuture { selfUserId =>
    verbose(s"addTextMessage($convId, $content, $mentions)")
    val (tpe, ct) = MessageData.messageContent(content, mentions)
    verbose(s"parsed content: $ct")
    addLocalMessage(MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, tpe, selfUserId, ct))
  }

  def addAssetMessage(convId: ConvId, assetId: AssetId, width: Int, height: Int) = withSelfUserFuture { selfUserId =>
    addLocalMessage(MessageData(MessageId(assetId.str), convId, EventId.Zero, EventId.Zero, Message.Type.ASSET, selfUserId, MessageData.imageContent(assetId, width, height)))
  }

  def addRenameConversationMessage(convId: ConvId, selfUserId: UserId, name: String) =
    updateOrCreateLocalMessage(convId, Message.Type.RENAME, _.copy(name = Some(name)), MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.RENAME, selfUserId, name = Some(name)))

  def addConnectRequestMessage(convId: ConvId, fromUser: UserId, toUser: UserId, message: String, name: String, fromSync: Boolean = false) = {
    val msg = MessageData(
      MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.CONNECT_REQUEST, fromUser, content = MessageData.textContent(message), name = Some(name), recipient = Some(toUser),
      time = if (fromSync) MessageData.UnknownInstant else Instant.now)

    if (fromSync) messagesStorage.insert(msg) else addLocalMessage(msg)
  }

  def addKnockMessage(convId: ConvId, selfUserId: UserId) = {
    debug(s"addKnockMessage($convId, $selfUserId)")
    addLocalMessage(MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.KNOCK, selfUserId))
  }

  def addDeviceStartMessages(convs: Seq[ConversationData], selfUserId: UserId): Future[Set[MessageData]] =
    Serialized.future('addDeviceStartMessages)(traverse(convs filter isGroupOrOneToOne) { conv =>
      messagesStorage.getLastMessage(conv.id) map {
        case None =>    Some(MessageData(MessageId(), conv.id, EventId.Zero, EventId.Zero, Message.Type.STARTED_USING_DEVICE, selfUserId, time = Instant.EPOCH))
        case Some(_) => None
      }
    } flatMap { msgs =>
      messagesStorage.insert(msgs.flatten)
    })

  private def isGroupOrOneToOne(conv: ConversationData) = conv.convType == ConversationType.Group || conv.convType == ConversationType.OneToOne

  def addHistoryLostMessages(cs: Seq[ConversationData], selfUserId: UserId): Future[Set[MessageData]] = {
    // TODO: those messages should include information about what was actually changed
    traverseSequential(cs) { conv =>
      messagesStorage.getLastMessage(conv.id) map {
        case Some(msg) if msg.msgType != Message.Type.STARTED_USING_DEVICE =>
          Some(MessageData(MessageId(), conv.id, EventId.Zero, EventId.Zero, Message.Type.HISTORY_LOST, selfUserId, time = msg.time.plusMillis(1)))
        case _ =>
          // conversation has no messages or has STARTED_USING_DEVICE msg,
          // it means that conv was just created and we don't need to add history lost msg
          None
      }
    } flatMap { msgs =>
      messagesStorage.insert(msgs.flatten) flatMap { added =>
        // mark messages read if there is no other unread messages
        val times: Map[ConvId, Instant] = added.map(m => m.convId -> m.time) (breakOut)
        convs.storage.updateAll2(times.keys, { c =>
          val t = times(c.id)
          if (c.lastRead.toEpochMilli == t.toEpochMilli - 1) c.copy(lastRead = t) else c
        }) map { _ => added }
      }
    }
  }

  def addMemberJoinMessage(convId: ConvId, creator: UserId, users: Set[UserId]) = {
    verbose(s"addMemberJoinMessage($convId, $creator, $users)")

    def updateOrCreate(added: Set[UserId]) = {
      def update(msg: MessageData) = {
        msg.copy(members = msg.members ++ added)
      }
      def create = MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.MEMBER_JOIN, creator, members = added)
      updateOrCreateLocalMessage(convId, Message.Type.MEMBER_JOIN, update, create)
    }

    // check if we have local leave message with same users
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_LEAVE) flatMap {
      case Some(msg) if users.exists(msg.members) =>
        val toRemove = msg.members -- users
        val toAdd = users -- msg.members
        if (toRemove.isEmpty) deleteMessage(convId, msg.source) // FIXME: race condition
        else updateMessage(msg.id)(_.copy(members = toRemove)) // FIXME: race condition

        if (toAdd.isEmpty) successful(()) else updateOrCreate(toAdd)
      case _ =>
        updateOrCreate(users)
    }
  }

  def removeLocalMemberJoinMessage(convId: ConvId, users: Set[UserId]) = {
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_JOIN) flatMap {
      case Some(msg) =>
        val members = msg.members -- users
        if (members.isEmpty) {
          deleteMessage(convId, msg.source)
        } else {
          updateMessage(msg.id) { _.copy(members = members) } // FIXME: possible race condition with addMemberJoinMessage or sync
        }
      case _ =>
        warn("removeLocalMemberJoinMessage: no local join message found")
        CancellableFuture.successful(())
    }
  }

  def addMemberLeaveMessage(convId: ConvId, selfUserId: UserId, user: UserId) = {
    // check if we have local join message with this user and just remove him from the list
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_JOIN) flatMap {
      case Some(msg) if msg.members == Set(user) => deleteMessage(convId, msg.source) // FIXME: race condition
      case Some(msg) if msg.members(user) => updateMessage(msg.id)(_.copy(members = msg.members - user)) // FIXME: race condition
      case _ =>
        // check for local MemberLeave message before creating new one
        def newMessage = MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.MEMBER_LEAVE, selfUserId, members = Set(user))
        def update(msg: MessageData) = msg.copy(members = msg.members + user)
        updateOrCreateLocalMessage(convId, Message.Type.MEMBER_LEAVE, update, newMessage)
    }
  }

  def addOtrVerifiedMessage(convId: ConvId): Future[Option[MessageData]] =
    messagesStorage.getLastMessage(convId) flatMap {
      case Some(msg) if msg.msgType == Message.Type.OTR_UNVERIFIED || msg.msgType == Message.Type.OTR_DEVICE_ADDED =>
        verbose(s"addOtrVerifiedMessage, removing previous message: $msg")
        messagesStorage.delete(msg.id) map { _ => None }
      case _ =>
        withSelfUserFuture { selfUser =>
          addLocalMessage(MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, Message.Type.OTR_VERIFIED, selfUser), Status.SENT) map { Some(_) }
        }
    }

  def addOtrUnverifiedMessage(convId: ConvId, users: Seq[UserId], change: VerificationChange): Future[Option[MessageData]] = {
      val msgType = if (change == ClientUnverified) Message.Type.OTR_UNVERIFIED else Message.Type.OTR_DEVICE_ADDED
      withSelfUserFuture { selfUser =>
        addLocalSentMessage(MessageData(MessageId(), convId, EventId.Zero, EventId.Zero, msgType, selfUser, members = users.toSet)) map { Some(_) }
      }
    }

  def retryMessageSending(conv: ConvId, msgId: MessageId) =
    updateMessage(msgId) { msg =>
      if (msg.state == Status.SENT || msg.state == Status.PENDING) msg
      else msg.copy(state = Status.PENDING)
    } .flatMap {
      case Some(msg) => sync.postMessage(msg.id, conv) map (Some(_))
      case _ => successful(None)
    }

  def messageSent(convId: ConvId, messageId: MessageId) = updateMessageState(convId, messageId, Message.Status.SENT)

  def messageDeliveryFailed(convId: ConvId, messageId: MessageId) = updateMessageState(convId, messageId, Message.Status.FAILED)

  def updateMessageState(convId: ConvId, messageId: MessageId, state: Message.Status) =
    updateMessage(messageId) { _.copy(state = state) }

  def markMessageRead(convId: ConvId, id: MessageId) =
    if (network.isOfflineMode) CancellableFuture.successful(None)
    else
      updateMessage(id) { msg =>
        if (msg.state == Status.FAILED) msg.copy(state = Status.FAILED_READ)
        else msg
      }
}
