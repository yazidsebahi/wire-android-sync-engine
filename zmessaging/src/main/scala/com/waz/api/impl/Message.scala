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
package com.waz.api.impl

import android.os.Parcel
import com.waz.Control.getOrUpdate
import com.waz.ZLog._
import com.waz.api
import com.waz.api.Message.{Part, Status, Type}
import com.waz.api.MessageContent.{Location, Text}
import com.waz.api.{EphemeralExpiration, IConversation, ImageAssetFactory, UpdateListener}
import com.waz.model.GenericContent.{LinkPreview, MsgEdit}
import com.waz.model.GenericMessage.TextMessage
import com.waz.model._
import com.waz.model.sync.SyncJob.Priority
import com.waz.service.media.GoogleMapsMediaService
import com.waz.service.messages.MessageAndLikes
import com.waz.sync.client.OpenGraphClient.OpenGraphData
import com.waz.threading.Threading
import com.waz.ui._
import com.waz.utils._
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.URI
import org.threeten.bp.Instant

import scala.collection.breakOut

class Message(val id: MessageId, var data: MessageData, var likes: IndexedSeq[UserId], var likedBySelf: Boolean)(implicit context: UiModule) extends api.Message with SignalLoading with UiObservable {

  private implicit val logTag: LogTag = logTagFor[Message]
  private val convId = if (data == MessageData.Empty) Signal[ConvId]() else Signal(data.convId)
  private var parts = Array.empty[Part]
  private var lastMessageFromSelf = false
  private var lastMessageFromOther = false

  def this(msg: MessageAndLikes)(context: UiModule) = this(msg.message.id, msg.message, msg.likes, msg.likedBySelf)(context)
  def this(id: MessageId)(context: UiModule) = this(id, MessageData.Empty, Vector.empty, false)(context)

  updateParts()
  reload() // always reload because data from constructor might always be outdated already
  addLoader(zms => convId.flatMap(c => zms.messagesStorage.lastMessageFromSelfAndFromOther(c)), (Option.empty[MessageData], Option.empty[MessageData])) { case (fromSelf, fromOther) =>
    val isMostRecentFromSelf = fromSelf.exists(_.id == id)
    val isMostRecentFromOther = fromOther.exists(_.id == id)
    if (isMostRecentFromSelf != lastMessageFromSelf || isMostRecentFromOther != lastMessageFromOther) {
      lastMessageFromSelf = isMostRecentFromSelf
      lastMessageFromOther = isMostRecentFromOther
      notifyChanged()
    }
  }

  def reload() = context.zms.flatMapFuture(_.msgAndLikes.getMessageAndLikes(id))(Threading.Background).map { m => set(m.getOrElse(MessageAndLikes.Deleted)) } (Threading.Ui)

  def set(msg: MessageAndLikes): Unit = if (msg.message != data || msg.likes != likes || msg.likedBySelf != likedBySelf) {
    data = msg.message
    likes = msg.likes
    likedBySelf = msg.likedBySelf
    updateParts()
    notifyChanged()
    convId ! data.convId
    notifyEphemeralRead()
  }


  override def addUpdateListener(listener: UpdateListener): Unit = {
    super.addUpdateListener(listener)
    notifyEphemeralRead()
  }

  private def notifyEphemeralRead() =
    if (data.ephemeral != EphemeralExpiration.NONE && data.expiryTime.isEmpty && getListenersCount > 0)
      context.zms { _.ephemeral.onMessageRead(id) }

  private def updateParts(): Unit = parts = data.content.zipWithIndex.map { case (c, index) => new MessagePart(c, data, index) } (breakOut)

  private def content = if (data.content.isEmpty) MessageContent.Empty else data.content.head

  override def getParts: Array[Part] = parts

  override def getMentionedUsers =
    if (content.mentions.isEmpty) Array.empty
    else content.mentions.map { case (userId, _) => context.users.getUser(userId) } (breakOut)

  override def isUserMentioned: Boolean = context.users.selfUser.userId.exists(content.mentions.contains)

  override def isDeleted: Boolean = data.state == Status.DELETED

  override def getTime: Instant = data.time

  override def getEditTime: Instant = data.editTime

  override def isEdited: Boolean = data.protos exists {
    case GenericMessage(_, MsgEdit(_, _)) => true
    case _ => false
  }

  override def getBody: String = data.contentString

  override def getLocation: Location = data.location.orNull

  override def isEphemeral = data.ephemeral != EphemeralExpiration.NONE

  override def isExpired: Boolean = data.expired

  override def getEphemeralExpiration = data.ephemeral

  override def getExpirationTime: Instant = data.expiryTime.getOrElse(Instant.MAX)

  override def getImage = getImage(0, 0)

  override def getImage(w: Int, h: Int) = data.msgType match {
    case Type.LOCATION =>
      data.location.fold2(ImageAsset.Empty, { loc =>
        val id = AssetId(s"${data.assetId.str}_${w}_$h") // use dimensions in id, to avoid caching images with different sizes
        context.images.getLocalImageAsset(GoogleMapsMediaService.mapImageAsset(id, loc, if (w <= 0) GoogleMapsMediaService.ImageDimensions else Dim2(w, h)))
      })
    case _ => context.images.getImageAsset(data.assetId)
  }

  override def getAsset =
    if (data.isAssetMessage) getOrUpdate(context.assets)(data.assetId, new Asset(data.assetId, id)(context))
    else Asset.Empty

  override def getUser: User = context.users.getUser(data.userId)

  override def getMessageStatus: Status = if (data.state == Status.FAILED_READ) Status.FAILED else data.state

  override def getMessageType = data.msgType

  override def getConversationId: String = data.convId.toString

  override def getConversation: IConversation = context.convs.convById(data.convId)

  override def getId: String = data.id.str

  override def getMembers = data.members.map(id => context.users.getUser(id))(breakOut)

  override def getImageWidth: Int = data.imageDimensions.fold(0)(_.width)

  override def getImageHeight: Int = data.imageDimensions.fold(0)(_.height)

  override def getNewConversationName: String = data.name.getOrElse("")

  /**
    * @deprecated hot knocks are no longer supported.
    *            This method will always return `false`.
    */
  override def isHotKnock: Boolean = false

  override def isFirstMessage: Boolean = data.firstMessage

  override def isLastMessageFromSelf: Boolean = lastMessageFromSelf
  override def isLastMessageFromOther: Boolean = lastMessageFromOther

  override def isCreateConversation: Boolean = data.msgType == api.Message.Type.MEMBER_JOIN && data.firstMessage

  override def getLocalTime: Instant = data.localTime

  override def retry(): Unit =
    if (data.state == Status.FAILED || data.state == Status.FAILED_READ) context.messages.retry(data.convId, data.id)
    else error(s"Retrying a message that has not yet failed (${data.state}): $id")

  override def delete(): Unit = context.zms.flatMapFuture(_.convsUi.deleteMessage(data.convId, id))

  override def recall(): Unit = context.zms.flatMapFuture(_.convsUi.recallMessage(data.convId, id))

  override def update(content: Text): Unit = context.zms.flatMapFuture(_.convsUi.updateMessage(data.convId, id, content))

  override def equals(other: Any): Boolean = other match {
    case other: Message => id == other.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = s"Message($id, $content, $likes, $data)"

  override def writeToParcel(dest: Parcel, flags: Int): Unit = {
    dest.writeString(JsonEncoder.encodeString(data))
    dest.writeStringArray(likes.map(_.str)(breakOut))
    dest.writeInt(if (likedBySelf) 1 else 0)
  }

  override def describeContents(): Int = 0

  override def getLikes: Array[api.User] = likes.map(context.users.getUser)(breakOut)

  override def like(): Unit = context.zms.flatMapFuture(_.reactions.like(data.convId, id))
  override def unlike(): Unit = context.zms.flatMapFuture(_.reactions.unlike(data.convId, id))
  override def isLikedByThisUser: Boolean = likedBySelf
  override def isLiked: Boolean = likes.nonEmpty

  override def isEmpty: Boolean = getBody.isEmpty
}

class MessagePart(content: MessageContent, message: MessageData, index: Int)(implicit ui: UiModule) extends Part {
  lazy val linkIndex = message.content.take(index).count(_.tpe == Part.Type.WEB_LINK)
  lazy val linkPreview = message.protos.lastOption flatMap {
    case TextMessage(_, _, previews) if previews.size > linkIndex => Some(previews(linkIndex))
    case _ => None
  }

  lazy val image = (content.tpe, content.asset, content.openGraph, linkPreview) match {
    case (Part.Type.ASSET, Some(assetId), _, _) => ui.images.getImageAsset(assetId)
    case (Part.Type.WEB_LINK, _, _, Some(LinkPreview.WithAsset(_))) => ui.images.getImageAsset(message.assetId) //TODO Dean: bit ugly, what if we want multiple assets per message?
    case (Part.Type.WEB_LINK, _, Some(OpenGraphData(_, _, Some(uri), _, _)), None) => ImageAssetFactory.getImageAsset(uri)
    case _ => ImageAsset.Empty
  }

  lazy val dimensions = linkPreview match {
    case Some(LinkPreview.WithAsset(AssetData.WithDimensions(d))) => d
    case _ => Dim2(content.width, content.height)
  }

  lazy val openGraph = (linkPreview, content.openGraph) match {
    case (Some(LinkPreview.WithDescription(title, summary)), _) => OpenGraphData(title, summary, None, "", None)
    case (_, Some(data)) => data
    case _ => OpenGraphData.Empty
  }

  override def getPartType = content.tpe match {
    case Part.Type.WEB_LINK if openGraph == OpenGraphData.Empty => Part.Type.TEXT
    case _ => content.tpe
  }

  override def getBody = content.content
  override def getImageWidth = dimensions.width
  override def getImageHeight = dimensions.height
  override def getImage: api.ImageAsset = image

  override def getTitle = openGraph.title
  override def getDescription = openGraph.description
  override def getPermanentUri: URI = openGraph.permanentUrl.orNull
  override def getContentUri: URI = content.contentAsUri

  override lazy val getMediaAsset: api.MediaAsset = content.richMedia .map { media =>
    if (media.hasExpired) ui.zms(_.sync.syncRichMedia(message.id, Priority.High))
    new MediaAsset(media)(ui)
  } .orNull
}

object EmptyMessage extends com.waz.api.Message {
  override def getId: String = Uid(0, 0).str
  override val getMentionedUsers: Array[api.User] = Array.empty
  override def isUserMentioned: Boolean = false
  override def isFirstMessage: Boolean = false
  override def isLastMessageFromSelf: Boolean = false
  override def isLastMessageFromOther: Boolean = false
  override def getBody: String = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  override def getLocalTime: Instant = MessageData.UnknownInstant
  override def getImageWidth: Int = 0
  override def retry(): Unit = ()
  override def getImage: api.ImageAsset = ImageAsset.Empty
  override def getImage(w: Int, h: Int): api.ImageAsset = ImageAsset.Empty
  override def getAsset = Asset.Empty
  override val getMembers: Array[api.User] = Array.empty
  override def getUser: api.User = EmptyUser
  override def getNewConversationName: String = ""
  override def getMessageStatus: Status = Status.DEFAULT
  override def getImageHeight: Int = 0
  override def getTime: Instant = MessageData.UnknownInstant
  override def getConversation: IConversation = null
  override def getConversationId = ""
  override def isHotKnock: Boolean = false
  override def getMessageType = com.waz.api.Message.Type.TEXT
  override def isDeleted: Boolean = false
  override def removeUpdateListener(listener: UpdateListener): Unit = {}
  override def addUpdateListener(listener: UpdateListener): Unit = {}
  override def isCreateConversation: Boolean = false
  override def writeToParcel(dest: Parcel, flags: Int): Unit = {
    dest.writeString(JsonEncoder.encodeString(MessageData.Empty))
    dest.writeStringArray(Array.empty[String])
    dest.writeInt(if (isLikedByThisUser) 1 else 0)
    dest.writeInt(0)
  }
  override def getLocation: Location = null
  override def describeContents(): Int = 0
  override val getLikes: Array[api.User] = Array.empty
  override def unlike(): Unit = ()
  override def like(): Unit = ()
  override def isLikedByThisUser: Boolean = false
  override def isLiked: Boolean = false
  override def isEmpty: Boolean = true
  override def delete(): Unit = ()
  override def recall(): Unit = ()
  override def getEditTime: Instant = MessageData.UnknownInstant
  override def isEdited: Boolean = false
  override def update(content: Text): Unit = ()
  override def isEphemeral: Boolean = false
  override def isExpired: Boolean = false
  override def getEphemeralExpiration = EphemeralExpiration.NONE
  override def getExpirationTime: Instant = Instant.MAX

  override val getParts: Array[Part] = Array.empty
}
