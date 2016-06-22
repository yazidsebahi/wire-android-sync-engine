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
import com.waz.api.Message.{Part, Status}
import com.waz.api.{IConversation, UpdateListener}
import com.waz.model._
import com.waz.model.sync.SyncJob.Priority
import com.waz.service.messages.MessageAndLikes
import com.waz.threading.Threading
import com.waz.ui._
import com.waz.utils._
import org.threeten.bp.Instant

import scala.collection.breakOut

class Message(val id: MessageId, var data: MessageData, var likes: IndexedSeq[UserId], var likedBySelf: Boolean)(context: UiModule) extends api.Message with UiObservable {

  private implicit val logTag: LogTag = logTagFor[Message]
  private var parts = Array.empty[Part]

  def this(msg: MessageAndLikes)(context: UiModule) = this(msg.message.id, msg.message, msg.likes, msg.likedBySelf)(context)
  def this(id: MessageId)(context: UiModule) = this(id, MessageData.Empty, Vector.empty, false)(context)

  updateParts()
  reload() // always reload because data from constructor might always be outdated already

  def reload() = context.zms.flatMapFuture(_.msgAndLikes.getMessageAndLikes(id))(Threading.Background).map(_ foreach set)(Threading.Ui)

  def set(msg: MessageAndLikes): Unit = if (msg.message != data || msg.likes != likes || msg.likedBySelf != likedBySelf) {
    data = msg.message
    likes = msg.likes
    likedBySelf = msg.likedBySelf
    updateParts()
    notifyChanged()
  }

  private def updateParts(): Unit = parts = data.content.map { c =>
    new Part {
      override val getPartType: Part.Type = c.tpe
      override val getBody: String = c.content
      override val getImageWidth: Int = c.width
      override val getImageHeight: Int = c.height
      override val getImage: api.ImageAsset = c.asset.filter(_ => c.tpe == api.Message.Part.Type.ASSET).fold2(ImageAsset.Empty, context.images.getImageAsset)

      override val getMediaAsset: api.MediaAsset = c.richMedia .map { media =>
        if (media.hasExpired) context.zms(_.sync.syncRichMedia(id, Priority.High))
        new MediaAsset(media)(context)
      } .orNull
    }
  } (breakOut)

  private def content = if (data.content.isEmpty) MessageContent.Empty else data.content.head

  override def getParts: Array[Part] = parts

  override def getMentionedUsers =
    if (content.mentions.isEmpty) Array.empty
    else content.mentions.map { case (userId, _) => context.users.getUser(userId) } (breakOut)

  override def isUserMentioned: Boolean = context.users.selfUser.userId.exists(content.mentions.contains)

  override def isDeleted: Boolean = data.state == Status.DELETED

  override def isOtr: Boolean = true

  override def getTime: Instant = data.time

  override def getBody: String = if (data.msgType == api.Message.Type.RICH_MEDIA) data.contentString else content.content

  override def getImage = context.images.getImageAsset(data.assetId)

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

  override def isHotKnock: Boolean = data.hotKnock

  override def isFirstMessage: Boolean = data.firstMessage

  override def isCreateConversation: Boolean = data.msgType == api.Message.Type.MEMBER_JOIN && data.source.sequence == 1

  override def getLocalTime: Instant = data.localTime

  override def retry(): Unit =
    if (data.state == Status.FAILED || data.state == Status.FAILED_READ) context.messages.retry(data.convId, data.id)
    else error(s"Retrying a message that has not yet failed (${data.state}): $id")

  override def delete(): Unit = context.zms.flatMapFuture(_.convsUi.deleteMessage(data.convId, id))

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

  override def like(): Unit = context.zms.flatMapFuture(_.likings.like(data.convId, id))
  override def unlike(): Unit = context.zms.flatMapFuture(_.likings.unlike(data.convId, id))
  override def isLikedByThisUser: Boolean = likedBySelf
  override def isLiked: Boolean = likes.nonEmpty

  override def isEmpty: Boolean = getBody.isEmpty
}

object EmptyMessage extends com.waz.api.Message {
  override def getId: String = Uid(0, 0).str
  override val getMentionedUsers: Array[api.User] = Array.empty
  override def isUserMentioned: Boolean = false
  override def isFirstMessage: Boolean = false
  override def getBody: String = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  override def getLocalTime: Instant = MessageData.UnknownInstant
  override def getImageWidth: Int = 0
  override def isOtr: Boolean = false
  override def retry(): Unit = ()
  override def getImage: api.ImageAsset = ImageAsset.Empty
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
  override def describeContents(): Int = 0
  override val getLikes: Array[api.User] = Array.empty
  override def unlike(): Unit = ()
  override def like(): Unit = ()
  override def isLikedByThisUser: Boolean = false
  override def isLiked: Boolean = false
  override def isEmpty: Boolean = true
  override def delete(): Unit = ()

  override val getParts: Array[Part] = Array.empty
}
