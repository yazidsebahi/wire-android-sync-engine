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
package com.waz.ui

import android.os.Parcel
import com.waz.Control.getOrUpdate
import com.waz.ZLog._
import com.waz.api
import com.waz.api.IConversation
import com.waz.api.impl._
import com.waz.model.ConversationData.ConversationType
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.threading.CancellableFuture
import com.waz.utils.{JsonDecoder, returning}
import com.waz.znet.ZNetClient.ErrorOrResponse

class Users(implicit ui: UiModule) {
  import ui.{convs, images, zms}

  private implicit val tag: LogTag = logTagFor[Users]

  def getPicture(imageId: Option[AssetId]): api.ImageAsset = imageId.fold[api.ImageAsset](ImageAsset.Empty)(images.getImageAsset)

  lazy val selfUser = new Self()

  val users = new UiCache[UserId, User](lruSize = 50)

  def getStorage = zms(_.storage)

  def getUser(id: UserId): User = getOrUpdate(users)(id, new User(id))

  def getUser(data: UserData): User = getOrUpdate(users)(data.id, new User(data.id, data))

  def getUser(p: Parcel): api.User = getUser(JsonDecoder.decode[UserData](p.readString()))

  def setSelfColor(color: AccentColor, user: Option[User]): Unit = {
    debug(s"setSelfColor $color, $user")

    user.foreach(_.update(accent = color.id))
    zms(_.users.updateSelf(accent = Some(color)))
  }

  def setSelfName(name: String, user: Option[User]): Unit = {
    debug(s"setSelfName $name, $user")

    user.foreach(_.update(name = name))
    zms(_.users.updateSelf(name = Some(name)))
  }

  def setSelfEmail(email: EmailAddress, user: Option[User]): ErrorOrResponse[Unit] = zms flatMap (_.users.updateEmail(email))

  def setSelfPhone(phone: PhoneNumber, user: Option[User]): ErrorOrResponse[Unit] = zms flatMap (_.users.updatePhone(phone))

  def updatePassword(newPassword: String, currentPassword: Option[String]): ErrorOrResponse[Unit] = zms flatMap { z =>
    import com.waz.threading.Threading.Implicits.Background
    z.users.updatePassword(newPassword, currentPassword) flatMap {
      case Right(_) =>
        z.user.update(z.user.user.copy(password = Some(newPassword)))
        CancellableFuture.lift(z.otrClientsService.awaitClientRegistered() map (_ => Right(())))
      case Left(err) => CancellableFuture successful Left(err)
    }
  }

  def setSelfPicture(image: com.waz.api.ImageAsset): Unit = zms(_.users.updateSelfPicture(image))

  def clearSelfPicture(): Unit = zms(_.users.clearSelfPicture())

  def connectToUser(user: User, message: String): Conversation = {
    zms(_.connection.connectToUser(user.id, message, user.getDisplayName)) // TODO: what if user is already connected, and what if its connected on backend already but we don't know it, we should handle this
    getFakeConv(user, ConversationType.WaitForConnection)
  }

  def acceptConnection(user: User): IConversation = {
    zms(_.connection.acceptConnection(user.id))
    getFakeConv(user, ConversationType.OneToOne).updateType(ConversationType.OneToOne)
  }

  def ignoreConnection(user: User): Unit = zms(_.connection.ignoreConnection(user.id))

  def cancelConnection(user: User): Unit = zms(_.connection.cancelConnection(user.id))

  def excludeFromPymk(user: User): Unit = zms(_.usersearch.excludeFromPymk(user.id))

  def blockUser(user: User): Unit = zms(_.connection.blockConnection(user.id))

  def unblockUser(user: User): IConversation = {
    zms(_.connection.unblockConnection(user.id))
    getFakeConv(user, ConversationType.OneToOne).updateType(ConversationType.OneToOne)
  }

  def getConversation(user: User): IConversation = {
    debug(s"getConversation($user)")
    zms(_.convsUi.getOrCreateOneToOneConversation(user.id)) // TODO: what if user is actually not connected
    getFakeConv(user, user.getConnectionStatus match {
      case ConnectionStatus.Self => ConversationType.Self
      case ConnectionStatus.Ignored => ConversationType.Incoming
      case _ => ConversationType.OneToOne })
  }

  /*
   * Returns conversation from 'fake' conversation data.
   * This ensures that returned conversation object will be preloaded with correct name and type, it also requests conv reload.
   * This is needed for UI, in some cases they want to have conv type set immediately without a need to wait for update.
   * If we have corresponding conversation in UI cache then this fake data will be ignored, otherwise it will be reloaded with correct data.
   */
  private def getFakeConv(user: User, tpe: ConversationType = ConversationType.OneToOne) = {
    returning(convs.getConversation(ConversationData(ConvId(user.id.str), RConvId(), Some(user.getDisplayName), user.id, tpe, generatedName = user.getDisplayName)))(_.reload())
  }

  def requestVerificationEmail(email: EmailAddress): Unit = zms(_.users.requestVerificationEmail(email))
}
