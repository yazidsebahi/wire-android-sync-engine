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

import com.waz.ZLog._
import com.waz.api
import com.waz.api._
import com.waz.api.impl.otr.OtrClients
import com.waz.model._
import com.waz.model.otr.Client
import com.waz.threading.Threading
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.Signal
import com.waz.znet.ZNetClient.ErrorOrResponse

import scala.util.{Failure, Success}

class Self(var zuser: Option[ZUser] = None, var user: Option[User] = None)(implicit ui: UiModule) extends com.waz.api.Self with UiObservable with SignalLoading {

  private implicit val logTag: LogTag = logTagFor[Self]
  private var upToDate = true
  private var otrRegState = ClientRegistrationState.UNKNOWN
  private var trackingId: Option[TrackingId] = None

  private def users = ui.users

  val userUpdateListener = new UpdateListener {
    override def updated(): Unit = notifyChanged()
  }

  addLoader({ zms => Signal(zms.user.signal, zms.users.selfUser, zms.blacklist.upToDate, zms.otrContent.registrationStateSignal).map(Some(_)) }, Option.empty[(ZUser, UserData, Boolean, ClientRegistrationState)]) {
    case Some((zuser, user, upToDate, otrRegState)) =>
      debug(s"onLoaded: ${(zuser, user, upToDate, otrRegState)}")
      val previousState = (this.user, this.zuser, this.upToDate, this.otrRegState)
      this.zuser = Some(zuser)
      this.upToDate = upToDate
      updateUser(user, otrRegState)
      if (previousState != (this.user, this.zuser, this.upToDate, this.otrRegState)) notifyChanged()
    case None =>
      user foreach (_.removeUpdateListener(userUpdateListener))
      val shouldNotify = user.isDefined || zuser.isDefined
      user = None
      zuser = None
      if (shouldNotify) notifyChanged()
  }

  def updateUser(u: UserData, otrRegState: ClientRegistrationState): Unit = {
    trackingId = u.trackingId
    this.otrRegState = otrRegState
    val user = users.getUser(u)
    if (!this.user.contains(user)) {
      this.user foreach (_.removeUpdateListener(userUpdateListener))
      this.user = Some(user)
      this.user foreach (_.addUpdateListener(userUpdateListener))
    }
  }

  private[impl] def userId = user.map(_.data.id)

  override def getClientRegistrationState = otrRegState

  override def getOtherOtrClients = user.fold[CoreList[OtrClient]](OtrClients.Empty) { u => OtrClients(UserId(u.getId), skipSelf = true) }

  override def getIncomingOtrClients = OtrClients.incoming

  override def getOtrClient: api.UiSignal[api.OtrClient] =
    new UiSignal(
      zms => Signal(zms.users.selfUser, zms.otrClientsService.selfClient),
      { p: (UserData, Client) => new otr.OtrClient(p._1.id, p._2.id, p._2) }
    )

  override def isLoggedIn = zuser.isDefined

  override def isEmailVerified: Boolean = zuser.exists(_.emailVerified)

  override def isPhoneVerified: Boolean = zuser.exists(_.phoneVerified)

  override def isUpToDate: Boolean = upToDate

  override def resendVerificationEmail(email: String): Unit = users.requestVerificationEmail(EmailAddress(email))

  override def getUser: api.User = user.orNull

  override def getName: String = user.fold("")(_.getName)

  override def getAccent: AccentColor = user.fold(AccentColor())(_.getAccent)

  override def getEmail: String = user.map(_.getEmail).orElse(zuser.flatMap(_.email map (_.str))).getOrElse("")

  override def getPhone: String = user.map(_.getPhone).orElse(zuser.flatMap(_.phone map (_.str))).getOrElse("")

  override def getPicture = user.fold[api.ImageAsset](ImageAsset.Empty)(_.getPicture)

  override def setAccent(color: api.AccentColor): Unit = users.setSelfColor(AccentColor(color), user)

  override def getTrackingId: String = trackingId.map(_.str).orNull

  override def setName(name: String): Unit = users.setSelfName(name, user)

  override def setPicture(image: api.ImageAsset): Unit = users.setSelfPicture(image)

  override def clearPicture(): Unit = users.clearSelfPicture()

  override def setEmail(email: String, listener: CredentialsUpdateListener): Unit = handlingErrors(users.setSelfEmail(EmailAddress(email), user), listener)
  override def setPhone(phone: String, listener: CredentialsUpdateListener): Unit = handlingErrors(users.setSelfPhone(PhoneNumber(phone), user), listener)
  override def updatePassword(newPassword: String, currentPassword: String, listener: CredentialsUpdateListener): Unit = handlingErrors(users.updatePassword(newPassword, Option(currentPassword)), listener)
  override def setPassword(password: String, listener: CredentialsUpdateListener): Unit = handlingErrors(users.updatePassword(password, None), listener)

  override def deleteAccount(): Unit = ui.zms.flatMapFuture(_.users.deleteAccount())

  private def handlingErrors[T](request: ErrorOrResponse[Unit], listener: CredentialsUpdateListener): Unit = request.onComplete {
    case Success(Right(())) => listener.onUpdated()
    case Success(Left(ErrorResponse(code, message, label))) => listener.onUpdateFailed(code, message, label)
    case Failure(ex) => listener.onUpdateFailed(499, ex.getMessage, "")
  } (Threading.Ui)
}
