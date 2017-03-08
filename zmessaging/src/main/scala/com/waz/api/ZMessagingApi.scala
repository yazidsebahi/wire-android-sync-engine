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
package com.waz.api

import android.content.Context
import android.net.Uri
import com.waz.api.ZMessagingApi.PhoneConfirmationCodeRequestListener
import com.waz.media.manager.MediaManager

object ZMessagingApi {

  trait RegistrationListener {
    def onRegistered(user: Self): Unit
    def onRegistrationFailed(code: Int, message: String, label: String): Unit
  }

  trait PhoneConfirmationCodeRequestListener {
    def onConfirmationCodeSent(kindOfAccess: KindOfAccess): Unit
    def onPasswordExists(kindOfAccess: KindOfAccess): Unit
    def onConfirmationCodeSendingFailed(kindOfAccess: KindOfAccess, code: Int, message: String, label: String): Unit
  }

  trait PhoneNumberVerificationListener {
    def onVerified(kindOfVerification: KindOfVerification): Unit
    def onVerificationFailed(kindOfVerification: KindOfVerification, code: Int, message: String, label: String): Unit
  }
}

trait ZMessagingApi {

  def onCreate(context: Context): Unit

  def onResume(): Unit

  def onPause(): Unit

  def onDestroy(): Unit

  def onInit(callback: InitListener): Unit

  def setPermissionProvider(p: PermissionProvider): Unit
  def removePermissionProvider(p: PermissionProvider): Unit

  def login(credentials: Credentials, listener: LoginListener): Unit

  def logout(): Unit

  def register(credentials: Credentials, name: String, accent: AccentColor, listener: ZMessagingApi.RegistrationListener): Unit

  /**
   * Request a confirmation code for a given phone number. The code will be sent to the given phone number via SMS.
   * This can be used during registration as well as login. If this is called during registration, but the phone is already
   * registered, or if this is called during login, but the phone is not yet registered, this will fail with a conflict (HTTP status 409).
   */
  def requestPhoneConfirmationCode(phoneNumber: String, kindOfAccess: KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Unit

  def requestPhoneConfirmationCall(phoneNumber: String, kindOfAccess: KindOfAccess, listener: PhoneConfirmationCodeRequestListener): Unit

  /**
   * Verify that the given combination of a phone number and a confirmation code is valid. Used either in the registration process, in order to
   * pre-verify the phone number even before the user entered a name and proper registration starts, or when the user adds/changes her phone number later on.
   */
  def verifyPhoneNumber(phoneNumber: String, confirmationCode: String, kindOfVerification: KindOfVerification, listener: ZMessagingApi.PhoneNumberVerificationListener): Unit

  def search(): Search

  def getSelf: Self

  def getConversations: ConversationsList

  def getCache: ZCache

  def getIncomingMessages: IncomingMessagesList

  def getActiveVoiceChannels: ActiveVoiceChannels

  def getUser(id: String): User

  def getMediaManager: MediaManager

  def getMediaResourceUri(name: String): Uri

  /** Lists the contacts from this phone's contact providers (if they have an associated email or phone number) blended
    * with Wire users.
    */
  def getContacts: Contacts

  def getInvitations: Invitations

  def getErrors: ErrorsList

  /**
   * Access point for methods that get passed through to AVS.
   */
  def getAvs: Avs

  def getGiphy: Giphy

  def getSpotify: Spotify

  def getConnectionIndicator: ConnectionIndicator

  def getUsernames: Usernames
}
