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
package com.waz.service.otr

import com.waz.api.ClientRegistrationState
import com.waz.model.otr.ClientId
import com.waz.service.KeyValueService
import com.waz.service.Preference.PrefCodec

import scala.util.Try

class OtrContentService(kvService: KeyValueService) {
  import OtrContentService._

  val clientIdPref = kvService.keyValuePref[Option[ClientId]](ClientIdPref, None)
  val registrationStatePref = kvService.keyValuePref[ClientRegistrationState](ClientRegStatePref, ClientRegistrationState.UNKNOWN)(RegStatePrefCodec)

  def currentClientId = clientIdPref()
  def currentClientIdSignal = clientIdPref.signal

  def registrationState = registrationStatePref()
  def registrationStateSignal = registrationStatePref.signal
}

object OtrContentService {
  val ClientIdPref = "otr-client-id"
  val ClientRegStatePref = "otr-client-reg-state"

  implicit object RegStatePrefCodec extends PrefCodec[ClientRegistrationState] {
    override def encode(v: ClientRegistrationState): String = v.name()
    override def decode(str: String): ClientRegistrationState =
      Try(ClientRegistrationState.valueOf(str)).getOrElse(ClientRegistrationState.UNKNOWN)
  }
}
