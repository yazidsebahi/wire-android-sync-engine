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

trait ActiveVoiceChannels extends UiObservable {

    /** Returns whether the current information in this object is up-to-date. */
    def isUpToDate: Boolean

    /** Returns whether there is an ongoing or outgoing call currently. */
    def hasOngoingCall: Boolean

    /** Returns the ongoing or outgoing call, if any, or <code>null</code> otherwise. */
    def getOngoingCall: VoiceChannel

    /** Returns whether there is at least one incoming call currently. */
    def hasIncomingCall: Boolean

    /** Returns the current incoming call, if any, or <code>null</code> otherwise.
      * In the case of multiple incoming calls, returns the first one.
      */
    def getIncomingCall: VoiceChannel
}
