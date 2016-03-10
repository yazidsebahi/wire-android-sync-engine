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
package com.waz.api;

public enum VoiceChannelState {
    UNKNOWN,
    NO_ACTIVE_USERS,   // voice channel idle - no call
    SELF_CALLING,      // we are calling from current device
    OTHER_CALLING,     // someone else is calling
    SELF_JOINING,      // we are joining an already active channel
    SELF_CONNECTED,    // we are connected to call
    TRANSFER_READY,    // we are connected on other device, we can transfer
    TRANSFER_CALLING,  // we are calling from other device, we can transfer it already
    OTHERS_CONNECTED   // other users are in call - group call
}
