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

public enum CauseForCallStateEvent {
    REQUESTED("requested"), INTERRUPTED("interrupted"), GONE("gone"), DISCONNECTED("disconnected"), ONGOING_CALL("ongoing-call"), FLOW_ERROR("flow-error");

    public String asJson;
    CauseForCallStateEvent(String asJson) {
        this.asJson = asJson;
    }

    public static CauseForCallStateEvent fromJson(String serialized) {
        for (CauseForCallStateEvent cause: values()) {
            if (cause.asJson.equals(serialized)) {
                return cause;
            }
        }
        throw new IllegalArgumentException("unknown call state cause: " + serialized);
    }
}
