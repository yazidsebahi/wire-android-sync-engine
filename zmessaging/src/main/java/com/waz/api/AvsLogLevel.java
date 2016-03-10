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

import com.waz.call.FlowManager;

public enum AvsLogLevel {
    DEBUG(FlowManager.LOG_LEVEL_DEBUG),
    INFO(FlowManager.LOG_LEVEL_INFO),
    WARN(FlowManager.LOG_LEVEL_WARN),
    ERROR(FlowManager.LOG_LEVEL_ERROR);

    public final int priority;

    AvsLogLevel(int priority) {
        this.priority = priority;
    }

    public static AvsLogLevel fromPriority(int priority) {
        for (AvsLogLevel value: AvsLogLevel.values()) {
            if (value.priority == priority) {
                return value;
            }
        }
        throw new IllegalArgumentException("unknown/invalid priority: " + priority);
    }
}
