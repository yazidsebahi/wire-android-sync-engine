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
package com.waz.model;

public enum CallProperty {
    Muted("muted"), Ignored("ignored"), Suspended("suspended"), SendsVideo("videod");
    
    public final String asJson;

    CallProperty(String serialized) {
        this.asJson = serialized;
    }

    public static CallProperty fromJson(String serialized) {
        for (CallProperty p : values()) {
            if (p.asJson.equals(serialized)) {
                return p;
            }
        }
        throw new IllegalArgumentException("unknown call property '" + serialized + "' encountered");
    }
}
