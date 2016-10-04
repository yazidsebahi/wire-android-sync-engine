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

public enum EphemeralExpiration {

    NONE(0), FIVE_SECONDS(5 * 1000), FIFTEEN_SECONDS(15 * 1000), ONE_MINUTE(60 * 1000), FIFTEEN_MINUTES(15 * 60 * 1000);

    public long milliseconds;

    EphemeralExpiration(long millis) {
        this.milliseconds = millis;
    }

    public static EphemeralExpiration getForMillis(long millis) {
        for (EphemeralExpiration exp : values()) {
            if (exp.milliseconds >= millis) {
                return exp;
            }
        }
        return FIFTEEN_MINUTES;
    }
}
