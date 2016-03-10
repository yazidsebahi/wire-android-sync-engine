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

import java.util.NoSuchElementException;

import static android.Manifest.permission;
import static android.content.pm.PackageManager.PERMISSION_DENIED;
import static android.content.pm.PackageManager.PERMISSION_GRANTED;

public enum Permission {
    READ_CONTACTS(permission.READ_CONTACTS),
    READ_PHONE_STATE(permission.READ_PHONE_STATE),
    RECORD_AUDIO(permission.RECORD_AUDIO),
    WRITE_EXTERNAL_STORAGE(permission.WRITE_EXTERNAL_STORAGE);

    public final String id;

    private Permission(String id) {
        this.id = id;
    }

    public static Permission forId(String id) {
        for (Permission p : values()) {
            if (p.id.equals(id)) return p;
        }
        throw new NoSuchElementException("no permission for id: " + id);
    }

    public static enum Status {
        GRANTED(PERMISSION_GRANTED),
        DENIED(PERMISSION_DENIED);

        public final int id;

        private Status(int id) {
            this.id = id;
        }

        public static Status forId(int id) {
            switch(id) {
                case PERMISSION_GRANTED: return GRANTED;
                case PERMISSION_DENIED: return DENIED;
                default: throw new NoSuchElementException("no permission status for id: " + id);
            }
        }
    }
}
