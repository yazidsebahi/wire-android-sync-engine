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

import android.app.PendingIntent;
import android.graphics.Bitmap;

public interface NotificationsHandler {

    interface NotificationsHandlerFactory {
        NotificationsHandler getNotificationsHandler();
        CallingEventsHandler getCallingEventsHandler();
        TrackingEventsHandler getTrackingEventsHandler();
    }

    interface GcmNotification {
        enum Type {
            CONNECT_REQUEST,
            CONNECT_ACCEPTED,
            CONTACT_JOIN,
            ASSET,
            ANY_ASSET,
            VIDEO_ASSET,
            AUDIO_ASSET,
            TEXT,
            MEMBER_JOIN,
            MEMBER_LEAVE,
            RENAME,
            KNOCK,
            MISSED_CALL,
            LIKE,
            MESSAGE_SENDING_FAILED;
        }

        enum LikedContent {
            TEXT_OR_URL, // the text or URL is contained in #getMessage in this case
            PICTURE
        }

        Type getType();

        @Deprecated
        String getContent(); // legacy string content

        /**
         * Text message content.
         */
        String getMessage();
        String getConversationId();
        String getConversationName();
        String getUserId();
        String getUserName();
        LikedContent getTypeOfLikedContent();
        boolean isGroupConversation();
        boolean isHotKnock();
        boolean isUserMentioned();
    }

    interface ActiveChannel {
        String getConversationName();
        String getConversationId();
        VoiceChannelState getState();
        String getCallerName(); // might return an empty string if the caller is unknown or unsync'ed
        Bitmap getPicture(); // returns preview image of caller (or an empty bitmap)
        KindOfCall getKindOfCall();
        boolean isVideoCall();

        PendingIntent getJoinActionIntent();
        PendingIntent getJoinWithVideoActionIntent();
        PendingIntent getLeaveActionIntent();
        PendingIntent getSilenceActionIntent();
    }

    public void updateGcmNotification(GcmNotificationsList notifications);

    /**
     * Returns information about currently active calls in case of any updates to them.
     *
     * @param ongoingCall - Active channel data of a potential ongoing call (or null if none is ongoing).
     * @param incomingCall - Active channel data of a potential incoming call (or null if none is incoming).
     * @param isUiActive - true if application is currently in foreground
     */
    public void updateOngoingCallNotification(ActiveChannel ongoingCall, ActiveChannel incomingCall, boolean isUiActive);
}
