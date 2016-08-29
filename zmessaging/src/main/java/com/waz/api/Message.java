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

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable;
import com.waz.service.ZMessaging$;
import org.threeten.bp.Instant;

public interface Message extends UiObservable, Parcelable {

    Parcelable.Creator<Message> CREATOR = new Parcelable.Creator<Message>() {
        @Override
        public Message createFromParcel(Parcel source) {
            return ZMessaging$.MODULE$.currentUi().messages().cachedOrFromParcel(source);
        }

        @Override
        public Message[] newArray(int size) {
            return new Message[size];
        }
    };

    enum Status {
        DEFAULT, SENT, PENDING, DELETED, FAILED, FAILED_READ, DELIVERED;

        public boolean isFailed() {
            return this == FAILED;
        }
    }

    enum Type {
        TEXT, TEXT_EMOJI_ONLY, ASSET, ANY_ASSET, VIDEO_ASSET, AUDIO_ASSET, KNOCK, MEMBER_JOIN, MEMBER_LEAVE, CONNECT_REQUEST,
        CONNECT_ACCEPTED, RENAME, MISSED_CALL, INCOMING_CALL, RICH_MEDIA, OTR_ERROR, OTR_IDENTITY_CHANGED, OTR_VERIFIED, OTR_UNVERIFIED,
        OTR_DEVICE_ADDED, STARTED_USING_DEVICE, HISTORY_LOST, LOCATION, UNKNOWN, RECALLED
    }

    /**
     * If #getMessageType() == Type.RICH_MEDIA, returns the individual parts of this message. Otherwise, the returned array is empty.
     */
    Part[] getParts();

    interface Part {
        enum Type {
            TEXT, TEXT_EMOJI_ONLY, ASSET, ANY_ASSET, YOUTUBE, SOUNDCLOUD, TWITTER, SPOTIFY, WEB_LINK, GOOGLE_MAPS
        }

        Part.Type getPartType();

        String getBody();

        /**
         * Returns OpenGraph title if available.
         * @return empty string when no metadata is available.
         */
        String getTitle();
        String getDescription();

        /**
         * Returns Uri parsed from part body, will only be valid for WEB_LINK parts.
         */
        Uri getContentUri();

        /**
         * Returns permanen url property from open graph data, if available.
         */
        Uri getPermanentUri();

        ImageAsset getImage();
        int getImageWidth();
        int getImageHeight();
        MediaAsset getMediaAsset();
    }

    static final Message EMPTY = com.waz.api.impl.EmptyMessage$.MODULE$;

    String getId();
    String getConversationId();
    IConversation getConversation();
    Type getMessageType();
    Status getMessageStatus();
    User getUser();
    Asset getAsset();
    String getBody();
    Instant getTime();

    /**
     * Returns last time this message was edited (or recalled).
     * @return
     */
    Instant getEditTime();

    boolean isDeleted();
    boolean isEmpty();
    boolean isHotKnock();

    /***
     * true if message was edited.
     */
    boolean isEdited();

    ImageAsset getImage();

    /**
     * Returns image asset with specified max dimensions.
     * Given dimensions are only a suggestion, it will be used only in special cases (like location map),
     * the goal is to request specific aspect ratio, so we don't fetch too much if UI is going to crop it anyway.
     */
    ImageAsset getImage(int width, int height);

    /**
     * Returns true for first member join message in conversation.
     * This is essentially different, create conversation, message type.
     */
    boolean isCreateConversation();

    /**
     * @deprecated - all messages are OTR now
     */
    boolean isOtr();

    /**
     * Returns whether this message is the first "real" one in a new conversation.
     * This means the first message after a user connection is established, excluding
     * the initial member joins or the connect request message itself, but including knocks.
     */
    boolean isFirstMessage();

    /**
     * @return whether this message is the last (or "most recent") message sent from this user to the conversation
     *         this message belongs to
     */
    boolean isLastMessageFromSelf();

    /**
     * Returns true if self user was mentioned in this message.
     */
    boolean isUserMentioned();

    User[] getMentionedUsers();

    /**
     * Returns users (including self) that like this message.
     */
    User[] getLikes();

    /**
     * Indicates whether self user likes this message.
     */
    boolean isLikedByThisUser();

    /**
     * Indicates whether any user (including self) likes this messages.
     */
    boolean isLiked();

    void like();
    void unlike();

    /**
     * Retry sending a previously failed message.
     */
    void retry();

    /**
     * Deletes message on users devices. Will not request deletion on the other side.
     */
    void delete();

    /**
     * Deletes message globally. Can only be used on users own messages.
     */
    void recall();

    /**
     * Edits message - replaces it with given content.
     * Only text messages can be edited, only by message creator.
     */
    void update(MessageContent.Text content);

    /**
     * Returns local time when this message was received.
     * This timestamp is only relevant for messages received through push channel, will be set to Instant.EPOCH for other messages.
     * This value should only be used with knocks to check if knock has just been received - for sounds and animations.
     */
    Instant getLocalTime();

    /**
     * Returns shared location.
     * @return location or null if message doesn't contain location info
     */
    MessageContent.Location getLocation();

    /**
     * For message type {@link Type.RENAME} which notifies that a conversation has been renamed.
     *
     * @return The new name of the conversation.
     */
    String getNewConversationName();

    int getImageWidth();
    int getImageHeight();
    User[] getMembers();
}
