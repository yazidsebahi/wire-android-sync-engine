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

public interface MessageContent<T> {

    User[] EmptyMentions = new User[0];

    T getContent();
    User[] getMentions();

    class Text implements MessageContent<String> {
        private final String content;
        private final User[] mentions;

        public Text(String content, User[] mentioned) {
            this.content = content;
            this.mentions = mentioned;
        }

        public Text(String content, User mentioned) {
            this.content = content;
            this.mentions = new User[] { mentioned };
        }

        public Text(String content) {
            this(content, EmptyMentions);
        }

        @Override
        public String getContent() {
            return content;
        }

        @Override
        public User[] getMentions() {
            return mentions;
        }
    }

    class Image implements MessageContent<ImageAsset> {
        private final ImageAsset content;

        public Image(ImageAsset file) {
            this.content = file;
        }

        @Override
        public ImageAsset getContent() {
            return content;
        }

        @Override
        public User[] getMentions() {
            return EmptyMentions;
        }
    }

    class Asset implements MessageContent<AssetForUpload> {
        private final AssetForUpload content;
        private final ErrorHandler handler;

        public Asset(AssetForUpload a, ErrorHandler eh) {
            this.content = a;
            this.handler = eh;
        }

        @Override public AssetForUpload getContent() { return content; }
        @Override public User[] getMentions() { return EmptyMentions; }
        public ErrorHandler getErrorHandler() { return handler; }

        public interface ErrorHandler {
            void noWifiAndFileIsLarge(long sizeInBytes, NetworkMode net, Answer answer);
        }

        public interface Answer {
            void ok();
            void cancel();
        }
    }
}
