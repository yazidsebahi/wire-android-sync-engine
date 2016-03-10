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
package com.waz.model

package object messages {

  import GenericMessage._

  object TextMessage {
    def apply(text: String, mentions: Map[UserId, String]): GenericMessage = GenericMessage(Uid(), Text(text, mentions))

    def apply(msg: MessageData): GenericMessage = GenericMessage(Uid(msg.id.str), Text(msg.contentString, msg.content.flatMap(_.mentions).toMap))

    def unapply(msg: GenericMessage): Option[(String, Map[UserId, String])] = msg match {
      case GenericMessage(_, Text(content, mentions)) => Some((content, mentions))
      case _ => None
    }
  }

  object LikingMessage {
    def apply(liking: Liking): GenericMessage = GenericMessage(Uid(liking.message.str), LikeAction(liking.action))

    def unapply(msg: GenericMessage): Option[(MessageId, Liking.Action)] = msg match {
      case GenericMessage(m, LikeAction(l)) => Some((MessageId(m.str), l))
      case _ => None
    }
  }
}
