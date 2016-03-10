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
package com.waz.service.messages

import com.waz.ZLog._
import com.waz.content.{MessageLoader, Likes, LikingsStorage}
import com.waz.model._
import com.waz.service.UserService
import com.waz.sync.SyncServiceHandle
import com.waz.threading.Threading
import com.waz.utils.{RichOption, RichTraversableOnce, returning}
import org.threeten.bp.Instant.now

import scala.concurrent.Future
import scala.collection.breakOut
import scala.util.Failure

class LikingsService(storage: LikingsStorage, messages: MessagesContentUpdater, sync: SyncServiceHandle, users: UserService) extends MessageLoader {
  import LikingsService._
  import Threading.Implicits.Background

  override def apply(ids: Seq[MessageId]): Future[Seq[MessageAndLikes]] =
    messages.messagesStorage.getMessages(ids: _*) flatMap { msgs => withLikes(msgs.flatten) }

  def getMessageAndLikes(id: MessageId): Future[Option[MessageAndLikes]] =
    messages.getMessage(id).flatMap(_.fold2(Future.successful(None), msg => combineWithLikes(msg).map(Some(_))))

  def combineWithLikes(msg: MessageData): Future[MessageAndLikes] =
    users.withSelfUserFuture(u => storage.getLikes(msg.id).map(l => combine(msg, l, u)))

  def withLikes(msgs: Seq[MessageData]): Future[Seq[MessageAndLikes]] = {
    val ids: Set[MessageId] = msgs.map(_.id)(breakOut)
    users.withSelfUserFuture { selfUserId =>
      storage.loadAll(msgs.map(_.id)).map { likes =>
        val likesById = likes.by[MessageId, Map](_.message)
        returning(msgs.map(msg => combine(msg, likesById(msg.id), selfUserId))) { _ =>
          verbose(s"combined ${ids.size} message(s) with ${likesById.size} liking(s)")
        }
      }
    }.andThen { case Failure(t) => error("failed while adding likings to messages", t) }
  }

  def combine(msg: MessageData, likes: Likes, selfUserId: UserId): MessageAndLikes =
    if (likes.likers.isEmpty) MessageAndLikes(msg, Vector(), false)
    else sortedLikes(likes, selfUserId) match { case (likers, selfLikes) => MessageAndLikes(msg, likers, selfLikes) }

  def sortedLikes(likes: Likes, selfUserId: UserId): (IndexedSeq[UserId], Boolean) =
    (likes.likers.toVector.sortBy(_._2).map(_._1), likes.likers contains selfUserId)

  def like(conv: ConvId, msg: MessageId): Future[Likes] = addLiking(conv, msg, Liking.Action.Like)

  def unlike(conv: ConvId, msg: MessageId): Future[Likes] = addLiking(conv, msg, Liking.Action.Unlike)

  private def addLiking(conv: ConvId, msg: MessageId, action: Liking.Action): Future[Likes] = {
    verbose(s"addLiking: $conv $msg, $action")
    for {
      liking <- users.withSelfUserFuture(self => Future.successful(Liking(msg, self, now, action)))
      likes  <- storage.addOrUpdate(liking)
      _      <- sync.postLiking(conv, liking)
    } yield likes
  }

  def processLiking(likings: Seq[Liking]): Future[Seq[Likes]] = Future.traverse(likings) { storage.addOrUpdate } // FIXME: use batching
}

object LikingsService {
  private implicit val logTag: LogTag = logTagFor[LikingsService]
}

case class MessageAndLikes(message: MessageData, likes: IndexedSeq[UserId], likedBySelf: Boolean)
object MessageAndLikes extends ((MessageData, IndexedSeq[UserId], Boolean) => MessageAndLikes) {
  val Empty = MessageAndLikes(MessageData.Empty, Vector.empty, false)
}
