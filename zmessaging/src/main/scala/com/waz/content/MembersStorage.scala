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
package com.waz.content

import android.content.Context
import com.waz.ZLog._
import com.waz.model.ConversationMemberData.ConversationMemberDataDao
import com.waz.model._
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.{AggregatingSignal, Signal}
import com.waz.utils.{CachedStorage, TrimmingLruCache}

import scala.collection._
import scala.concurrent.Future

class MembersStorage(context: Context, storage: ZmsDatabase) extends CachedStorage[(UserId, ConvId), ConversationMemberData](new TrimmingLruCache(context, Fixed(1024)), storage)(ConversationMemberDataDao, "MembersStorage_Cached") {
  private implicit val dispatcher = new SerialDispatchQueue(name = "MembersStorage")

  def get(conv: ConvId) = find(_.convId == conv, ConversationMemberDataDao.findForConv(conv)(_), identity)

  def getActive(conv: ConvId) = find({ m => m.active && m.convId == conv }, ConversationMemberDataDao.findActiveForConv(conv)(_), identity)

  def get(user: UserId) = find(_.userId == user, ConversationMemberDataDao.findForUser(user)(_), identity)

  def getActive(user: UserId) = find({ m => m.active && m.userId == user }, ConversationMemberDataDao.findActiveForUser(user)(_), identity)

  def activeMembers(conv: ConvId): Signal[Set[UserId]] = new AggregatingSignal[Seq[ConversationMemberData], Set[UserId]](onConvMemberChanged(conv), getActiveUsers(conv).map(_.toSet), { (current, changes) =>
    val (active, inactive) = changes.partition(_.active)

    current -- inactive.map(_.userId) ++ active.map(_.userId)
  })

  private def onConvMemberChanged(conv: ConvId) = onChanged.map(_.filter(_.convId == conv)).filter(_.nonEmpty)

  def getActiveUsers(conv: ConvId): Future[Seq[UserId]] = getActive(conv) map { _.map(_.userId) }

  def getActiveConvs(user: UserId): Future[Seq[ConvId]] = getActive(user) map { _.map(_.convId) }

  def add(conv: ConvId, event: EventId, users: UserId*): Future[Set[ConversationMemberData]] =
    updateOrCreateAll2(users.map((_, conv)), { (k, v) =>
      v match {
        case None     => ConversationMemberData(k._1, conv, active = true, event)
        case Some(m)  => m.copy(active = true, eventId = event)
      }
    })

  def remove(conv: ConvId, event: EventId, users: UserId*): Future[Seq[ConversationMemberData]] =
    updateAll2(users.map(_ -> conv), _.copy(active = false, eventId = event)).map(_.map(_._2))

  def add(conv: ConvId, users: UserId*): Future[Set[ConversationMemberData]] =
    updateOrCreateAll2(users.map(_ -> conv), { (k, v) => v.fold(ConversationMemberData(k._1, conv, active = true)){_.copy(active = true)} })

  def remove(conv: ConvId, users: UserId*): Future[Seq[ConversationMemberData]] =
    updateAll2(users.map(_ -> conv), _.copy(active = false)).map(_.map(_._2))


  def set(conv: ConvId, event: EventId, users: Seq[UserId]): Future[Unit] = getActiveUsers(conv) flatMap { active =>
    val usersSet = users.toSet
    val toRemove = active.filterNot(usersSet)
    val toAdd = usersSet -- toRemove

    remove(conv, toRemove: _*).zip(add(conv, toAdd.toSeq: _*)).map(_ => ())
  }

  def isActiveMember(conv: ConvId, user: UserId) = get(user -> conv) map (_.exists(_.active))

  def delete(conv: ConvId) = get(conv) map { users => remove(users.map(_.userId -> conv)) }
}

object MembersStorage {
  private implicit val tag: LogTag = logTagFor[MembersStorage]
}
