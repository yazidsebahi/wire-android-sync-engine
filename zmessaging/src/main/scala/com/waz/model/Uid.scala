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

import java.nio.ByteBuffer
import java.util.UUID

import com.waz.utils.{JsonDecoder, JsonEncoder}
import org.json.JSONObject

trait Id[A] extends Ordering[A] {
  def random(): A
  def decode(str: String): A
  def encode(id: A): String = id.toString

  override def compare(x: A, y: A): Int = Ordering.String.compare(encode(x), encode(y))
}

case class Uid(str: String) {
  override def toString: String = str
}

object Uid {
  def apply(): Uid = Uid(UUID.randomUUID().toString)
  def apply(mostSigBits: Long, leastSigBits: Long): Uid = Uid(new UUID(mostSigBits, leastSigBits).toString)

  implicit object UidId extends Id[Uid] {
    override def random(): Uid = Uid()
    override def decode(str: String): Uid = Uid(str)
  }
}

case class UserId(str: String) {
  def bytes = {
    val uuid = UUID.fromString(str)
    val bytes = Array.ofDim[Byte](16)
    val bb = ByteBuffer.wrap(bytes).asLongBuffer()
    bb.put(uuid.getMostSignificantBits)
    bb.put(uuid.getLeastSignificantBits)
    bytes
  }
  override def toString: String = str
}

object UserId extends (String => UserId) {
  val Zero = UserId(new UUID(0, 0).toString)

  def apply(): UserId = Id.random()

  implicit object Id extends Id[UserId] {
    override def random(): UserId = UserId(Uid().toString)
    override def decode(str: String): UserId = UserId(str)
  }

  implicit lazy val UserIdDecoder: JsonDecoder[UserId] = new JsonDecoder[UserId] {
    override def apply(implicit o: JSONObject): UserId = UserId(o.getString("userId"))
  }

  implicit lazy val UserIdEncoder: JsonEncoder[UserId] = new JsonEncoder[UserId] {
    override def apply(id: UserId): JSONObject = JsonEncoder { _.put("userId", id.str) }
  }
}

case class ZUserId(str: String) {
  override def toString: String = str
}

object ZUserId {
  def apply(): ZUserId = Id.random()

  implicit object Id extends Id[ZUserId] {
    override def random(): ZUserId = ZUserId(Uid().toString)
    override def decode(str: String): ZUserId = ZUserId(str)
  }
}

case class AssetId(str: String) {
  override def toString: String = str
}

object AssetId extends (String => AssetId) {
  def apply(): AssetId = Id.random()

  implicit object Id extends Id[AssetId] {
    override def random() = AssetId(Uid().toString)
    override def decode(str: String) = AssetId(str)
  }
}

case class RAssetDataId(str: String) {
  override def toString: String = str
}

object RAssetDataId {
  val Empty = RAssetDataId("empty")
  def apply(): RAssetDataId = Id.random()

  implicit object Id extends Id[RAssetDataId] {
    override def random() = RAssetDataId(Uid().toString)
    override def decode(str: String) = RAssetDataId(str)
  }
}

case class MessageId(str: String) {
  override def toString: String = str
}

object MessageId {
  val Empty = MessageId("")

  def apply(): MessageId = Id.random()
  def apply(uid: Uid): MessageId = MessageId(uid.str)

  implicit object Id extends Id[MessageId] {
    override def random() = MessageId(Uid().toString)
    override def decode(str: String) = MessageId(str)
  }
}

case class ConvId(str: String) {
  override def toString: String = str
}

object ConvId extends (String => ConvId) {
  def apply(): ConvId = Id.random()

  implicit object Id extends Id[ConvId] {
    override def random(): ConvId = ConvId(Uid().toString)
    override def decode(str: String): ConvId = ConvId(str)
  }
}

case class RConvId(str: String) {
  override def toString: String = str
}

object RConvId {
  val Empty = RConvId("")
  def apply(): RConvId = Id.random()

  implicit object Id extends Id[RConvId] {
    override def random(): RConvId = RConvId(Uid().toString)
    override def decode(str: String): RConvId = RConvId(str)
  }
}

case class SyncId(str: String) {
  override def toString: String = str
}

object SyncId {
  def apply(): SyncId = Id.random()

  implicit object Id extends Id[SyncId] {
    override def random(): SyncId = SyncId(Uid().toString)
    override def decode(str: String): SyncId = SyncId(str)
  }
}

case class GcmId(str: String) {
  override def toString: String = str
}

object GcmId {
  def apply(): GcmId = Id.random()

  implicit object Id extends Id[GcmId] {
    override def random(): GcmId = GcmId(Uid().toString)
    override def decode(str: String): GcmId = GcmId(str)
  }
}

case class TrackingId(str: String) {
  override def toString: String = str
}

object TrackingId {
  def apply(): TrackingId = Id.random()

  implicit object Id extends Id[TrackingId] {
    override def random(): TrackingId = TrackingId(Uid().toString)
    override def decode(str: String): TrackingId = TrackingId(str)
  }
}

case class CallSessionId(str: String) {
  override def toString: String = str
}

object CallSessionId extends (String => CallSessionId) {
  def apply(): CallSessionId = Id.random()

  implicit object Id extends Id[CallSessionId] {
    override def random(): CallSessionId = CallSessionId(Uid().toString)
    override def decode(str: String): CallSessionId = CallSessionId(str)
  }

  implicit object DefaultOrdering extends Ordering[CallSessionId] {
    def compare(a: CallSessionId, b: CallSessionId): Int = Ordering.String.compare(a.str, b.str)
  }
}

case class ContactId(str: String) {
  override def toString: String = str
}

object ContactId extends (String => ContactId) {
  def apply(): ContactId = Id.random()

  implicit object Id extends Id[ContactId] {
    override def random(): ContactId = ContactId(Uid().toString)
    override def decode(str: String): ContactId = ContactId(str)
  }
}

case class InvitationId(str: String) {
  override def toString: String = str
}

object InvitationId extends (String => InvitationId) {
  def apply(): InvitationId = Id.random()

  implicit object Id extends Id[InvitationId] {
    override def random(): InvitationId = InvitationId(Uid().toString)
    override def decode(str: String): InvitationId = InvitationId(str)
  }
}
