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

import java.math.BigInteger
import java.nio.ByteBuffer
import java.util.UUID

import com.waz.utils.{JsonDecoder, JsonEncoder}
import org.json.JSONObject

import scala.util.Random

trait IdGen[A <: Id] extends Ordering[A] {
  def random(): A
  def decode(str: String): A
  def encode(id: A): String = id.str

  override def compare(x: A, y: A): Int = Ordering.String.compare(encode(x), encode(y))
}

sealed trait Id {
  val str: String //for application logic
  override def toString: String = s"${str.take(4)}..." //for debugging only
}

abstract class BaseId(str: String) extends Id {
  if (str.contains("..."))
    throw new IllegalStateException(s"toString() $str has been used for application logic. Use str instead.")
}

case class Uid(str: String) extends BaseId(str)

object Uid {
  def apply(): Uid = Uid(UUID.randomUUID().toString)
  def apply(mostSigBits: Long, leastSigBits: Long): Uid = Uid(new UUID(mostSigBits, leastSigBits).toString)

  implicit object UidId extends IdGen[Uid] {
    override def random(): Uid = Uid()
    override def decode(str: String): Uid = Uid(str)
  }
}

case class UserId(str: String) extends BaseId(str) {
  def bytes = {
    val uuid = UUID.fromString(str)
    val bytes = Array.ofDim[Byte](16)
    val bb = ByteBuffer.wrap(bytes).asLongBuffer()
    bb.put(uuid.getMostSignificantBits)
    bb.put(uuid.getLeastSignificantBits)
    bytes
  }
}

object UserId extends (String => UserId) {
  val Zero = UserId(new UUID(0, 0).toString)

  def apply(): UserId = IdGen.random()

  implicit object IdGen extends IdGen[UserId] {
    override def random(): UserId = UserId(Uid().str)
    override def decode(str: String): UserId = UserId(str)
  }

  implicit lazy val UserIdDecoder: JsonDecoder[UserId] = new JsonDecoder[UserId] {
    override def apply(implicit o: JSONObject): UserId = UserId(o.getString("userId"))
  }

  implicit lazy val UserIdEncoder: JsonEncoder[UserId] = new JsonEncoder[UserId] {
    override def apply(id: UserId): JSONObject = JsonEncoder { _.put("userId", id.str) }
  }
}

case class AccountId(str: String) extends BaseId(str)

object AccountId {
  def apply(): AccountId = IdGen.random()

  implicit object IdGen extends IdGen[AccountId] {
    override def random(): AccountId = AccountId(Uid().str)
    override def decode(str: String): AccountId = AccountId(str)
  }
}

case class AssetId(str: String) extends BaseId(str)

object AssetId extends (String => AssetId) {
  def apply(): AssetId = IdGen.random()

  implicit object IdGen extends IdGen[AssetId] {
    override def random() = AssetId(Uid().str)
    override def decode(str: String) = AssetId(str)
  }
}

case class RAssetDataId(str: String) extends BaseId(str)

object RAssetDataId {
  val Empty = RAssetDataId("empty")
  def apply(): RAssetDataId = IdGen.random()

  implicit object IdGen extends IdGen[RAssetDataId] {
    override def random() = RAssetDataId(Uid().str)
    override def decode(str: String) = RAssetDataId(str)
  }
}

case class MessageId(str: String) extends BaseId(str) {
  def uid = Uid(str)
}

object MessageId {
  val Empty = MessageId("")

  def apply(): MessageId = IdGen.random()
  def fromUid(uid: Uid): MessageId = MessageId(uid.str)

  implicit object IdGen extends IdGen[MessageId] {
    override def random() = MessageId(Uid().str)
    override def decode(str: String) = MessageId(str)
  }
}

case class ConvId(str: String) extends BaseId(str)

object ConvId extends (String => ConvId) {
  def apply(): ConvId = IdGen.random()

  implicit object IdGen extends IdGen[ConvId] {
    override def random(): ConvId = ConvId(Uid().str)
    override def decode(str: String): ConvId = ConvId(str)
  }
}

case class RConvId(str: String) extends BaseId(str)

object RConvId {
  val Empty = RConvId("")
  def apply(): RConvId = IdGen.random()

  implicit object IdGen extends IdGen[RConvId] {
    override def random(): RConvId = RConvId(Uid().str)
    override def decode(str: String): RConvId = RConvId(str)
  }
}

case class SyncId(str: String) extends BaseId(str)

object SyncId {
  def apply(): SyncId = IdGen.random()

  implicit object IdGen extends IdGen[SyncId] {
    override def random(): SyncId = SyncId(Uid().str)
    override def decode(str: String): SyncId = SyncId(str)
  }
}

case class GcmId(str: String) extends BaseId(str)

object GcmId {
  def apply(): GcmId = IdGen.random()

  implicit object IdGen extends IdGen[GcmId] {
    override def random(): GcmId = GcmId(Uid().str)
    override def decode(str: String): GcmId = GcmId(str)
  }
}

case class TrackingId(str: String) extends BaseId(str)

object TrackingId {
  def apply(): TrackingId = IdGen.random()

  implicit object IdGen extends IdGen[TrackingId] {
    override def random(): TrackingId = TrackingId(Uid().str)
    override def decode(str: String): TrackingId = TrackingId(str)
  }
}

case class CallSessionId(str: String) extends BaseId(str)

object CallSessionId extends (String => CallSessionId) {
  def apply(): CallSessionId = IdGen.random()

  implicit object IdGen extends IdGen[CallSessionId] {
    override def random(): CallSessionId = CallSessionId(Uid().str)
    override def decode(str: String): CallSessionId = CallSessionId(str)
  }

  implicit object DefaultOrdering extends Ordering[CallSessionId] {
    def compare(a: CallSessionId, b: CallSessionId): Int = Ordering.String.compare(a.str, b.str)
  }
}

case class ContactId(str: String) extends BaseId(str)

object ContactId extends (String => ContactId) {
  def apply(): ContactId = IdGen.random()

  implicit object IdGen extends IdGen[ContactId] {
    override def random(): ContactId = ContactId(Uid().str)
    override def decode(str: String): ContactId = ContactId(str)
  }
}

case class InvitationId(str: String) extends BaseId(str)

object InvitationId extends (String => InvitationId) {
  def apply(): InvitationId = IdGen.random()

  implicit object IdGen extends IdGen[InvitationId] {
    override def random(): InvitationId = InvitationId(Uid().str)
    override def decode(str: String): InvitationId = InvitationId(str)
  }

}

case class ClientId(str: String) extends BaseId(str) {
  def longId = new BigInteger(str, 16).longValue()
}

object ClientId {

  implicit val id: IdGen[ClientId] = new IdGen[ClientId] {
    override def random(): ClientId = ClientId(Random.nextLong().toHexString)
    override def decode(str: String): ClientId = ClientId(str)
  }

  def apply() = id.random()

  def opt(id: String) = Option(id).filter(_.nonEmpty).map(ClientId(_))
}
