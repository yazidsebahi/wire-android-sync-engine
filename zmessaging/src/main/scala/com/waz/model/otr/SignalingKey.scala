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
package com.waz.model.otr

import java.security.SecureRandom
import javax.crypto.spec.SecretKeySpec

import android.util.Base64
import com.waz.utils.{JsonDecoder, JsonEncoder, returning}
import org.json.JSONObject

case class OtrKey(str: String) {
  lazy val bytes = SignalingKey.base64(str)
}
object OtrKey {
  def apply() = new OtrKey(SignalingKey.randomKey)
  def apply(bytes: Array[Byte]) = new OtrKey(SignalingKey.base64(bytes))
}

case class Sha256(str: String) {
  lazy val bytes = SignalingKey.base64(str)

  def matches(bytes: Array[Byte]) = str == com.waz.utils.sha2(bytes)
}
object Sha256 {
  def apply(bytes: Array[Byte]) = new Sha256(SignalingKey.base64(bytes))
}

case class MsgAuthCode(str: String) {
  lazy val bytes = SignalingKey.base64(str)
}

object MsgAuthCode {
  def apply(bytes: Array[Byte]) = new MsgAuthCode(SignalingKey.base64(bytes))
}

/*
 * "we use standard AES256 in CBC mode with PKCS#5/7 padding and the initialization vector (IV) prepended to the ciphertext"
 * The MAC uses HMAC-SHA256.
 */
case class SignalingKey(encKey: String, macKey: String) {

  lazy val encKeyBytes = Base64.decode(encKey, Base64.NO_WRAP | Base64.NO_CLOSE)
  lazy val macKeyBytes = Base64.decode(macKey, Base64.NO_WRAP | Base64.NO_CLOSE)
  lazy val mac = new SecretKeySpec(macKeyBytes, "HmacSHA256")
}

object SignalingKey {

  lazy val random = new SecureRandom()

  def base64(key: Array[Byte]) = Base64.encodeToString(key, Base64.NO_WRAP | Base64.NO_CLOSE)
  def base64(key: String) = Base64.decode(key, Base64.NO_WRAP | Base64.NO_CLOSE)

  def randomKeyBytes = returning(new Array[Byte](32)) { random.nextBytes }
  def randomKey = Base64.encodeToString(returning(new Array[Byte](32)) { random.nextBytes }, Base64.NO_WRAP | Base64.NO_CLOSE)

  def apply(): SignalingKey = new SignalingKey(randomKey, randomKey)

  def apply(enc: Array[Byte], mac: Array[Byte]): SignalingKey = new SignalingKey(base64(enc), base64(mac))

  implicit lazy val Decoder: JsonDecoder[SignalingKey] = new JsonDecoder[SignalingKey] {
    import JsonDecoder._
    override def apply(implicit js: JSONObject): SignalingKey = SignalingKey('enckey, 'mackey)
  }

  implicit lazy val Encoder: JsonEncoder[SignalingKey] = new JsonEncoder[SignalingKey] {
    override def apply(v: SignalingKey): JSONObject = JsonEncoder { o =>
      o.put("enckey", v.encKey)
      o.put("mackey", v.macKey)
    }
  }
}
