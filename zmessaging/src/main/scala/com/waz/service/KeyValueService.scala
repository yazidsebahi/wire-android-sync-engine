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
package com.waz.service

import com.waz.ZLog._
import com.waz.content.KeyValueStorage
import com.waz.model.{KeyValueData, Id}
import com.waz.service.Preference.PrefCodec
import com.waz.threading.Threading
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.znet.AuthenticationManager.Token
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.{ExecutionContext, Future}
/**
 * General preference storage in user db.
 * This can be used similarly to PreferenceService, but it keeps separate data for each logged in user as opposed to PreferenceService which uses single global preferences file.
 */
class KeyValueService(kvStorage: KeyValueStorage, reporting: ReportingService) {
  import com.waz.service.KeyValueService._

  private implicit val logTag: LogTag = logTagFor[KeyValueService]

  import Threading.Implicits.Background

  def getPref(key: String): Future[Option[String]] = kvStorage.get(key).map(_.map(_.value))
  def setPref(key: String, value: String): Future[KeyValueData] = kvStorage.put(key, KeyValueData(key, value))
  def delPref(key: String): Future[Unit] = kvStorage.remove(key)
  def decodePref[A](key: String, dec: String => A): Future[Option[A]] = getPref(key).map(_.map(dec))

  def lastSlowSyncTimestamp = decodePref(LastSlowSyncTimeKey, java.lang.Long.parseLong)
  def lastSlowSyncTimestamp_=(time: Long): Unit = setPref(LastSlowSyncTimeKey, String.valueOf(time))

  def keyValuePref[A: PrefCodec](key: String, default: A) = new KeyValuePref[A](this, key, default)

  def accessTokenPref = keyValuePref[Option[Token]](AccessToken, None)(PrefCodec.TokenCodec)

  reporting.addStateReporter { pw =>
    Future {
      pw.println(s"cached:")
      kvStorage foreachCached {
        case KeyValueData(k, v) => pw.println(s"$k: $v")
      }
    }
  }
}

object KeyValueService {
  val SelfUserIdKey = "self_user_id"
  val LastSlowSyncTimeKey = "last_slow_sync_time"
  val Verified = "verified"
  val SelectedConvId = "selected_conv_id"
  val SpotifyRefreshToken = "spotify_refresh_token"
  val AccessToken = "access_token"

  class KeyValuePref[A](service: KeyValueService, key: String, val default: A)(implicit val trans: PrefCodec[A], implicit val dispatcher: ExecutionContext) extends Preference[A] {
    def apply(): Future[A] = service.decodePref(key, trans.decode).map(_.getOrElse(default))
    def :=(value: A): Future[Unit] = {
      service.setPref(key, trans.encode(value)) .map { _ => signal ! value }
    }
  }
}

trait Preference[A] {
  def default: A
  def apply(): Future[A]
  def :=(value: A): Future[Unit]

  lazy val signal: SourceSignal[A] = {
    val s = Signal[A]()
    apply().onSuccess { case v => s.publish(v, Threading.Background) }(Threading.Background)
    s
  }
}

object Preference {
  def empty[A] = new Preference[Option[A]] {
    def default = None
    def apply() = Future.successful(None)
    def :=(value: Option[A]) = Future.successful(())
  }

  trait PrefCodec[A] {
    def encode(v: A): String
    def decode(str: String): A
  }

  object PrefCodec {
    def apply[A](enc: A => String, dec: String => A): PrefCodec[A] = new PrefCodec[A] {
      override def encode(v: A): String = enc(v)
      override def decode(str: String): A = dec(str)
    }

    implicit val StrCodec = apply[String](identity, identity)
    implicit val IntCodec = apply[Int](String.valueOf, java.lang.Integer.parseInt)
    implicit val LongCodec = apply[Long](String.valueOf, java.lang.Long.parseLong)
    implicit val BooleanCodec = apply[Boolean](String.valueOf, java.lang.Boolean.parseBoolean)
    implicit def idCodec[A: Id]: PrefCodec[A] = apply[A](implicitly[Id[A]].encode, implicitly[Id[A]].decode)
    implicit def optCodec[A: PrefCodec]: PrefCodec[Option[A]] = apply[Option[A]](_.fold("")(implicitly[PrefCodec[A]].encode), { str => if (str == "") None else Some(implicitly[PrefCodec[A]].decode(str)) })
    implicit val InstantCodec = apply[Instant](d => String.valueOf(d.toEpochMilli), s => Instant.ofEpochMilli(java.lang.Long.parseLong(s)))
    implicit val TokenCodec = apply[Option[Token]] (
      { t => optCodec[String].encode(t map Token.Encoder.apply map (_.toString)) },
      { s => optCodec[String].decode(s) map (new JSONObject(_)) map (Token.Decoder.apply(_)) })
  }
}
