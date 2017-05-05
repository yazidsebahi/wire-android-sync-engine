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

import com.waz.model.Id
import com.waz.sync.client.OAuth2Client.RefreshToken
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.znet.AuthenticationManager.Token
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.{ExecutionContext, Future}

trait Preference[A] {
  protected implicit def dispatcher: ExecutionContext
  def default: A
  def apply(): Future[A]
  def update(value: A): Future[Unit]

  def :=(value: A): Future[Unit] = update(value)
  def mutate(f: A => A): Future[Unit] = apply().flatMap(cur => update(f(cur)))

  lazy val signal: SourceSignal[A] = {
    val s = Signal[A]()
    apply().onSuccess { case v => s.publish(v, Threading.Background) }(Threading.Background)
    s
  }
}

object Preference {
  def empty[A] = new Preference[Option[A]] {
    override protected implicit val dispatcher = null
    def default = None
    def apply() = Future.successful(None)
    override def update(value: Option[A]) = Future.successful({})
  }

  def inMemory[A](defaultValue: A): Preference[A] = new Preference[A] {
    override protected implicit val dispatcher = new SerialDispatchQueue()
    private var value = defaultValue
    override def default = defaultValue
    override def update(v: A) = Future { value = v; signal ! v }
    override def apply() = Future { value }
  }

  def apply[A](defaultValue: A, load: => Future[A], save: A => Future[Any]): Preference[A] = new Preference[A] {
    override protected implicit val dispatcher = Threading.Background
    override def default = defaultValue
    override def update(v: A) = save(v) map { _ => signal ! v }
    override def apply() = load
  }

  trait PrefCodec[A] {
    def encode(v: A): String
    def decode(str: String): A
    def default: A
  }

  object PrefCodec {
    def apply[A](enc: A => String, dec: String => A, defaultValue: A): PrefCodec[A] = new PrefCodec[A] {
      override def encode(v: A): String = enc(v)
      override def decode(str: String): A = dec(str)
      override def default = defaultValue
    }

    implicit lazy val StrCodec     = apply[String] (identity,       identity,                       "")
    implicit lazy val IntCodec     = apply[Int]    (String.valueOf, java.lang.Integer.parseInt,     0)
    implicit lazy val LongCodec    = apply[Long]   (String.valueOf, java.lang.Long.parseLong,       0L)
    implicit lazy val BooleanCodec = apply[Boolean](String.valueOf, java.lang.Boolean.parseBoolean, false)

    implicit def idCodec[A: Id]: PrefCodec[A] = apply[A](implicitly[Id[A]].encode, implicitly[Id[A]].decode, implicitly[Id[A]].empty)
    implicit def optCodec[A: PrefCodec]: PrefCodec[Option[A]] = apply[Option[A]](_.fold("")(implicitly[PrefCodec[A]].encode), { str => if (str == "") None else Some(implicitly[PrefCodec[A]].decode(str)) }, None)
    implicit lazy val InstantCodec = apply[Instant](d => String.valueOf(d.toEpochMilli), s => Instant.ofEpochMilli(java.lang.Long.parseLong(s)), Instant.EPOCH)

    implicit lazy val AuthTokenCodec = apply[Option[Token]] (
      { t => optCodec[String].encode(t map Token.Encoder.apply map (_.toString)) },
      { s => optCodec[String].decode(s) map (new JSONObject(_)) map (Token.Decoder.apply(_)) },
      None)

    implicit lazy val SpotifyRefreshTokenCodec = apply[RefreshToken](_.str, RefreshToken, RefreshToken(""))
  }
}
