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

import android.content.SharedPreferences.OnSharedPreferenceChangeListener
import android.content.{Context, SharedPreferences}
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.{debug, warn}
import com.waz.content.Preferences.Preference
import com.waz.content.Preferences.Preference.PrefCodec
import com.waz.model.KeyValueData.KeyValueDataDao
import com.waz.model.{Id, KeyValueData}
import com.waz.sync.client.OAuth2Client.RefreshToken
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.TrimmingLruCache.Fixed
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.{CachedStorageImpl, TrimmingLruCache}
import com.waz.znet.AuthenticationManager.{Cookie, Token}
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

trait Preferences {

  implicit protected def dispatcher: ExecutionContext

  def preference[A: PrefCodec](key: String, defaultValue: Option[A] = None): Preference[A] = new Preference[A](this, key, defaultValue)

  protected def getValue[A: PrefCodec](key: String, defaultValue: Option[A] = None): Future[A]
  protected def setValue[A: PrefCodec](key: String, value: A): Future[Unit]
}

object Preferences {

  class Preference[A: PrefCodec](prefs: Preferences, key: String, default: Option[A])(implicit val dispatcher: ExecutionContext) {

    def apply():          Future[A]    = prefs.getValue(key, default)
    def update(value: A): Future[Unit] = prefs.setValue(key, value).map { _ => signal ! value }

    def :=(value: A):      Future[Unit] = update(value)
    def mutate(f: A => A): Future[Unit] = apply().flatMap(cur => update(f(cur)))

    lazy val signal: SourceSignal[A] = {
      val s = Signal[A]()
      apply().onSuccess { case v => s.publish(v, Threading.Background) }(Threading.Background)
      s
    }
  }

  object Preference {

    //TODO should be able to eventually get rid of apply and inMemory...
    def apply[A: PrefCodec](defaultValue: A, load: => Future[A], save: A => Future[Any]): Preference[A] = new Preference[A](null, null, null)(implicitly[PrefCodec[A]], Threading.Background) {
      override def apply()      = load
      override def update(v: A) = save(v) map { _ => signal ! v }
    }

    def inMemory[A: PrefCodec](defaultValue: A): Preference[A] = new Preference[A](null, null, null)(implicitly[PrefCodec[A]], Threading.Background) {
      private var value = defaultValue

      override def apply()      = Future { value }
      override def update(v: A) = Future { value = v; signal ! v }
    }

    trait PrefCodec[A] {
      def encode(v: A): String
      def decode(str: String): A
      val default: A
    }

    //TODO maybe we can use JSON codecs at some point...
    object PrefCodec {
      def apply[A](enc: A => String, dec: String => A, defaultValue: A): PrefCodec[A] = new PrefCodec[A] {
        override def encode(v: A): String = enc(v)
        override def decode(str: String): A = dec(str)
        override val default = defaultValue
      }

      implicit lazy val StrCodec     = apply[String] (identity,       identity,                       "")
      implicit lazy val IntCodec     = apply[Int]    (String.valueOf, java.lang.Integer.parseInt,     0)
      implicit lazy val DoubleCodec  = apply[Double] (String.valueOf, java.lang.Double.parseDouble,   0.0)
      implicit lazy val LongCodec    = apply[Long]   (String.valueOf, java.lang.Long.parseLong,       0L)
      implicit lazy val BooleanCodec = apply[Boolean](String.valueOf, java.lang.Boolean.parseBoolean, false)

      implicit def idCodec[A: Id]: PrefCodec[A] = apply[A](implicitly[Id[A]].encode, implicitly[Id[A]].decode, implicitly[Id[A]].empty)
      implicit def optCodec[A: PrefCodec]: PrefCodec[Option[A]] = apply[Option[A]](_.fold("")(implicitly[PrefCodec[A]].encode), { str => if (str == "") None else Some(implicitly[PrefCodec[A]].decode(str)) }, None)
      implicit lazy val InstantCodec = apply[Instant](d => String.valueOf(d.toEpochMilli), s => Instant.ofEpochMilli(java.lang.Long.parseLong(s)), Instant.EPOCH)

      implicit lazy val AuthTokenCodec = apply[Option[Token]] (
        { t => optCodec[String].encode(t map Token.Encoder.apply map (_.toString)) },
        { s => optCodec[String].decode(s) map (new JSONObject(_)) map (Token.Decoder.apply(_)) },
        None)
      implicit lazy val AuthCookieCodec = apply[Cookie] (_.str, Cookie, Cookie(""))

      implicit lazy val SpotifyRefreshTokenCodec = apply[RefreshToken](_.str, RefreshToken, RefreshToken(""))
    }
  }
}

/**
  * Global preference based on Android SharedPreferences
  */
class GlobalPreferences(context: Context) extends Preferences {

  override protected implicit val dispatcher = new SerialDispatchQueue(name = "GlobalPreferencesDispatcher")

  protected val prefs = context.getSharedPreferences("com.wire.preferences", Context.MODE_PRIVATE)

  def v31AssetsEnabled = false

  //TODO eventually remove
  private def migrate() = dispatcher {

    val oldPrefFiles = Seq("zmessaging", "com.waz.zclient.user.preferences")
      .map(name => Option(context.getSharedPreferences(name, Context.MODE_PRIVATE)))
      .collect { case Some(pref) => pref }

    val oldPrefs = oldPrefFiles
      .map(_.getAll.asScala)
      .foldLeft(Map.empty[String, Any]){ case (cur, i) => cur ++ i }
      .map {
        case (key, value) =>
          debug(s"Migrating $key: $value")
          import PrefCodec._
          val encodedValue = value match {
            case v: String  => Some(v)
            case v: Boolean => Some(BooleanCodec.encode(v))
            case v: Int     => Some(IntCodec.encode(v))
            case v: Float   => Some(DoubleCodec.encode(v))
            case v => warn(s"Preference $key: $v has unexpected type. Leaving out of migration"); None
          }
          encodedValue.map(v => key -> v)
      }.collect { case Some((key, v)) => key -> v }

      val editor = prefs.edit()
      oldPrefs.foreach { case (key, value) =>
        editor.putString(key, value)
      }
      if (editor.commit()) {
        debug("Clearing old preference files")
        oldPrefFiles.foreach { pref =>
          pref.edit().clear().commit()
        }
      }
  }
  migrate()

  //TODO would be nice to hide this, but for now it's fine
  def getFromPref[A: PrefCodec](key: String, default: Option[A] = None) = {
    val codec = implicitly[PrefCodec[A]]
    Option(prefs.getString(key, null)).map(codec.decode).orElse(default).getOrElse(codec.default)
  }

  override def preference[A: PrefCodec](key: String, default: Option[A]) = {
    new Preference[A](this, key, default) {

      //No need to update the signal. The SharedPreferences Listener will do this for us.
      override def update(value: A) = setValue[A](key, value)

      private def load = getFromPref(key, default)

      override lazy val signal = new SourceSignal[A](Some(load)) {

        private val listener = new OnSharedPreferenceChangeListener {
          override def onSharedPreferenceChanged(sharedPreferences: SharedPreferences, k: String): Unit = {
            if (key == k) {
              if (Threading.isUiThread) publish(load, Threading.Ui)
              else publish(load)
            }
          }
        }

        override def onWire(): Unit = {
          super.onWire()
          Threading.Ui { prefs.registerOnSharedPreferenceChangeListener(listener) } .map { _ =>
            publish(load, Threading.Background) // load value again after registering the listener (it could have been changed in meantime)
          } (Threading.Background)
        }

        override def onUnwire(): Unit =
          Threading.Ui { prefs.unregisterOnSharedPreferenceChangeListener(listener) }
      }
    }
  }

  protected def getValue[A: PrefCodec](key: String, defaultValue: Option[A] = None): Future[A] =
    dispatcher(getFromPref[A](key, defaultValue))

  protected def setValue[A: PrefCodec](key: String, value: A): Future[Unit] =
    dispatcher(prefs.edit().putString(key, implicitly[PrefCodec[A]].encode(value)).apply())
}

object GlobalPreferences {
  val AutoAnswerCallPrefKey = "PREF_KEY_AUTO_ANSWER_ENABLED"
  val CallingV3Key          = "PREF_KEY_CALLING_V3"
  val GcmEnabledKey         = "PREF_KEY_GCM_ENABLED"
  val V31AssetsEnabledKey   = "PREF_V31_ASSETS_ENABLED"
  val WsForegroundKey       = "PREF_KEY_WS_FOREGROUND_SERVICE_ENABLED"
  val AnalyticsPrefKey      = "PREF_KEY_AVS_METRICS"
}

/**
  * Per-user preference storage in user db.
  */
class UserPreferences(context: Context, storage: ZmsDatabase) extends CachedStorageImpl[String, KeyValueData](new TrimmingLruCache(context, Fixed(128)), storage)(KeyValueDataDao, "KeyValueStorage_Cached") with Preferences {
  override protected implicit val dispatcher = Threading.Background

  protected def getValue[A: PrefCodec](key: String, defaultValue: Option[A]) = {
    val codec = implicitly[PrefCodec[A]]
    get(key).map(_.map(_.value)).map(_.map(codec.decode).orElse(defaultValue).getOrElse(codec.default))
  }

  protected def setValue[A: PrefCodec](key: String, value: A) =
    put(key, KeyValueData(key, implicitly[PrefCodec[A]].encode(value))).map(_ => {})

}

object UserPreferences {
  val LastSlowSyncTimeKey     = "last_slow_sync_time"
  val Verified                = "verified"
  val SelectedConvId          = "selected_conv_id"
  val SpotifyRefreshToken     = "spotify_refresh_token"
  val ShouldSyncConversations = "should_sync_conversations"
}
