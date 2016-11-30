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
package com.waz.db

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog._
import com.waz.api.ClientRegistrationState
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.ZmsDatabase
import com.waz.db.Col._
import com.waz.db.ZGlobalDB.Migrations
import com.waz.db.migrate.{AccountDataMigration, TableDesc, TableMigration}
import com.waz.model.AccountData.AccountDataDao
import com.waz.model.otr.ClientId
import com.waz.model.{AccountId, UserId}
import com.waz.utils.{IoUtils, JsonDecoder, JsonEncoder, Resource}
import com.waz.znet.AuthenticationManager.Token

class ZGlobalDB(context: Context, dbNameSuffix: String = "") extends DaoDB(context.getApplicationContext, ZGlobalDB.DbName + dbNameSuffix, null, ZGlobalDB.DbVersion) {
  private implicit val logTag: LogTag = logTagFor[ZGlobalDB]

  override val daos = Seq(AccountDataDao, CacheEntryDao)

  override val migrations = Seq(
    Migration(5, 6)(addCacheEntry_Path_LastUsed_Timeout),
    Migration(6, 7)(addPhoneNumber),
    Migration(7, 8)(addPhoneNumberVerified),
    Migration(8, 9){ implicit db => db.execSQL("ALTER TABLE CacheEntry ADD COLUMN enc_key TEXT") },
    Migration(9, 10){ implicit db =>
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN mime TEXT default ''")
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN file_name TEXT")
    },
    Migration(10, 11) { implicit db =>
      db.execSQL("DELETE FROM CacheEntry WHERE enc_key IS NOT NULL") // encryption handling was changed, so it's easiest to just drop old cache entries
    },
    Migration(11, 12){ implicit db =>
      db.execSQL("ALTER TABLE CacheEntry ADD COLUMN length INTEGER")
    },
    Migration(12, 13)(Migrations.v11(context)),
    Migration(13,14){
      implicit db => AccountDataMigration.v78(db)
    }
  )

  override def onUpgrade(db: SQLiteDatabase, from: Int, to: Int): Unit = {
    if (from < 5) clearAllData(db)
    else super.onUpgrade(db, from, to)
  }

  def clearAllData(db: SQLiteDatabase) = {
    debug("wiping global db...")
    dropAllTables(db)
    onCreate(db)
  }

  lazy val addCacheEntry_Path_LastUsed_Timeout: SQLiteDatabase => Unit = { implicit db =>
    import CacheEntryDao._

    debug("Upgrading global db to accommodate for upgraded image caching...")
    Seq("path", "lastUsed", "timeout") foreach { col => db.execSQL(s"ALTER TABLE CacheEntry ADD COLUMN $col TEXT") }
    setPathForFileEntries(context.getCacheDir)
    moveToTimeouts()
  }

  lazy val addPhoneNumber: SQLiteDatabase => Unit = { implicit db =>
    debug("Upgrading global db to prepare for phone number verification...")
    db.execSQL(s"ALTER TABLE ZUsers ADD COLUMN phone TEXT")
  }

  lazy val addPhoneNumberVerified: SQLiteDatabase => Unit = { implicit db =>
    debug("Upgrading global DB to remember if phone number was verified…")
    db.execSQL(s"ALTER TABLE ZUsers ADD COLUMN phone_verified INTEGER")
    db.execSQL(s"UPDATE ZUsers SET phone_verified = phone is not null")
  }
}

object ZGlobalDB {
  val DbName = "ZGlobal.db"
  val DbVersion = 14


  object Migrations {

    implicit object ZmsDatabaseRes extends Resource[ZmsDatabase] {
      override def close(r: ZmsDatabase): Unit = r.close()
    }

    implicit object DbRes extends Resource[SQLiteDatabase] {
      override def close(r: SQLiteDatabase): Unit = r.close()
    }

    def v11(context: Context): SQLiteDatabase => Unit = { implicit db =>
      // migrates ZUsers to AccountData, extracts some info from user specific KeyValue storage

      val SelfUserId = "self_user_id"
      val OtrClientId = "otr-client-id"
      val OtrClientRegState = "otr-client-reg-state"

      val c = db.query("ZUsers", Array("_id"), null, null, null, null, null)
      val ids = Seq.tabulate(c.getCount) { idx =>
        c.moveToPosition(idx)
        c.getString(0)
      }
      c.close()

      val accountData: Map[AccountId, (Option[UserId], Option[ClientId], Option[ClientRegistrationState])] = ids .map { id =>
        val values = try {
          IoUtils.withResource(new ZmsDatabase(AccountId(id), context)) { zmsDb =>
            IoUtils.withResource(zmsDb.dbHelper.getReadableDatabase) { zd =>
              val c = zd.query("KeyValues", Array("key", "value"), s"key IN ('$SelfUserId', '$OtrClientId', '$OtrClientRegState')", null, null, null, null)
              Seq.tabulate(c.getCount) { i => c.moveToPosition(i); c.getString(0) -> c.getString(1) } .toMap
            }
          }
        } catch {
          case _: Throwable => Map.empty[String, String]
        }

        AccountId(id) -> (values.get(SelfUserId).map(UserId), values.get(OtrClientId).map(ClientId(_)), values.get(OtrClientRegState).map(ClientRegistrationState.valueOf))
      } .toMap

      val moveConvs = new TableMigration(TableDesc("ZUsers", Columns.v12.all), TableDesc("Accounts", Columns.v13.all)) {
        import Columns.{v13 => dst, v12 => src}

        override val bindings: Seq[Binder] = Seq(
          dst.Id := src.Id,
          dst.Email := src.Email,
          dst.Hash := src.Hash,
          dst.EmailVerified := src.EmailVerified,
          dst.Cookie := src.Cookie,
          dst.Phone := src.Phone,
          dst.Token := { _ => None },
          dst.UserId := { c => accountData.get(src.Id(c)).flatMap(_._1) },
          dst.ClientId := { c => accountData.get(src.Id(c)).flatMap(_._2) },
          dst.ClientRegState := { c => accountData.get(src.Id(c)).flatMap(_._3).getOrElse(ClientRegistrationState.UNKNOWN) }
        )
      }

      moveConvs.migrate(db)
      db.execSQL("DROP TABLE IF EXISTS ZUsers")
    }
  }

  object Columns {

    object v12 {
      val Id = id[AccountId]('_id, "PRIMARY KEY")
      val Email = opt(emailAddress('email))
      val Hash = text('hash)
      val EmailVerified = bool('verified)
      val PhoneVerified = bool('phone_verified)
      val Cookie = opt(text('cookie))
      val Phone = opt(phoneNumber('phone))

      val all = Seq(Id, Email, Hash, EmailVerified, PhoneVerified, Cookie, Phone)
    }

    object v13 {
      val Id = id[AccountId]('_id, "PRIMARY KEY")
      val Email = opt(emailAddress('email))
      val Hash = text('hash)
      val EmailVerified = bool('verified)
      val Cookie = opt(text('cookie))
      val Phone = opt(phoneNumber('phone))
      val Token = opt(text[Token]('access_token, JsonEncoder.encodeString[Token], JsonDecoder.decode[Token]))
      val UserId = opt(id[UserId]('user_id))
      val ClientId = opt(id[ClientId]('client_id))
      val ClientRegState = text[ClientRegistrationState]('reg_state, _.name(), ClientRegistrationState.valueOf)

      val all = Seq(Id, Email, Hash, EmailVerified, Cookie, Phone, Token, UserId, ClientId, ClientRegState)
    }
  }
}
