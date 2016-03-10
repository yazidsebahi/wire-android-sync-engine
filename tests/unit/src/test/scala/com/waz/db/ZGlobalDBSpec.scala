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

import android.database.sqlite.SQLiteDatabase
import com.waz.model.ZUser.ZUserDao
import com.waz.model.{ZUserId, EmailAddress, PhoneNumber, ZUser}
import com.waz.utils.DbLoader
import org.robolectric.Robolectric
import org.scalatest._

class ZGlobalDBSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfter with RobolectricTests with DbLoader {
  lazy val dbHelper = new ZGlobalDB(Robolectric.application)

  after {
    dbHelper.close()
    Robolectric.application.getDatabasePath(dbHelper.getDatabaseName).delete()
  }

  feature("ZUser") {
    implicit def db: SQLiteDatabase = dbHelper.getWritableDatabase

    lazy val email = EmailAddress("test@test.com")
    lazy val user = ZUser(email, "test_pass")
    lazy val user1 = user.copy(password = None)
    lazy val user2 = user.copy(phone = Some(PhoneNumber("meep")))
    lazy val user3 = user2.copy(phoneVerified = true)

    scenario("insert new ZUser") {
      ZUserDao.insertOrReplace(user) shouldEqual user
      ZUserDao.getById(user.id) shouldEqual Some(user1)
    }

    scenario("load added ZUser") {
      ZUserDao.insertOrReplace(user) shouldEqual user
      ZUserDao.findByEmail(user.email.value) shouldEqual Some(user1)
    }

    scenario("user with phone number") {
      ZUserDao.insertOrReplace(user2) shouldEqual user2
      ZUserDao.findByEmail(user2.email.value).value.phone.value shouldEqual PhoneNumber("meep")
      ZUserDao.findByEmail(user2.email.value).value.phoneVerified shouldBe false
    }

    scenario("user with verified phone number") {
      ZUserDao.insertOrReplace(user3) shouldEqual user3
      ZUserDao.findByEmail(user3.email.value).value.phoneVerified shouldBe true
    }
  }

  feature("Database migrations") {
    scenario("Migrate ZUsers from 6") {
      implicit val db = loadDb("/db/ZGlobal_6.db")

      try {
        dbHelper.onUpgrade(db, 6, ZGlobalDB.DbVersion)

        ZUserDao.list should have size 2
        ZUserDao.list foreach { user =>
          user.phone shouldBe empty
          user.phoneVerified shouldBe false
        }
      } finally db.close()
    }

    scenario("Migrate ZUsers from 7") {
      implicit val db = loadDb("/db/ZGlobal_7.db")

      try {
        dbHelper.onUpgrade(db, 7, ZGlobalDB.DbVersion)
        ZUserDao.getById(ZUserId("8546c628-c9e8-45d6-82dd-7f6dcb56e171")).value.phoneVerified shouldBe false
        ZUserDao.getById(ZUserId("09621ddd-736f-4ec5-b4b5-d24cbb56b9f3")).value.phoneVerified shouldBe true
      } finally db.close()
    }
  }
}
