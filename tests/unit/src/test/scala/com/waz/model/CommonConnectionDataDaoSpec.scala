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

import android.database.sqlite.SQLiteDatabase
import com.waz.db.ZMessagingDB
import com.waz.model.CommonConnectionsData.CommonConnectionsDataDao
import com.waz.model.UserData.UserDataDao
import org.robolectric.Robolectric
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

class CommonConnectionDataDaoSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests {

  var dbHelper: ZMessagingDB = _

  lazy val users = Seq(
    UserData("User 01"),
    UserData("User 02"),
    UserData("User 03"),
    UserData("User 04"),
    UserData("User 05")
  )

  lazy val connections = CommonConnectionsData(users(0).id, 10, users.drop(1).map(_.id))

  before {
    dbHelper = new ZMessagingDB(Robolectric.application, "testDB")
  }

  after {
    dbHelper.close()
    Robolectric.application.getDatabasePath(dbHelper.getDatabaseName).delete()
  }

  implicit def db: SQLiteDatabase = dbHelper.getWritableDatabase

  feature("CRUD") {

    import CommonConnectionsDataDao._

    scenario("get connections for user") {
      UserDataDao.insertOrReplace(users)
      insertOrReplace(connections)

      getById(users(0).id) shouldEqual Some(connections)
    }
  }
}
