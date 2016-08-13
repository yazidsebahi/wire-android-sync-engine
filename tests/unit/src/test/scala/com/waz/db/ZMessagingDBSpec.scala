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

import android.database.DatabaseUtils
import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteDatabase._
import com.waz.api.{KindOfCallingEvent, Message}
import com.waz.model.AssetData.AssetDataDao
import com.waz.model.CallLogEntry.CallLogEntryDao
import com.waz.model.ConversationData.{ConversationDataDao, ConversationType}
import com.waz.model.MessageData.MessageDataDao
import com.waz.model.MsgDeletion.MsgDeletionDao
import com.waz.model.UserData.UserDataDao
import com.waz.model._
import com.waz.utils.DbLoader
import org.robolectric.Robolectric
import org.scalatest._
import org.threeten.bp.Instant

class ZMessagingDBSpec extends FeatureSpec with Matchers with Inspectors with BeforeAndAfter with RobolectricTests with DbLoader {
  lazy val dbHelper = new ZMessagingDB(Robolectric.application, "")

  after {
    dbHelper.close()
    Robolectric.application.getDatabasePath(dbHelper.getDatabaseName).delete()
  }

  feature("Database migrations") {

    scenario("Load db from binary file") {
      val db = loadDb("/db/zmessaging_60.db")
      val c = db.rawQuery("select * from Users", null)
      c should not be null
      c.getCount should be > 0
      db.close()
    }

    scenario("Drop all data for older db versions") {
      implicit val db = loadDb("/db/zmessaging_60.db")

      dbHelper.onUpgrade(db, 59, ZMessagingDB.DbVersion)
      UserDataDao.list should be(empty)
      MessageDataDao.list should be(empty)

      db.close()
    }

    scenario("Migrate UserData from 60") {
      implicit val db = loadDb("/db/zmessaging_60.db")

      val numberOfUsersBeforeMigration = countUsers
      dbHelper.onUpgrade(db, 60, ZMessagingDB.DbVersion)
      countUsers shouldEqual numberOfUsersBeforeMigration
      UserDataDao.list should have size numberOfUsersBeforeMigration
      UserDataDao.list foreach { user =>
        user.relation should not be null
        user.excludeFromPymk shouldEqual false
      }
    }

    scenario("Load AssetData from 60") {
      implicit val db = loadDb("/db/zmessaging_60.db")

      dbHelper.onUpgrade(db, 60, ZMessagingDB.DbVersion)
      val assets = AssetDataDao.list
      assets should not be empty
      assets.map(_.id).toSet should have size assets.size
    }

    scenario("Load ImageAssetData from 60") {
      implicit var db = loadDb("/db/zmessaging_60.db")
      dbHelper.onUpgrade(db, 60, ZMessagingDB.DbVersion)

      // XXX: db cursors keep some cache for column indexes, and this causes errors when reusing tables
      // hopefully this is only a problem in robolectric db driver
      // reopening the db fixes the problem
      val path = db.getPath
      db.close()
      db = SQLiteDatabase.openDatabase(path, null, OPEN_READWRITE)

      val assets = AssetDataDao.list
      assets should have size 57
      forAll(assets) { _.isInstanceOf[ImageAssetData] shouldEqual true }
      assets.flatMap(_.asInstanceOf[ImageAssetData].versions) should have size 114

      val data = ImageAssetData(AssetId(), RConvId(), Seq(ImageData("tag", "mime", 12, 13, 14, 15, 10, None, url = Some("url"))))
      val im = AssetDataDao.insertOrReplace(data)
      im shouldEqual data
      AssetDataDao.list should have size 58
      AssetDataDao.getById(data.id) shouldEqual Some(data)
    }


    scenario("Load MessageData from 60") {
      implicit val db = loadDb("/db/zmessaging_60.db")

      dbHelper.onUpgrade(db, 60, ZMessagingDB.DbVersion)
      val data = MessageDataDao.list
      data should have size 994
    }

    scenario("Load ConversationData from 60") {
      implicit val db = loadDb("/db/zmessaging_60.db")

      dbHelper.onUpgrade(db, 60, ZMessagingDB.DbVersion)
      val convs = ConversationDataDao.list
      convs should have size 72
      convs foreach { conv =>
        conv.missedCallMessage shouldEqual None
        conv.incomingKnockMessage shouldEqual None
        conv.lastRead should be >= Instant.EPOCH
        if (conv.convType == ConversationType.Group)
          conv.renameEvent shouldEqual conv.lastEventTime
        else
          conv.renameEvent shouldEqual Instant.EPOCH
      }
    }

    scenario("New call log table in 66") {
      implicit val db = loadDb("/db/zmessaging_60.db")
      dbHelper.onUpgrade(db, 60, 66)

      val entry = CallLogEntry(KindOfCallingEvent.CALL_ESTABLISHED, Some(CallSessionId()), ConvId(), Instant.now, true)
      CallLogEntryDao.insertOrReplace(entry)
      CallLogEntryDao.list should contain only entry
    }

    scenario("MsgDeletion table added in 68") {
      implicit val db = loadDb("/db/zmessaging_60.db")
      dbHelper.onUpgrade(db, 60, 68)

      val entry = MsgDeletion(MessageId(), Instant.now())
      MsgDeletionDao.insertOrReplace(entry)
      MsgDeletionDao.list should contain only entry
    }

    scenario("Migrate to protobuf model in 69") {
      implicit val db = loadDb("/db/zmessaging_60.db")
      dbHelper.onUpgrade(db, 60, 71)

      val msgs = MessageDataDao.list
      msgs should have size 994
      msgs foreach { m =>
        if (m.msgType == Message.Type.KNOCK) m.protos should have size 1
      }
    }

    scenario("Add message editTime column in 71") {
      implicit val db = loadDb("/db/zmessaging_60.db")
      dbHelper.onUpgrade(db, 60, 71)

      val msgs = MessageDataDao.list
      msgs should have size 994
      msgs foreach { m =>
        if (m.msgType == Message.Type.KNOCK) m.protos should have size 1
        m.editTime shouldEqual Instant.EPOCH
      }
    }
  }

  def countUsers(implicit db: SQLiteDatabase) = DatabaseUtils.queryNumEntries(db, "Users")
}
