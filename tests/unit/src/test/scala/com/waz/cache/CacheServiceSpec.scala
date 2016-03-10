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
package com.waz.cache

import android.database.sqlite.SQLiteDatabase
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.GlobalStorage
import com.waz.db.ZGlobalDB
import com.waz.testutils.Matchers._
import org.robolectric.Robolectric
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class CacheServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests { test =>

  var storage: GlobalStorage = _
  var service: CacheService = _

  implicit def db: SQLiteDatabase = storage.dbHelper.getWritableDatabase

  lazy val cacheDir = Robolectric.application.getCacheDir

  before {
    storage = new GlobalStorage(Robolectric.application)
    service = new CacheService(Robolectric.application, storage)
  }

  after {
    Thread.sleep(1000) // because some operations (deleting) are scheduled on background
    storage.close.await()
    Robolectric.application.getDatabasePath(ZGlobalDB.DbName).getParentFile.listFiles.foreach(_.delete())
  }

  feature("Create") {
    scenario("Create CacheEntry with data") {
      val entry = service.addData("foo", "bar".getBytes)
      entry.data.key should be("foo")
    }

    scenario("Create CacheEntry with inputStream") {
      val in = getClass.getResourceAsStream("/images/fixedit.jpg")

      val entry = service.addStream("foo", in)
      val result = Await.result(entry, 10.seconds)

      result.data.key should be("foo")
      result.cacheFile.exists() shouldEqual true
    }

    scenario("Create empty CacheEntry") {
      val entry = service.createForFile("foo")

      entry.data.key should be("foo")
    }

    scenario("Create CacheEntry with data will overwrite") {
      val entry1 = service.addData("foo", "bar".getBytes)
      val entry2 = service.addData("foo", "bar".getBytes)

      entry2.data.fileId should not be entry1.data.fileId
      entry1.cacheFile.exists() shouldEqual false
    }

    scenario("Create CacheEntry with inputStream will overwrite") {
      val entry1 = service.addStream("foo", getClass.getResourceAsStream("/images/fixedit.jpg"))
      val result1 = Await.result(entry1, 10.seconds)

      val entry2 = service.addStream("foo", getClass.getResourceAsStream("/images/fixedit.jpg"))
      val result2 = Await.result(entry2, 10.seconds)

      Thread.sleep(250)

      result2.cacheFile should not be result1.cacheFile
      result1.cacheFile.exists() shouldEqual false
    }
  }

  feature("Read") {

    scenario("Create CacheEntry with data and read it") {
      val entry = service.addData("foo", "bar".getBytes)

      assertEquals(Await.result(service.getEntry("foo"), 10.seconds).get, entry)
    }

    scenario("Create CacheEntry with inputStream and read it") {
      val in = getClass.getResourceAsStream("/images/fixedit.jpg")

      val entry = service.addStream("foo", in)
      val result = Await.result(entry, 10.seconds)

      assertEquals(Await.result(service.getEntry("foo"), 10.seconds).get, result)
    }

    scenario("Create empty CacheEntry and try to read it") { // cache should never return empty entries
      val entry = service.createForFile("foo")

      Await.result(service.getEntry("foo"), 10.seconds) shouldEqual None
    }
  }

  def assertEquals(e1: CacheEntry, e2: CacheEntry): Unit = assertEquals(e1.data, e2.data)

  def assertEquals(e1: CacheEntryData, e2: CacheEntryData): Unit = {
    e1.fileId shouldEqual e2.fileId
    e1.key shouldEqual e2.key
    e1.timeout shouldEqual e2.timeout
    e1.data.map(_.toSeq) shouldEqual e2.data.map(_.toSeq)
  }

  feature("Delete") {
    scenario("Create CacheEntry with data and delete it") {
      val entry = service.addData("foo", "bar".getBytes)

      Await.result(service.remove(entry), 1.second)
      Await.result(service.getEntry("foo"), 10.seconds) should be(None)
    }

    scenario("Create CacheEntry with file and delete it") {
      def in = getClass.getResourceAsStream("/images/fixedit.jpg")

      val entry = Await.result(service.addStream("foo", in), 10.seconds)

      Await.result(service.remove(entry), 1.second)
      Await.result(service.getEntry("foo"), 10.seconds) should be(None)
    }
  }

  feature("Sync") {
    scenario("Delete all expired") {
      val Seq(foo, bar) = Seq("foo", "bar") map (service.createForFile(_)(Expiration(0)))
      val Seq(goo, hoo) = Seq("goo", "hoo") map (service.addData(_, "zeta".getBytes)(Expiration(0)))
      Seq(foo, bar) foreach { _.cacheFile.createNewFile() }

      Thread.sleep(1000)
      Seq(foo, bar) foreach { _.cacheFile should exist }

      Await.result(service.deleteExpired(), 10.seconds)
      Thread.sleep(1000)

      Seq(foo, bar, goo, hoo) foreach { _.cacheFile should not(exist) }

      CacheEntryDao.findAll should be(Seq.empty)
    }
  }
}
