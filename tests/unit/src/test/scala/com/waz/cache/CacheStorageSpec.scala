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
import com.waz.RobolectricUtils
import com.waz.cache.CacheEntryData.CacheEntryDao
import com.waz.content.GlobalDatabase
import com.waz.testutils.Matchers._
import com.waz.testutils._
import com.waz.utils.returning
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.duration._


class CacheStorageSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit def db: SQLiteDatabase = storage.dbHelper.getWritableDatabase
  implicit val timeout: FiniteDuration = 5.seconds

  lazy val cacheDir = context.getCacheDir

  lazy val storage = new GlobalDatabase(context)
  lazy val cache = new CacheStorage(storage, context)

  feature("Cache Storage Initialization") {
    scenario("Cache entries where files and data are missing are not loaded.") {
      cache.insert(Seq(withData, withFile, withoutDataOrFile)).await()

      cache.get("withData") should eventually(be('defined))
      cache.get("withFile") should eventually(be('defined))
      cache.get("withoutDataOrFile") should eventually(be(None))

      withDelay { CacheEntryDao.getByKey("withoutDataOrFile") shouldEqual None }
    }

    scenario("Expired cache entries are not loaded.") {
      val exp = expiredWithFile
      cache.insert(Seq(withData, withFile, expiredWithData, exp)).await()

      cache.get("withData") should eventually(be('defined))
      cache.get("withFile") should eventually(be('defined))
      cache.get("expiredWithData") should eventually(be(None))
      cache.get("expiredWithFile") should eventually(be(None))

      withDelay {
        CacheEntryDao.getByKey("expiredWithData") shouldEqual None
        CacheEntryDao.getByKey("expiredWithFile") shouldEqual None

        CacheStorage.entryFile(cacheDir, exp.fileId).exists shouldEqual false
      }
    }

    scenario("Cache entries with 'infinite' timeout.") {
      import Expiration._
      cache.insert(CacheEntryData("meep", Some(Array[Byte](1)), lastUsed = 0L, timeout = Duration.Inf.timeout, path = None)).await()
      cache.get("meep") should eventually(be('defined))
    }
  }

  def withoutDataOrFile = CacheEntryData("withoutDataOrFile", path = None)

  def withData = CacheEntryData("withData", Some(Array[Byte](1, 2, 3)), path = Some(cacheDir))

  def withFile = returning(CacheEntryData("withFile", None, path = Some(cacheDir))) { entry =>
    val file = CacheStorage.entryFile(cacheDir, entry.fileId)
    file.getParentFile.mkdirs()
    file.createNewFile()
  }

  def expiredWithData = CacheEntryData("expiredWithData", Some(Array[Byte](1, 2, 3)), lastUsed = 0L, path = None)
  def expiredWithFile = withFile.copy(key = "expiredWithFile", lastUsed = 0L)
}
