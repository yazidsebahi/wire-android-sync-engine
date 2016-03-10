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
import com.waz.model.AssetData.AssetDataDao
import com.waz.utils.JsonDecoder._
import com.waz.utils.JsonEncoder._
import org.robolectric.Robolectric
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}
import com.waz.Generators._

class AssetDataSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfter with GeneratorDrivenPropertyChecks with RobolectricTests {

  var dbHelper: ZMessagingDB = _

  before {
    dbHelper = new ZMessagingDB(Robolectric.application, "dbName")
  }

  after {
    dbHelper.close()
    Robolectric.application.getDatabasePath("dbName").delete()
  }

  implicit def db: SQLiteDatabase = dbHelper.getWritableDatabase

  feature("json serialization") {
    scenario("Random image assets") {
      forAll { asset: ImageAssetData =>
        asset should beUnchangedByEncodingAndDecoding
      }
    }

    def beUnchangedByEncodingAndDecoding = Matcher[ImageAssetData] { left =>
      val json = encode(left)
      val decoded = decode[ImageAssetData](json.toString)
      val encoded = encode(decoded)
      MatchResult(
        decoded == left && encode(decoded).toString == json.toString,
        s"$left was not equal to $decoded, or ${encoded.toString(2)} was not equal to ${json.toString(2)}}",
        s"$left staid the same after encoding, decoding and re-encoding."
      )
    }
  }

  feature("Database") {

    scenario("Store a list of assets and retrieve them again") {

      forAll { assets: List[ImageAssetData] =>
        AssetDataDao.deleteAll
        AssetDataDao.insertOrReplace(assets)
        AssetDataDao.list shouldEqual assets
      }
    }
  }

  feature("ImageAssetData") {

    scenario("Sort image with broken meta-data") {
      val data = Seq(ImageData("smallProfile", "", 280, 280, 0, 0, remoteId = Some(RImageDataId())), ImageData("medium", "", 996, 660, 0, 0, remoteId = Some(RImageDataId())))
      data.sorted shouldEqual data
    }

    scenario("sort image with weird file size") {
      val data = Seq(ImageData("medium", "", 996, 660, 0, 0, 100, remoteId = Some(RImageDataId())), ImageData("smallProfile", "", 280, 280, 0, 0, 200, remoteId = Some(RImageDataId())))
      data.sorted.map(_.tag) shouldEqual List("smallProfile", "medium")
    }
  }
}
