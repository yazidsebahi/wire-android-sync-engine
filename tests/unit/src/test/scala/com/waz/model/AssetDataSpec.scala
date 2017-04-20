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

import com.waz.Generators._
import com.waz.db.ZMessagingDB
import com.waz.model.AssetStatus.UploadInProgress
import com.waz.testutils.Matchers._
import com.waz.utils.wrappers.{DB, DBHelper}
import org.robolectric.Robolectric
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

class AssetDataSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfter with GeneratorDrivenPropertyChecks with RobolectricTests {

  import AssetData._

  var dbHelper: DBHelper = _

  before {
    dbHelper = new ZMessagingDB(Robolectric.application, "dbName")
  }

  after {
    dbHelper.close()
    Robolectric.application.getDatabasePath("dbName").delete()
  }

  implicit def db: DB = dbHelper.getWritableDatabase

  feature("json serialization") {
    scenario("Random metadata") {
      forAll((_: AssetMetaData) should beUnchangedByEncodingAndDecoding[AssetMetaData])
    }
    scenario("Random assets assets") {
      forAll((_: AssetData) should beUnchangedByEncodingAndDecoding[AssetData])
    }
  }

  feature("Database") {
    scenario("Store a list of assets and retrieve them again") {
      forAll { assets: Vector[AssetData] =>
        AssetDataDao.deleteAll
        AssetDataDao.insertOrReplace(assets)
        AssetDataDao.list shouldEqual assets
      }
    }
  }

  feature("ImageAssetData") {

    scenario("Sort image with broken meta-data") {
      fail()
//      val data = Seq(AssetData(metaData = Some(AssetMetaData.Image(Dim2(280, 280), "smallProfile")), remoteId = Some(RAssetId())), AssetData(metaData = Some(AssetMetaData.Image(Dim2(960, 960), "medium")), remoteId = Some(RAssetId())))
//      data.sorted shouldEqual data
    }
  }

  feature("AnyAssetData.updated") {

    val id = AssetId()
    val conv = RConvId()
    val mime = Mime("text/plain")
    lazy val asset = AssetData(id = id, mime = mime, sizeInBytes = 100, convId = Some(conv), status = UploadInProgress, name = Some("file.txt"))

    //TODO Dean - test merging asset data
  }
}
