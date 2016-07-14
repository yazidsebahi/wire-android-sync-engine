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
import com.waz.Generators._
import com.waz.content.Mime
import com.waz.db.ZMessagingDB
import com.waz.model.AssetData.AssetDataDao
import com.waz.model.AssetPreviewData.Image
import com.waz.model.AssetStatus.{UploadCancelled, UploadDone, UploadFailed, UploadInProgress}
import com.waz.model.GenericContent.Asset
import com.waz.model.GenericContent.Asset.{Original, Preview}
import com.waz.testutils.Matchers._
import com.waz.utils.crypto.AESUtils
import org.robolectric.Robolectric
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}
import org.threeten.bp.Instant

class AssetDataSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfter with GeneratorDrivenPropertyChecks with RobolectricTests {

  import ImageAssetData._

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
    scenario("Random metadata") {
      forAll((_: AssetMetaData) should beUnchangedByEncodingAndDecoding[AssetMetaData])
    }
    scenario("Random image assets") {
      forAll((_: ImageAssetData) should beUnchangedByEncodingAndDecoding[ImageAssetData])
    }
    scenario("Random previews") {
      forAll((x: AssetPreviewData) => {println(x);x should beUnchangedByEncodingAndDecoding[AssetPreviewData]})
    }
    scenario("Random assets") {
      forAll((_: AnyAssetData) should beUnchangedByEncodingAndDecoding[AnyAssetData])
    }
  }

  feature("Database") {
    scenario("Store a list of image assets and retrieve them again") {
      forAll { assets: Vector[ImageAssetData] =>
        AssetDataDao.deleteAll
        AssetDataDao.insertOrReplace(assets)
        AssetDataDao.list shouldEqual assets
      }
    }
    scenario("Store a list of assets and retrieve them again") {
      forAll { assets: Vector[AnyAssetData] =>
        AssetDataDao.deleteAll
        AssetDataDao.insertOrReplace(assets)
        AssetDataDao.list shouldEqual assets
      }
    }
  }

  feature("ImageAssetData") {

    scenario("Sort image with broken meta-data") {
      val data = Seq(ImageData("smallProfile", "", 280, 280, 0, 0, remoteId = Some(RAssetDataId())), ImageData("medium", "", 996, 660, 0, 0, remoteId = Some(RAssetDataId())))
      data.sorted shouldEqual data
    }

    scenario("sort image with weird file size") {
      val data = Seq(ImageData("medium", "", 996, 660, 0, 0, 100, remoteId = Some(RAssetDataId())), ImageData("smallProfile", "", 280, 280, 0, 0, 200, remoteId = Some(RAssetDataId())))
      data.sorted.map(_.tag) shouldEqual List("smallProfile", "medium")
    }
  }

  feature("AnyAssetData.updated") {

    val id = AssetId()
    val conv = RConvId()
    val mime = Mime("text/plain")
    lazy val orig = Original(mime, 100, Some("file.txt"))
    lazy val asset = AnyAssetData(id, conv, Asset(orig, UploadInProgress), None, Instant.ofEpochMilli(100))

    scenario("Create from Asset") {
      asset shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, None, None, None, AssetStatus.UploadInProgress, Instant.ofEpochMilli(100))
    }

    scenario("Add preview") {
      val preview = Preview(Mime("image/jpeg"), 50L, AESKey(), Sha256("sha"))
      val dataId = RAssetDataId()
      val updated = asset.updated(AnyAssetData(id, conv, Asset(orig, preview), Some(dataId), Instant.ofEpochMilli(101)))
      updated shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, Some(Image(ImageData("preview", "image/jpeg", 0, 0, 0, 0, 50, Some(dataId), None, true, None, None, Some(AESKey(preview.remote.otrKey)), Some(Sha256(preview.remote.sha256))))), None, None, AssetStatus.UploadInProgress, Instant.ofEpochMilli(101))
    }

    scenario("Asset is uploaded") {
      val dataId = RAssetDataId()
      val key = AESKey()
      val sha = Sha256(AESUtils.randomKey128().bytes)
      val updated = asset.updated(AnyAssetData(id, conv, Asset(orig, UploadDone(AssetKey(Left(RAssetDataId()), None, key, sha))), Some(dataId), Instant.ofEpochMilli(101)))
      updated shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, None, None, None, AssetStatus.UploadDone(AssetKey(Left(dataId), None, key, sha)), Instant.ofEpochMilli(101))
    }

    scenario("Upload is cancelled") {
      val updated = asset.updated(AnyAssetData(id, conv, Asset(UploadCancelled), None, Instant.ofEpochMilli(101)))
      updated shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, None, None, None, AssetStatus.UploadCancelled, Instant.ofEpochMilli(101))
    }

    scenario("Upload failed") {
      val updated = asset.updated(AnyAssetData(id, conv, Asset(UploadFailed), None, Instant.ofEpochMilli(101)))
      updated shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, None, None, None, AssetStatus.UploadFailed, Instant.ofEpochMilli(101))
    }

    scenario("Previously failed asset is re-uploaded") {
      val failed = asset.updated(AnyAssetData(id, conv, Asset(UploadFailed), None, Instant.ofEpochMilli(101)))
      val dataId = RAssetDataId()
      val key = AESKey()
      val sha = Sha256(AESUtils.randomKey128().bytes)
      val updated = failed.updated(AnyAssetData(id, conv, Asset(orig, UploadDone(AssetKey(Left(RAssetDataId()), None, key, sha))), Some(dataId), Instant.ofEpochMilli(102)))
      updated shouldEqual AnyAssetData(id, conv, mime, 100, Some("file.txt"), None, None, None, None, AssetStatus.UploadDone(AssetKey(Left(dataId), None, key, sha)), Instant.ofEpochMilli(102))
    }
  }
}
