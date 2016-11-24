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
package com.waz.sync.handler

import java.io.ByteArrayOutputStream

import android.database.sqlite.SQLiteDatabase
import android.graphics.Bitmap.CompressFormat
import android.graphics.{Bitmap, BitmapFactory}
import com.waz._
import com.waz.api.impl.ErrorResponse
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.cache._
import com.waz.model.AssetMetaData.Image
import com.waz.model.AssetMetaData.Image.Tag.Medium
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient
import com.waz.testutils.MockZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.IoUtils
import org.robolectric.Robolectric
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class ImageAssetSyncHandlerSpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with OptionValues with RobolectricTests with RobolectricUtils { test =>

  private lazy implicit val dispatcher = Threading.Background

  implicit def db: SQLiteDatabase = service.db.dbHelper.getWritableDatabase

  var postImageResponse: (AssetData, RConvId) => Either[ErrorResponse, RAssetId] = { (_, _) => Left(ErrorResponse.Cancelled) }
  var postImageRequest = None: Option[(AssetData, RConvId, LocalData)]

  val successImageResponse = { (asset: AssetData, convId: RConvId) => Right(RAssetId()) }

  lazy val selfUser = UserData("test")
  lazy val conv = ConversationData(ConvId(), RConvId(), None, selfUser.id, ConversationType.Group)

  lazy val service = new MockZMessaging(selfUserId = selfUser.id) {

    convsStorage.insert(conv)
    usersStorage.insert(selfUser)

    override lazy val assetClient: AssetClient = new AssetClient(zNetClient) {
      override def postImageAssetData(asset: AssetData, data: LocalData, nativePush: Boolean = true, convId: RConvId) = {
        postImageRequest = Some((asset, convId, data))
        CancellableFuture.successful(postImageResponse(asset, convId))
      }
    }
  }

  def cache = service.cache
  def handler = service.assetSync

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    ZMessaging.context = Robolectric.application
  }

  def generateImageAsset(resource: String = "/images/penguin_128.png") = {
    Await.result(service.assetGenerator.generateWireAsset(dataFromBitmap(resource), profilePicture = false), 15.seconds)
  }

  def dataFromBitmap(resource: String = "/images/penguin_128.png"): AssetData = {
    val bitmap = BitmapFactory.decodeStream(getClass.getResourceAsStream(resource))
    val bos = new ByteArrayOutputStream()
    bitmap.compress(CompressFormat.PNG, 0, bos)

    AssetData(
      sizeInBytes = bos.size(),
      metaData = Some(Image(Dim2(bitmap.getWidth, bitmap.getHeight), Medium)),
      remoteId = Some(RAssetId()),
      data = Some(bos.toByteArray)
    )
  }

  scenario("post full image data") {
    val asset = generateImageAsset()
    Await.ready(service.assets.storage.mergeOrCreateAsset(asset), 1.second)
    val file = Await.result(cache.getEntry(asset.cacheKey), 10.seconds).value.cacheFile

    file should exist

    info(file.toString)
    info(asset.toString)

    postImageResponse = successImageResponse
    val res = Await.result(handler.postImageData(conv.remoteId, asset, LocalData(file)), 1.second)
    res shouldEqual SyncResult.Success

    postImageRequest shouldEqual Some((asset, conv.remoteId, LocalData(file)))
    file should exist // file should still exist after syncing (it will be cleaned by normal cache cleanup

    val updated = Await.result(service.assetsStorage.get(asset.id), 5.seconds).get
    updated.convId shouldEqual conv.remoteId

    val entry = Await.result(cache.getEntry(asset.cacheKey), 1.second)
    entry should be('defined)
    entry.get.cacheFile should exist
    IoUtils.toByteArray(entry.get.inputStream) should have size asset.size
    entry.get.cacheFile.length().toInt shouldEqual (asset.size +- 32)
  }
}
