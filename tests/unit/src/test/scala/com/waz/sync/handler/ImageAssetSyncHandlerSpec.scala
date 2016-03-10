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
import java.util.Date

import android.database.sqlite.SQLiteDatabase
import android.graphics.Bitmap.CompressFormat
import android.graphics.{Bitmap, BitmapFactory}
import com.waz._
import com.waz.api.impl.ErrorResponse
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.cache._
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service._
import com.waz.sync.SyncResult
import com.waz.sync.client.ImageAssetClient
import com.waz.sync.queue.ConvLock
import com.waz.testutils.MockZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.robolectric.Robolectric
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class ImageAssetSyncHandlerSpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with OptionValues with RobolectricTests with RobolectricUtils { test =>

  private lazy implicit val dispatcher = Threading.Background

  implicit def db: SQLiteDatabase = service.storage.dbHelper.getWritableDatabase

  var postImageResponse: (ImageData, AssetId, RConvId) => Either[ErrorResponse, AssetAddEvent] = { (_, _, _) => Left(ErrorResponse.Cancelled) }
  var postImageRequest = None: Option[(ImageData, RConvId, LocalData)]

  val successImageResponse = { (im: ImageData, asset: AssetId, convId: RConvId) => Right(AssetAddEvent(Uid(), convId, EventId(1), new Date, selfUser.id, asset, im.copy(remoteId = Some(RImageDataId()), sent = true))) }

  lazy val selfUser = UserData("test")
  lazy val conv = ConversationData(ConvId(), RConvId(), None, selfUser.id, ConversationType.Group)

  lazy val service = new MockZMessaging() {

    convsStorage.insert(conv)
    usersStorage.insert(selfUser)

    override lazy val imageClient: ImageAssetClient = new ImageAssetClient(znetClient, cache) {
      override def postImageAssetData(image: ImageData, assetId: AssetId, convId: RConvId, data: LocalData, nativePush: Boolean): ErrorOrResponse[AssetAddEvent] = {
        postImageRequest = Some((image, convId, data))
        CancellableFuture.successful(postImageResponse(image, assetId, convId))
      }
    }

    users.selfUserId := selfUser.id
  }

  def cache = service.cache
  def handler = service.imageassetSync

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    ZMessaging.context = Robolectric.application
  }

  def generateImageAsset(resource: String = "/images/penguin_128.png") = {
    Await.result(service.assetGenerator.generateWireAsset(AssetId(), imageAssetFor(BitmapFactory.decodeStream(getClass.getResourceAsStream(resource))), RConvId(), profilePicture = false), 15.seconds)
  }

  def imageAssetFor(bitmap: Bitmap): ImageData = {
    val bos = new ByteArrayOutputStream()
    bitmap.compress(CompressFormat.PNG, 0, bos)

    new ImageData("full", Mime.Unknown, bitmap.getWidth, bitmap.getHeight, bitmap.getWidth, bitmap.getHeight, bos.size(), Some(RImageDataId())) {
      override lazy val data: Option[Array[Byte]] = Some(bos.toByteArray)
    }
  }

  scenario("post preview image data") {
    val Seq(preview, _) = generateImageAsset().versions
    postImageResponse = successImageResponse

    val res = Await.result(service.imageassetSync.postImageData(conv.remoteId, AssetId(), preview, LocalData(preview.data.get)), 3.seconds)
    res shouldEqual SyncResult.Success

    postImageRequest.map(_._1) shouldEqual Some(preview)
  }

  scenario("post full image data") {
    val asset = generateImageAsset()
    Await.ready(service.imageAssets.updateImageAsset(asset), 1.second)
    asset.versions should have size 2
    val im = asset.versions(1)
    val file = Await.result(cache.getEntry(im.cacheKey), 10.seconds).value.cacheFile

    file should exist

    info(file.toString)
    info(asset.toString)

    postImageResponse = successImageResponse
    val res = Await.result(handler.postImageData(conv.remoteId, asset.id, im, LocalData(file)), 1.second)
    res shouldEqual SyncResult.Success

    postImageRequest shouldEqual Some((im, conv.remoteId, LocalData(file)))
    file should exist // file should still exist after syncing (it will be cleaned by normal cache cleanup

    val updated = Await.result(service.imageAssets.getImageAsset(asset.id), 5.seconds).get
    updated.convId shouldEqual conv.remoteId
    updated.versions should have size 2
    val im1 = updated.versions(1)
    im1 should not be im
    im1 shouldEqual im.copy(remoteId = im1.remoteId, sent = true)

    val entry = Await.result(cache.getEntry(im1.cacheKey), 1.second)
    entry should be('defined)
    entry.get.cacheFile should exist
    entry.get.cacheFile.length() shouldEqual im.size
  }
}
