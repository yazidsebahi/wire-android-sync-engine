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
package com.waz.service

import java.io.{File, FileInputStream}

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.api.impl.{AssetForUpload, ErrorResponse}
import com.waz.cache.{CacheEntry, CacheService}
import com.waz.content.Mime
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.assets.AssetService
import com.waz.sync.client.AssetClient
import com.waz.testutils.Matchers._
import com.waz.testutils.{MockZMessaging, RoboPermissionProvider}
import com.waz.threading.CancellableFuture
import com.waz.utils.IoUtils
import com.waz.znet.Request
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class AssetServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils {

  val fullFile = new File(getClass.getResource("/images/penguin.png").getFile).getAbsoluteFile
  val mediumFile = new File(getClass.getResource("/images/penguin_240.png").getFile).getAbsoluteFile
  val imData = {
    val is = getClass.getResourceAsStream("/images/penguin_128.png")
    try {
      Stream.continually(is.read()).takeWhile(_ != -1).map(_.toByte).toArray
    } finally is.close()
  }

  val asset = ImageAssetData(AssetId(), RConvId(), Seq(
    ImageData("preview", "image/png", 128, 128, 480, 492, imData.length, Some(RAssetDataId())),
    ImageData("medium", "image/png", 240, 246, 480, 492, mediumFile.length.toInt, Some(RAssetDataId())),
    ImageData("full", "image/png", 480, 492, 480, 492, fullFile.length.toInt, Some(RAssetDataId()))
  ))

  lazy val zms = new MockZMessaging() { self =>
    override lazy val assetClient: AssetClient = new AssetClient(zNetClient) {
      override def loadAsset(req: Request[Unit]) = CancellableFuture.successful(loadImageResult)
    }
    permissions.setProvider(new RoboPermissionProvider)
  }

  lazy val service: AssetService = zms.assets
  lazy val cacheService: CacheService = zms.cache
  implicit def db: SQLiteDatabase = zms.db.dbHelper.getWritableDatabase

  var loadImageResult: Either[ErrorResponse, CacheEntry] = _

  before {
    loadImageResult = Left(ErrorResponse.internalError("loading failed"))
  }

  feature("Image assets") {
    scenario("Save image to gallery") {
      zms.assetsStorage.insert(asset)

      val entry = Await.result(cacheService.addFile("key", fullFile), 10.seconds)
      loadImageResult = Right(entry)

      val uri = zms.imageLoader.saveImageToGallery(asset).await()(patience(15.seconds))
      uri should be('defined)

      info(s"image saved to: ${uri.get}")

      val savedFile = new File(uri.get.getPath)
      savedFile should exist
      savedFile.length() shouldEqual fullFile.length()
    }
  }

  feature("File assets") {

    scenario("Save asset to downloads") {
      val conv = zms.insertConv(ConversationData(ConvId(), RConvId(), None, UserId(), ConversationType.Group))
      val source = AssetForUpload(AssetId(), Some("name.txt"), Mime("text/txt"), None) { _ => getClass.getResourceAsStream("/images/penguin_128.png") }
      lazy val asset = service.addAsset(source, conv.remoteId).await()

      val file = service.saveAssetToDownloads(asset.id).await()
      file should be('defined)
      info(s"saved to file: $file")
      IoUtils.toByteArray(new FileInputStream(file.get)).toSeq shouldEqual IoUtils.toByteArray(source.openDataStream(context)).toSeq
    }

  }
}
