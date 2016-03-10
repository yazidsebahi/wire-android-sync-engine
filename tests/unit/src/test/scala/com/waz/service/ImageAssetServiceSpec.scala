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

import java.io.File

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.cache.{CacheEntry, CacheService}
import com.waz.model.AssetData.AssetDataDao
import com.waz.model.ConversationData.{ConversationDataDao, ConversationType}
import com.waz.model._
import com.waz.service.images.ImageAssetService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.ImageAssetClient
import com.waz.testutils.Matchers._
import com.waz.testutils.{EmptySyncService, MockZMessaging, RoboPermissionProvider}
import com.waz.threading.CancellableFuture
import com.waz.utils.JsonDecoder
import com.waz.znet.Request
import com.waz.znet.Request.ProgressCallback
import org.json.JSONArray
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

import scala.concurrent.Await
import scala.concurrent.duration._

class ImageAssetServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils {

  val fullFile = new File(getClass.getResource("/images/penguin.png").getFile).getAbsoluteFile
  val mediumFile = new File(getClass.getResource("/images/penguin_240.png").getFile).getAbsoluteFile
  val imData = {
    val is = getClass.getResourceAsStream("/images/penguin_128.png")
    try {
      Stream.continually(is.read()).takeWhile(_ != -1).map(_.toByte).toArray
    } finally is.close()
  }

  val asset = ImageAssetData(AssetId(), RConvId(), Seq(
    ImageData("preview", "image/png", 128, 128, 480, 492, imData.length, Some(RImageDataId())),
    ImageData("medium", "image/png", 240, 246, 480, 492, mediumFile.length.toInt, Some(RImageDataId())),
    ImageData("full", "image/png", 480, 492, 480, 492, fullFile.length.toInt, Some(RImageDataId()))
  ))

  var zms: ZMessaging = _

  def service: ImageAssetService = zms.imageAssets
  def cacheService: CacheService = zms.cache
  implicit def db: SQLiteDatabase = zms.storage.dbHelper.getWritableDatabase

  var syncRequest = None: Option[SearchQueryCache]
  var sync: SyncServiceHandle = new EmptySyncService
  var loadImageResult = None: Option[CacheEntry]

  before {
    zms = new MockZMessaging() { self =>
      override lazy val imageClient: ImageAssetClient = new ImageAssetClient(znetClient, cache) {
        override def loadImageAsset(req: Request[Unit], cb: ProgressCallback): CancellableFuture[Option[CacheEntry]] = CancellableFuture.successful(loadImageResult)
      }
    }
    zms.permissions.setProvider(new RoboPermissionProvider)
  }

  after {
    syncRequest = None
    zms.storage.close()
  }

  scenario("Save image to gallery") {
    AssetDataDao.insertOrReplace(asset)

    val entry = Await.result(cacheService.addFile("key", fullFile), 10.seconds)
    loadImageResult = Some(entry)

    val uri = zms.imageLoader.saveImageToGallery(asset).await()(patience(15.seconds))
    uri should be('defined)

    info(s"image saved to: ${uri.get}")

    val savedFile = new File(uri.get.getPath)
    savedFile should exist
    savedFile.length() shouldEqual fullFile.length()
  }

  scenario("Process image asset events") {
    AssetDataDao.deleteAll

    val json = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/events/images.json")).getLines().mkString("\n")
    val events = JsonDecoder.array[Event](new JSONArray(json))
    val convIds = events.collect{ case ev: ConversationEvent => ev.convId } .toSet

    ConversationDataDao.insertOrReplace(convIds.map(id => ConversationData(ConvId(id.str), id, None, UserId(), ConversationType.Group)))

    events foreach {
      case AssetAddEvent(_, convId, _, _, _, id, d: ImageData) => Await.result(service.updateImageAsset(id, convId, d), 5.seconds)
      case ev => fail(s"unexpected event: $ev")
    }

    awaitUi(1.second)
    val assetIds = events.collect { case AssetAddEvent(_, _, _, _, _, id, d) => id } .toSet
    info(s"got assets: ${AssetDataDao.list}")
    AssetDataDao.list.map(_.id).toSet shouldEqual assetIds
  }

  scenario("Process image asset events1") {
    AssetDataDao.deleteAll

    val json = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/events/images.json")).getLines().mkString("\n")
    val events = JsonDecoder.array[Event](new JSONArray(json)).take(2)
    val convIds = events.collect{ case ev: ConversationEvent => ev.convId } .toSet

    ConversationDataDao.insertOrReplace(convIds.map(id => ConversationData(ConvId(), id, None, UserId(), ConversationType.Group)))

    val assetsData = events.collect { case AssetAddEvent(_, convId, _, _, _, id, d: ImageData) => (id, convId, d) }
    info(s"assets: $assetsData")
    val preview = assetsData(0)
    val medium = assetsData(1)

    Await.result(service.updateImageAsset(preview._1, preview._2, preview._3), 5.seconds)

    awaitUi(1.second)
    AssetDataDao.list should have size 1

    Await.result(service.updateImageAsset(medium._1, medium._2, medium._3), 5.seconds)

    info(s"got assets: ${AssetDataDao.list}")

    val assetIds = events.collect { case AssetAddEvent(_, _, _, _, _, id, d) => id } .toSet
    AssetDataDao.list.map(_.id).toSet shouldEqual assetIds
  }
}
