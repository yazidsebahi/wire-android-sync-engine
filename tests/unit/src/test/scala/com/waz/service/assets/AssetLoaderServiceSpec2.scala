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
package com.waz.service.assets

import com.waz.api.NetworkMode
import com.waz.api.impl.ErrorResponse
import com.waz.cache.{CacheEntry, CacheEntryData, CacheService}
import com.waz.model.AssetMetaData.Image
import com.waz.model._
import com.waz.service.NetworkModeService
import com.waz.service.downloads.AssetLoader.DownloadFailedException
import com.waz.service.downloads.AssetLoaderService.MaxRetriesErrorMsg
import com.waz.service.downloads.{AssetLoader, AssetLoaderService}
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.TestBackoff
import com.waz.threading.CancellableFuture
import com.waz.utils.events.Signal
import com.waz.znet.Response

class AssetLoaderServiceSpec2 extends AndroidFreeSpec {

  val network      = mock[NetworkModeService]
  val cacheService = mock[CacheService]
  val loader       = mock[AssetLoader]
  val service      = new AssetLoaderService()

  val networkMode  = Signal[NetworkMode]()

  val cacheKey = CacheKey("cachekey")


  val wireAsset = AssetData(
    mime      = Mime.Image.Jpg,
    remoteId  = Some(RAssetId("3-2-62cee8ff-6ebd-4cd3-8bc0-d6ea38392f9d")),
    token     = Some(AssetToken("nJw0Dac8SUR1yYuzlSuidg==")),
    otrKey    = Some(AESKey("RKqkqZ7SmWuR+ZTCnHt9aHpPvLBnwaYi5JkR38f8LnA=")),
    sha       = Some(Sha256("1AQ4br9Zv2vgB82QLKjg4lYSO8PnVaZXMQd9qB4QqPY=")),
    metaData  = Some(Image(Dim2(1080,720), Image.Tag.Empty))
  )

  AssetLoaderService.backoff = TestBackoff()

  override protected def beforeEach() = {
    super.beforeEach()
    networkMode ! NetworkMode.WIFI
  }

  feature("Download retries") {
    scenario("download successful on first attempt") {
      val savedEntry = cacheEntry(cacheKey, Uid())
      (loader.loadAsset _).expects(wireAsset, *, *).returning(CancellableFuture.successful(savedEntry))
      result(service.loadRevealAttempts(wireAsset)(loader)) shouldEqual (Some(savedEntry), 1)
    }

    scenario("download after multiple retries") {
      val savedEntry = cacheEntry(cacheKey, Uid())

      var attempts = 0

      (loader.loadAsset _).expects(wireAsset, *, *).anyNumberOfTimes().onCall { (data, callback, force) =>
        attempts += 1
        attempts match {
          case 1 | 2 => CancellableFuture.failed(DownloadFailedException(ErrorResponse(ErrorResponse.TimeoutCode, "", "")))
          case 3 => CancellableFuture.successful(savedEntry)
          case _ => fail("Unexpected number of call attempts")
        }
      }

      result(service.loadRevealAttempts(wireAsset)(loader)) shouldEqual (Some(savedEntry), 3)
    }

    scenario("give up after max retries") {
      (loader.loadAsset _).expects(wireAsset, *, *).anyNumberOfTimes.returning(CancellableFuture.failed(DownloadFailedException(ErrorResponse(ErrorResponse.TimeoutCode, "", ""))))
      assert(intercept[Exception](result(service.loadRevealAttempts(wireAsset)(loader))).getMessage == MaxRetriesErrorMsg)
    }
  }

  feature("Failures") {
    scenario("Unrecoverable failure should abort download") {

      (loader.loadAsset _).expects(wireAsset, *, *).anyNumberOfTimes.returning(CancellableFuture.failed(DownloadFailedException(ErrorResponse(Response.Status.Forbidden, "", ""))))
      assert(!intercept[DownloadFailedException](result(service.loadRevealAttempts(wireAsset)(loader))).isRecoverable)
    }
  }

  feature("Downloads on Wifi") {
    //TODO
    scenario("Switch from 'download images only on wifi' to 'download images always' should trigger download??") {
      fail()
    }
  }

  feature("Cancelling") {
    //TODO
    scenario("Cancelling active download does not discard work") {
      fail()
    }

    scenario("Cancelling non-active download does not start work") {
      fail()
    }
  }

  def cacheEntry(key: CacheKey, fileId: Uid) = new CacheEntry(new CacheEntryData(key), cacheService)

}
