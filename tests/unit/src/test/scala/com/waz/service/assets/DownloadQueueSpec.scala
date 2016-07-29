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

import com.waz.model.{Mime, _}
import com.waz.service.downloads.DownloadQueue
import com.waz.service.downloads.DownloadQueue.Entry
import com.waz.service.downloads.DownloadRequest.ImageAssetRequest
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers, RobolectricTests}

import scala.util.Random

class DownloadQueueSpec extends FeatureSpec with Matchers with GeneratorDrivenPropertyChecks with RobolectricTests {

  scenario("Sort entries according to waiting flag and timestamp") {
    def key = ImageAssetRequest("", RConvId(), AssetKey(Left(RAssetDataId()), None, AESKey.Empty, Sha256.Empty), Mime.Unknown)

    val entries = Seq(
      Entry(key, true, 10),
      Entry(key, true, 100),
      Entry(key, true, 1000),
      Entry(key, false, 999),
      Entry(key, false, 99),
      Entry(key, false, 1)
    )

    val gen = Gen.resultOf { (seed: Long) => new Random(seed).shuffle(entries) } suchThat(_.length == entries.length)

    forAll(gen) { es =>
      val queue = new DownloadQueue()
      es foreach {
        case Entry(asset, uiWaiting, time) => queue.put(asset, uiWaiting, time)
      }
      Iterator.continually(queue.poll()).takeWhile(_.isDefined).map(_.get).toSeq shouldEqual entries.map(_.key)
    }

  }
}
