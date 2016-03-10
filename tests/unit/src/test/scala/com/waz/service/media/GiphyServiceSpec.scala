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
package com.waz.service.media

import com.waz.RobolectricUtils
import com.waz.model.{ImageAssetData, ImageData}
import com.waz.service.images.ImageAssetGenerator
import com.waz.sync.client.GiphyClient
import com.waz.testutils.Matchers._
import com.waz.threading.CancellableFuture
import com.waz.znet.ZNetClient.EmptyClient
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers, RobolectricTests}

class GiphyServiceSpec extends FeatureSpec with Matchers with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  var loadRandomResult: Seq[ImageData] = Nil
  val biggestRandomResult = ImageData("medium", "image/gif", 500, 256, 500, 256, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/giphy.gif"))
  val smallestRandomResult = ImageData("preview", "image/gif", 100, 51, 100, 51, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/100w_s.gif"))
  val randomResult = Seq(
    biggestRandomResult,
    ImageData("medium", "image/gif", 391, 200, 391, 200, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/200_d.gif")),
    ImageData("medium", "image/gif", 200, 102, 200, 102, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/200w_d.gif")),
    ImageData("preview", "image/gif", 195, 100, 195, 100, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/100_s.gif")),
    smallestRandomResult
  )

  var loadSearchResult: Seq[Seq[ImageData]] = Nil
  var keyword: String = ""
  val biggestSearchResult = ImageData("medium", "image/gif", 391, 200, 391, 200, ImageAssetGenerator.MaxGifSize - 1, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/200_d.gif"))
  val smallestSearchResult = ImageData("preview", "image/gif", 100, 120, 100, 120, 0, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/200_d.gif"))
  val searchResult = Seq(Seq(
    biggestSearchResult,
    ImageData("medium", "image/gif", 391, 200, 391, 200, ImageAssetGenerator.MaxGifSize + 1, None, None, sent = false, Some("http://s3.amazonaws.com/giphygifs/media/Ggjwvmqktuvf2/200_d.gif")),
    smallestSearchResult
  ))

  lazy val service = new GiphyService(new GiphyClient(new EmptyClient) {
    override def loadRandom() = CancellableFuture.successful(loadRandomResult)
    override def search(keyword: String, offset: Int, limit: Int) = {
      test.keyword = keyword
      CancellableFuture.successful(loadSearchResult)
    }
  })

  feature("random") {
    scenario("success") {
      loadRandomResult = randomResult
      val preview = smallestRandomResult.copy(origWidth = 500, origHeight = 256)
      service.getRandomGiphyImage should eventually(beMatching({
        case Seq(ImageAssetData(_, _, Seq(`preview`, `biggestRandomResult`))) => true
      }))
    }

    scenario("failed") {
      loadRandomResult = Nil
      service.getRandomGiphyImage should eventually(be(Nil))
    }
  }

  feature("translate") {
    scenario("success") {
      loadSearchResult = searchResult
      val keyword = "test"
      val preview = smallestSearchResult.copy(origWidth = 391, origHeight = 200)
      service.searchGiphyImage(keyword) should eventually(beMatching({
        case Seq(ImageAssetData(_, _, Seq(`preview`, `biggestSearchResult`))) => true
      }))
      test.keyword shouldEqual keyword
    }

    scenario("failed") {
      loadSearchResult = Nil
      service.searchGiphyImage("") should eventually(be(Nil))
    }
  }
}
