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
package com.waz.messages

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import com.waz.api.MessageContent.Asset
import com.waz.api.ProvisionedApiSpec
import com.waz.api.impl.LocalImageAsset
import com.waz.cache.CacheEntry
import com.waz.model.{RConvId, RImageDataId}
import com.waz.service.ZMessaging
import com.waz.sync.client.ImageAssetClient
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.threading.CancellableFuture
import com.waz.utils.compareAndSet
import com.waz.znet.Request
import com.waz.znet.Request._
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.duration._

class ImageAssetMessageSpec extends FeatureSpec with Matchers with ProvisionedApiSpec {
  override val provisionFile: String = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = conversations.head
  lazy val messages = conv.getMessages

  scenario("Post a giphy search result as message") {
    val result = api.getGiphy.search("animated")
    (result shouldBe 'ready).soon
    val imageFromGiphy = result.head
    imageFromGiphy.shouldBeAnAnimatedGif

    (messages should have size 1).soon
    downloads.set(0)
    conv.sendMessage(new Asset(imageFromGiphy))
    (messages should have size 2).soon

    val postedImage = messages(1).getImage

    postedImage should not(be(a[LocalImageAsset]))
    postedImage.shouldBeAnAnimatedGif
    idle(5.seconds)
    postedImage.shouldBeAnAnimatedGif
    withClue("Image should not be downloaded but fetched from cache instead.")(downloads.get shouldEqual 0)
  }

  override lazy val zmessagingFactory: ZMessaging.Factory = (new ZMessaging(_, _, _) {
    override lazy val imageClient = new ImageAssetClient(znetClient, cache) {

      override def loadImageAsset(req: Request[Unit], cb: ProgressCallback): CancellableFuture[Option[CacheEntry]] = {
        downloads.incrementAndGet()
        super.loadImageAsset(req, cb)
      }
    }
  })

  private val downloads = new AtomicLong(0)
}
