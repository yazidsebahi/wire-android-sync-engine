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
package com.waz.service.images

import java.io.InputStream

import android.graphics.{Bitmap, BitmapFactory}
import com.waz.RobolectricUtils
import com.waz.api.impl.LocalBitmapAsset
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.model._
import com.waz.service.images.ImageLoader.Metadata
import com.waz.testutils.Matchers._
import com.waz.testutils.{DefaultPatienceConfig, MockUiModule, MockZMessaging}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Await
import scala.concurrent.duration._

class ImageAssetGeneratorSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with BeforeAndAfter with OptionValues with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig { test =>

  lazy val zms = new MockZMessaging()
  lazy val generator = zms.assetGenerator
  lazy val ui = new MockUiModule(zms)

  def imageAssetFor(bitmap: Bitmap): ImageData = ui.images.getOrCreateImageAssetFrom(bitmap).asInstanceOf[LocalBitmapAsset].data.versions.head

  def imageAssetFor(imageName: String): ImageData = imageAssetFor(BitmapFactory.decodeStream(getClass.getResourceAsStream(s"/images/$imageName")))

  def imageAssetFor(stream: => InputStream): ImageData = imageAssetFor(BitmapFactory.decodeStream(stream))

  feature("profile image generation") {

    def generate(im: ImageData) = {
      Await.result(generator.generateWireAsset(AssetId(), im, RConvId(), profilePicture = true), 25.seconds).versions
    }

    scenario("generate assets for small png image") {
      val Seq(small, medium) = generate(imageAssetFor("penguin_128.png"))
      info(s"generated: $small, $medium")

      small.width shouldEqual 128
      small.height shouldEqual 128
      small.mime shouldEqual Mime.Jpg
      small.tag shouldEqual "smallProfile"
      small.size should be <= 100 * 1024

      medium.width shouldEqual 128
      medium.tag shouldEqual "medium"

      val Seq(smallEntry, mediumEntry) = Seq(small, medium) map (img => Await.result(zms.cache.getEntry(img.cacheKey), 10.seconds).value)

      smallEntry.cacheFile should exist
      smallEntry.cacheFile.length shouldEqual small.size
      mediumEntry.cacheFile should exist
      mediumEntry.cacheFile.length shouldEqual medium.size
    }

    scenario("generate assets for big png image") {
      val Seq(small, medium) = generate(imageAssetFor("big.png"))

      info(s"generated assets: [$small, $medium]")

      small.width shouldEqual 280
      small.height shouldEqual 280
      small.mime shouldEqual Mime.Jpg
      small.tag shouldEqual "smallProfile"
      small.size should be < 25 * 1024

      medium.width shouldEqual 1920
      medium.height shouldEqual 1080
      medium.tag shouldEqual "medium"
    }

    scenario("generate assets for huge jpg image") {
      val entry = imageAssetFor(Bitmap.createBitmap(10 * 1024, 3280, Bitmap.Config.ARGB_8888))
      val Seq(small, medium) = generate(entry)

      info(s"generated assets: [$small, $medium]")

      small.width shouldEqual 280
      small.height shouldEqual 280
      small.mime shouldEqual Mime.Jpg
      small.tag shouldEqual "smallProfile"
      small.size should be < 25 * 1024

      medium.width should be < 3000
      medium.height should be <  1000
      medium.tag shouldEqual "medium"
    }
  }

  feature("generate from file") {
    def generate(im: ImageData) = {
      Await.result(generator.generateWireAsset(AssetId(), im, RConvId(), profilePicture = false), 25.seconds).versions
    }

    scenario("generate assets for small png image") {

      val entry = imageAssetFor("penguin_128.png")
      val Seq(small, medium) = generate(entry)

      small.data should be('defined)
      medium.width shouldEqual 128
      medium.height shouldEqual 128
      medium.mime shouldEqual Mime.Png

      withDelay {
        zms.cache.getEntry(small.cacheKey) should eventually(be(None))
      }

      val cached = Await.result(zms.cache.getEntry(medium.cacheKey), 10.seconds).value

      cached.cacheFile should exist
      cached.cacheFile.length shouldEqual medium.size
    }

    scenario("generate assets for big png image") {

      val entry = imageAssetFor("big.png")
      val assets = generate(entry)

      assets.last.width shouldEqual 1920
      assets.last.height shouldEqual 1080

      assets.last.mime shouldEqual Mime.Jpg
    }

    scenario("generate assets for huge jpg image") {

      val entry = imageAssetFor("huge.jpg")
      val assets = generate(entry)

      assert(assets.last.width < 2560)

      assets.last.width shouldEqual assets.last.origWidth
      assets.last.height shouldEqual assets.last.origHeight
    }

    scenario("generate assets for malformed jpg image") {
      val image = imageAssetFor(Bitmap.createBitmap(20 * 1024, 1280, Bitmap.Config.ARGB_8888))
      val assets = generate(image)

      assets.last.width shouldEqual assets.last.origWidth
      assets.last.height shouldEqual assets.last.origHeight
    }
  }

  feature("generate from bitmap") {

    def generate(im: ImageData) = Await.result(generator.generateWireAsset(AssetId(), im, RConvId(), profilePicture = false), 15.seconds).versions

    def generateAssets(resource: String) = generate(imageAssetFor(resource))

    scenario("generate assets for small image") {
      val assets = generateAssets("penguin_128.png")

      assets.last.width shouldEqual 128
      assets.last.height shouldEqual 128

      assets.last.mime shouldEqual Mime.Png
    }

    scenario("generate assets for big image") {
      val assets = generateAssets("big.png")

      assets.last.width shouldEqual 1920
      assets.last.height shouldEqual 1080

      assets.last.mime shouldEqual Mime.Jpg
    }

    scenario("generate assets for huge image") {
      val assets = generateAssets("huge.jpg")

      assert(assets.last.width < 2560)

      assets.last.width shouldEqual assets.last.origWidth
      assets.last.height shouldEqual assets.last.origHeight

      assets.last.mime shouldEqual Mime.Jpg
    }

    scenario("generate assets for very wide image") {
      val assets = generate(imageAssetFor(Bitmap.createBitmap(10 * 1024, 1280, Bitmap.Config.ARGB_8888)))

      assets.last.width  should be < 5000
      (assets.last.width * assets.last.height) should be < ImageAssetGenerator.MaxImagePixelCount.toInt

      assets.last.width shouldEqual assets.last.origWidth
      assets.last.height shouldEqual assets.last.origHeight

      assets.last.mime shouldEqual Mime.Png
    }

    scenario("generate assets for very tall image") {
      val assets = generate(imageAssetFor(Bitmap.createBitmap(1024, 10 * 1280, Bitmap.Config.ARGB_8888)))

      assets.last.width should be < 1024

      assets.last.width shouldEqual assets.last.origWidth
      assets.last.height shouldEqual assets.last.origHeight

      assets.last.mime shouldEqual Mime.Png
    }
  }

  feature("recode based on mime type") {

    lazy val tmpEntry = ui.cache.addData("tmp", Array.ofDim(100)).await()

    scenario("recode profile image") {
      Seq(Mime.Jpg, Mime.Png) foreach { mime =>
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.SmallProfileOptions) shouldEqual false
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.MediumProfileOptions) shouldEqual false
      }
      Seq(Mime.Gif, Mime.Tiff, Mime.WebP, Mime.Unknown) foreach { mime =>
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.SmallProfileOptions) shouldEqual true
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.MediumProfileOptions) shouldEqual true
      }
    }

    scenario("recode regular image") {
      generator.shouldRecode(tmpEntry, Metadata(10, 10, Mime.Gif), ImageAssetGenerator.PreviewOptions) shouldEqual true
      generator.shouldRecode(tmpEntry, Metadata(10, 10, Mime.Gif), ImageAssetGenerator.MediumOptions) shouldEqual false

      Seq(Mime.Jpg, Mime.Png) foreach { mime =>
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.PreviewOptions) shouldEqual false
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.MediumOptions) shouldEqual false
      }

      Seq(Mime.Tiff, Mime.WebP, Mime.Unknown) foreach { mime =>
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.PreviewOptions) shouldEqual true
        generator.shouldRecode(tmpEntry, Metadata(10, 10, mime), ImageAssetGenerator.MediumOptions) shouldEqual true
      }
    }
  }
}
