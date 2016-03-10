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
package com.waz.api.impl

import android.graphics.Bitmap
import android.net.Uri
import android.os.Parcel
import com.waz.RobolectricUtils
import com.waz.api.ImageAsset.BitmapCallback
import com.waz.api.ImageAssetFactory
import com.waz.model.AssetId
import com.waz.service.ZMessaging
import com.waz.testutils.{MockZMessaging, MockUiModule}
import com.waz.utils.IoUtils
import org.robolectric.Robolectric
import org.scalatest._

class ImageAssetSpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with RobolectricTests with RobolectricUtils {

  lazy val ui = new MockUiModule(new MockZMessaging())

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    ZMessaging.context = Robolectric.application
    ui.onCreate(Robolectric.application)
    ui.onResume()
  }

  feature("Parcelable") {

    scenario("Write and read Empty image") {
      val p = Parcel.obtain()
      ImageAsset.Empty.writeToParcel(p, 0)
      p.setDataPosition(0)
      ui.images.getImageAsset(p) shouldEqual ImageAsset.Empty
    }

    scenario("Write and read local image") {
      val p = Parcel.obtain()
      val im = ImageAssetFactory.getImageAsset(Uri.parse("content://test"))
      im.writeToParcel(p, 0)
      p.setDataPosition(0)
      ui.images.getImageAsset(p) shouldEqual im
    }

    scenario("Write and read wire image") {
      val p = Parcel.obtain()
      val im = ui.images.getImageAsset(AssetId())
      im.writeToParcel(p, 0)
      p.setDataPosition(0)
      ui.images.getImageAsset(p) shouldEqual im
    }
  }

  feature("Image loading") {

    lazy val img = ImageAssetFactory.getImageAsset(IoUtils.toByteArray(getClass.getResourceAsStream("/images/big.png")))

    scenario("Request small image from asset") {
      var res: Bitmap = null
      val handle = img.getBitmap(4, new BitmapCallback() {
        override def onBitmapLoaded(b: Bitmap, isPreview: Boolean): Unit = if (!isPreview) res = b
        override def onBitmapLoadingFailed(): Unit = ()
      })
      withDelay {
        res should not be null
        res.getWidth should be < 12
        res.getHeight should be < 12
      }
    }

    scenario("Request full bitmap from the same asset") {
      var res: Bitmap = null
      val handle = img.getSingleBitmap(1024, new BitmapCallback() {
        override def onBitmapLoaded(b: Bitmap, isPreview: Boolean): Unit = res = b
        override def onBitmapLoadingFailed(): Unit = ()
      })
      withDelay {
        res should not be null
        res.getWidth shouldEqual 1920
        res.getHeight shouldEqual 1080
      }
    }
  }
}
