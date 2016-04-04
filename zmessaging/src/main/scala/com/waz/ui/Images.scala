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
package com.waz.ui

import android.content.{ContentResolver, Context}
import android.graphics.Bitmap
import android.media.ExifInterface
import android.net.Uri
import android.os.Parcel
import com.waz.ZLog._
import com.waz.api.impl.ImageAsset.Parcelable
import com.waz.api.impl._
import com.waz.bitmap.BitmapDecoder
import com.waz.bitmap.BitmapUtils.Mime
import com.waz.Control.getOrUpdate
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.{JsonDecoder, returning}
import com.waz.{HockeyApp, api, bitmap}

import scala.util.hashing.MurmurHash3

class Images(context: Context, bitmapLoader: BitmapDecoder)(implicit ui: UiModule) {
  import Images._
  private implicit val dispatcher = Threading.ImageDispatcher

  val images = new UiCache[AssetId, ImageAsset](lruSize = 20)
  val localImages = new UiCache[AssetId, ImageAsset](lruSize = 5)
  val zms = ui.zms

  def getImageAsset(id: AssetId): ImageAsset = getOrUpdate(images)(id, new ImageAsset(id))

  def getImageAsset(p: Parcel): api.ImageAsset = {
    p.readInt() match {
      case Parcelable.FlagEmpty => ImageAsset.Empty
      case Parcelable.FlagWire => getImageAsset(AssetId(p.readString()))
      case Parcelable.FlagLocal => getLocalImageAsset(JsonDecoder.decode[ImageAssetData](p.readString()))
    }
  }

  def getOrCreateUriImageAsset(uri: Uri): api.ImageAsset = {
    if (uri == null || uri.toString == "null") {
      HockeyApp.saveException(new NullPointerException("image uri is null"), "ImageAssetFactory does not accept null uris.")
      ImageAsset.Empty
    } else {
      getLocalImageAsset(ImageAssetData(AssetId(uri.toString), RConvId(), Seq(ImageData("full", Mime.Unknown, 0, 0, 0, 0, 0, url = Some(uri.toString)))))
    }
  }

  def getLocalImageAsset(data: ImageAssetData) = getOrUpdate(localImages)(data.id, new LocalImageAsset(data))

  def getOrCreateImageAssetFrom(bytes: Array[Byte]): api.ImageAsset = {
    if (bytes == null || bytes.isEmpty) ImageAsset.Empty
    else {
      val im = new ImageData("full", Mime.Unknown, 0, 0, 0, 0, 0, Some(RImageDataId())) {
        override lazy val data: Option[Array[Byte]] = Some(bytes)
      }
      getLocalImageAsset(ImageAssetData(idFor(bytes), RConvId(), Seq(im)))
    }
  }

  def getOrCreateMirroredImageAssetFrom(bytes: Array[Byte]): api.ImageAsset = {
    if (bytes == null || bytes.isEmpty) ImageAsset.Empty
    else {
      val im = new ImageData("full", Mime.Unknown, 0, 0, 0, 0, 0, Some(RImageDataId())) {
        override lazy val data: Option[Array[Byte]] = Some(bytes)
      }
      val id = AssetId("mirror_" + idFor(bytes).str)
      returning(getLocalImageAsset(ImageAssetData(id, RConvId(), Seq(im))))(_.setMirrored(true))
    }
  }

  def getOrCreateImageAssetFromResourceId(resourceId: Int): api.ImageAsset =
    getOrCreateUriImageAsset(Uri.parse(s"${ContentResolver.SCHEME_ANDROID_RESOURCE}://${context.getPackageName}/$resourceId"))

  def getOrCreateImageAssetFrom(bitmap: Bitmap, orientation: Int = ExifInterface.ORIENTATION_NORMAL): api.ImageAsset = {
    if (bitmap == null || bitmap == com.waz.bitmap.EmptyBitmap) ImageAsset.Empty
    else new LocalBitmapAsset(bitmap, orientation)
  }
}

object Images {
  private implicit val logTag: LogTag = logTagFor[Images]

  val EmptyBitmap = bitmap.EmptyBitmap

  final def idFor(bytes: Array[Byte]): AssetId = AssetId(s"bytes-${MurmurHash3.bytesHash(bytes)}")
}

trait ImagesComponent {
  val images: Images
}
