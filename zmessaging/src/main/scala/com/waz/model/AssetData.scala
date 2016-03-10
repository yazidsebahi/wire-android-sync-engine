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
package com.waz.model

import android.database.Cursor
import android.net.Uri
import android.util.Base64
import com.waz.ZLog._
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.model.ImageAssetData.{ImageAssetDataDecoder, ImageAssetDataEncoder}
import com.waz.model.otr.{OtrKey, Sha256, SignalingKey}
import com.waz.utils.JsonDecoder._
import com.waz.utils.{JsonDecoder, JsonEncoder, LoggedTry}
import org.json.JSONObject

trait AssetData {
  val id: AssetId 
  val assetType: AssetType

  def json: String
}

object AssetData {

  implicit object AssetDataDao extends Dao[AssetData, AssetId] {
    val Id = id[AssetId]('_id, "PRIMARY KEY").apply(_.id)
    val Asset = text[AssetType]('asset_type, _.name, AssetType.valueOf)(_.assetType)
    val Data = text('data)(_.json)

    override val idCol = Id
    override val table = Table("Assets", Id, Asset, Data)

    override def apply(implicit cursor: Cursor): AssetData = {
      val tpe: AssetType = Asset
      tpe match {
        case AssetType.Image => JsonDecoder.decode(Data)(ImageAssetDataDecoder)
      }
    }
  }
}

sealed trait AssetContent

case class ImageData(tag: String,
                     mime: String,
                     width: Int,
                     height: Int,
                     origWidth: Int,
                     origHeight: Int,
                     size: Int = 0, // byte size of binary image data
                     remoteId: Option[RImageDataId] = None,
                     data64: Option[String] = None, // image data in base64 format
                     sent: Boolean = false,
                     url: Option[String] = None,
                     proxyPath: Option[String] = None,     // path to wire proxy
                     otrKey: Option[OtrKey] = None, // otr symmetric encryption key
                     sha256: Option[Sha256] = None
                    ) extends AssetContent {

  import ImageData._
  require(remoteId.isDefined || url.isDefined || proxyPath.isDefined, "RemoteId, url or proxyPath has to be defined")
  require(data64.forall(_.nonEmpty), "data can not be empty (if specified)")

  lazy val data = data64.flatMap(data => LoggedTry(Base64.decode(data, Base64.DEFAULT)).toOption)

  lazy val uri = url map Uri.parse

  def fileExtension = mime.substring(mime.indexOf('/') + 1)

  def cacheKey = ImageData.cacheKey(remoteId, url)

  override def toString: LogTag = s"ImageData($tag, $mime, $width, $height, $origWidth, $origHeight, $size, $remoteId, data: ${data64.map(_.take(10))}, $proxyPath, otrKey: $otrKey, $sent, $url)"
}

case object UnsupportedAssetContent extends AssetContent

object ImageData {

  private implicit val logTag: LogTag = logTagFor(ImageData)

  def cacheKey(remoteId: Option[RImageDataId], url: Option[String] = None) = s"image-data://${remoteId.getOrElse("")}/${url.getOrElse("")}"

  object Tag {
    val Preview = "preview"
    var Medium = "medium"
    val MediumPreview = "mediumPreview" // for still video or gif preview
    val SmallProfile = "smallProfile"
  }

  implicit lazy val ImageDataOrdering: Ordering[ImageData] = new Ordering[ImageData] {
    import com.waz.model.ImageData.Tag._

    override def compare(x: ImageData, y: ImageData): Int = {
      if (x.width == y.width) {
        if (x.tag == Preview || y.tag == Medium) -1
        else if (x.tag == Medium || y.tag == Preview) 1
        else Ordering.Int.compare(x.size, y.size)
      } else Ordering.Int.compare(x.width, y.width)
    }
  }

  implicit lazy val ImageDataDecoder: JsonDecoder[ImageData] = new JsonDecoder[ImageData] {
    override def apply(implicit js: JSONObject): ImageData = {
      val key = JsonDecoder.opt[SignalingKey]('otrKey).map(_.encKey).orElse(decodeOptString('otrKey)).map(OtrKey(_))

      new ImageData('tag, 'mime, 'width, 'height, 'orig_width, 'orig_height, 'size, decodeOptId[RImageDataId]('remoteId), 'data, 'sent, 'url, 'proxyPath, key, decodeOptString('sha256).map(Sha256(_)))
    }
  }

  implicit lazy val ImageDataEncoder: JsonEncoder[ImageData] = new JsonEncoder[ImageData] {
    override def apply(image: ImageData): JSONObject = JsonEncoder { o =>
      o.put("tag", image.tag)
      o.put("mime", image.mime)
      o.put("width", image.width)
      o.put("height", image.height)
      o.put("orig_width", image.origWidth)
      o.put("orig_height", image.origHeight)
      o.put("size", image.size)
      image.remoteId.foreach(id => o.put("remoteId", id.str))
      image.data64.foreach(o.put("data", _))
      o.put("sent", image.sent)
      image.url.foreach(o.put("url", _))
      image.proxyPath.foreach(o.put("proxyPath", _))
      image.otrKey.foreach { k => o.put("otrKey", k.str) }
      image.sha256.foreach { s => o.put("sha256", s.str) }
    }
  }
}

case class ImageAssetData(id: AssetId, convId: RConvId, versions: Seq[ImageData]) extends AssetData {
  require(id.str.nonEmpty, "ImageAssetData id can not be empty")

  override val assetType: AssetType = AssetType.Image

  override def json: String = JsonEncoder.encodeString(this)(ImageAssetDataEncoder)

  def updated(image: ImageData) = {
    val versionsMap: Map[String, ImageData] = versions.map(v => v.tag -> v)(collection.breakOut)
    copy(versions = (versionsMap + (image.tag -> image)).values.toSeq.sorted)
  }

  def width = versions.lastOption.fold(0)(_.origWidth)
  def height = versions.lastOption.fold(0)(_.origHeight)

  def isEmpty = versions.isEmpty
  def nonEmpty = versions.nonEmpty
}

object ImageAssetData {
  import ImageData._
  val Empty = ImageAssetData(AssetId("empty"), RConvId(), Nil)

  implicit lazy val ImageAssetDataDecoder: JsonDecoder[ImageAssetData] = new JsonDecoder[ImageAssetData] {
    override def apply(implicit js: JSONObject): ImageAssetData = ImageAssetData(decodeId[AssetId]('id), decodeId[RConvId]('convId), decodeSeq[ImageData]('versions))
  }

  implicit lazy val ImageAssetDataEncoder: JsonEncoder[ImageAssetData] = new JsonEncoder[ImageAssetData] {
    override def apply(data: ImageAssetData): JSONObject = JsonEncoder { o =>
      o.put("id", data.id.str)
      o.put("convId", data.convId.str)
      o.put("versions", JsonEncoder.arr(data.versions)(ImageData.ImageDataEncoder))
    }
  }
}
