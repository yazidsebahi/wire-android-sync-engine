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
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.model.AssetStatus.{DownloadFailed, UploadDone}
import com.waz.utils.JsonDecoder.{apply => _, opt => _}
import com.waz.utils._
import org.json.JSONObject

case class AssetData(id:          AssetId               = AssetId(), //TODO make independent of message id - will now be cache key
                     status:      AssetStatus           = AssetStatus.UploadNotStarted,
                     mime:        Mime                  = Mime.Unknown,
                     sizeInBytes: Long                  = 0L, //will be for metadata only??
                     name:        Option[String]        = None,
                     previewId:   Option[AssetId]       = None,
                     metaData:    Option[AssetMetaData] = None, //TODO can I move AssetMetaData into AssetData?
                     source:      Option[Uri]           = None,
                     proxyPath:   Option[String]        = None,
                     //TODO remove v2 attributes when transition period is over
                     convId:      Option[RConvId]       = None, //TODO remove
                     data64:      Option[String]        = None //TODO remove data from asset
                    ) {

  import AssetData._

  override def toString: String =
    s"""
       |AssetData:
       | id:            $id
       | status:        $status
       | mime:          $mime
       | sizeInBytes:   $sizeInBytes
       | preview:       $previewId
       | other fields:  $name, $metaData, $source, $proxyPath, $convId, $data64
       |
    """.stripMargin

  lazy val data = data64.flatMap(data => LoggedTry(Base64.decode(data, Base64.DEFAULT)).toOption)

  lazy val size = data.fold(sizeInBytes)(_.length)

  lazy val fileExtension = mime.extension

  lazy val assetKey = status match {
    case UploadDone(k) => Some(k)
    case DownloadFailed(k) => Some(k)
    case _ => None
  }

  val isImage = this match {
    case IsImage(_) => true
    case _ => false
  }

  //TODO feels untidy for non-image assets
  val tag = this match {
    case IsImage(_, t) => t
    case _ => ""
  }

  //TODO feels untidy for non-image/video assets
  val dimensions = this match {
    case IsImage(dim, _) => dim
    case _ => Dim2(0, 0)
  }

}

object AssetData {

  val NewImageAsset = AssetData(metaData = Some(AssetMetaData.Image(Dim2(0, 0), "full")))

  object IsImage {
    def unapply(asset: AssetData): Option[(Dim2, String)] = {
      asset match {
        case AssetData(_, _, _, _, _, _, Some(AssetMetaData.Image(dims, tag)), _, _, _, _) => Some((dims, tag))
        case _ => None
      }
    }
  }

  object HasData {
    def unapply(asset: AssetData): Option[Array[Byte]] = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(data64)) => asset.data
        case _ => None
      }
    }
  }

  object HasPreview {
    def unapply(asset: AssetData): Option[AssetId] = {
      asset match {
        case AssetData(_, _, _, _, _, pId, _, _, _, _, _) => pId
        case _ => None
      }
    }
  }

  val MaxAllowedAssetSizeInBytes = 26214383L
  // 25MiB - 32 + 15 (first 16 bytes are AES IV, last 1 (!) to 16 bytes are padding)
  val MaxAllowedBackendAssetSizeInBytes = 26214400L

  // 25MiB

  case class FetchKey(id: AssetId)

  case class UploadKey(id: AssetId)

  implicit object AssetDataDao extends Dao[AssetData, AssetId] {
    val Id    = id[AssetId]('_id, "PRIMARY KEY").apply(_.id)
    val Asset = text('asset_type, "").apply(_ => "")
    //TODO remove in migration
    val Data = text('data)(JsonEncoder.encodeString(_))

    override val idCol = Id
    override val table = Table("Assets", Id, Asset, Data)

    override def apply(implicit cursor: Cursor): AssetData = JsonDecoder.decode(Data)(AssetDataDecoder)
  }

  implicit lazy val AssetDataDecoder: JsonDecoder[AssetData] = new JsonDecoder[AssetData] {

    import JsonDecoder._

    override def apply(implicit js: JSONObject): AssetData =
      AssetData(
        'id, JsonDecoder[AssetStatus]('status), Mime('mime), 'sizeInBytes, 'name,
        'preview, opt[AssetMetaData]('metaData), decodeOptString('source).map(Uri.parse),
        'proxyPath, 'convId, 'data64
      )
  }

  implicit lazy val AssetDataEncoder: JsonEncoder[AssetData] = new JsonEncoder[AssetData] {
    override def apply(data: AssetData): JSONObject = JsonEncoder { o =>
      o.put("id", data.id.str)
      o.put("status", JsonEncoder.encode(data.status))
      o.put("mime", data.mime.str)
      o.put("sizeInBytes", data.sizeInBytes)
      data.name.foreach(o.put("name", _))
      data.previewId.foreach(p => o.put("preview", p.str))
      data.metaData.foreach(md => o.put("metaData", JsonEncoder.encode(md)))
      data.source.foreach(u => o.put("source", u.toString))
      data.proxyPath.foreach(o.put("proxyPath", _))
      data.convId.foreach(id => o.put("convId", id.str))
      data.data64.foreach(o.put("data64", _))
    }
  }
}

case class AssetToken(str: String) extends AnyVal

object AssetToken extends (String => AssetToken)

case class AssetKey(remoteId: RAssetId,
                    token:    Option[AssetToken]  = None, //public assets don't need
                    otrKey:   Option[AESKey]      = None, //public assets don't need
                    sha256:   Option[Sha256]      = None)

//TODO what's the difference between RAssetDataId and RemoteKey?
object AssetKey extends ((RAssetId, Option[AssetToken], Option[AESKey], Option[Sha256]) => AssetKey) {
  import JsonDecoder._

  implicit lazy val AssetKeyDecoder: JsonDecoder[AssetKey] = new JsonDecoder[AssetKey] {
    override def apply(implicit js: JSONObject): AssetKey = AssetKey(
      //TODO figure out where this gets saved and see if we can remove the if-else
      if (js.has("remoteId")) decodeRAssetDataId('remoteId) else decodeRAssetDataId('remoteKey),
      decodeOptString('token).map(AssetToken(_)),
      decodeOptString('otrKey).map(AESKey(_)),
      decodeOptString('sha256).map(Sha256(_))
    )
  }

  implicit lazy val AssetKeyEncoder: JsonEncoder[AssetKey] = new JsonEncoder[AssetKey] {
    override def apply(data: AssetKey): JSONObject = JsonEncoder { o =>
      o.put("remoteId", data.remoteId.str)
      data.token foreach(t => o.put("token", t.str))
      data.token foreach(k => o.put("otrKey", k.str))
      data.token foreach(s => o.put("sha256", s.str))
    }
  }
}


////TODO tidy up image data - do I need remoteKey??
//case class ImageData(tag:         String,
//                     mime:        String,
//                     width:       Int,
//                     height:      Int,
//                     origWidth:   Int,
//                     origHeight:  Int,
//                     size:        Int = 0, // byte size of binary image data
//                     remoteId:    Option[RAssetDataId] = None,
//                     data64:      Option[String] = None, // image data in base64 format
//                     sent:        Boolean = false,
//                     url:         Option[String] = None,
//                     proxyPath:   Option[String] = None, // path to wire proxy
//                     otrKey:      Option[AESKey] = None, // otr symmetric encryption key
//                     sha256:      Option[Sha256] = None
//                    ) {
//
//  import ImageData._
//  require(remoteId.isDefined || url.isDefined || proxyPath.isDefined, "RemoteId, url or proxyPath has to be defined")
//  require(data64.forall(_.nonEmpty), "data can not be empty (if specified)")
//
//  lazy val data = data64.flatMap(data => LoggedTry(Base64.decode(data, Base64.DEFAULT)).toOption)
//
//  lazy val uri = url map Uri.parse
//
//  def fileExtension = Mime(mime).extension
//
//  def cacheKey = proxyPath.fold(ImageData.cacheKey(remoteId, url))(path => s"image-data://$path")
//
//  override def toString: LogTag = s"ImageData($tag, $mime, $width, $height, $origWidth, $origHeight, $size, $remoteId, data: ${data64.map(_.take(10))}, $proxyPath, otrKey: $otrKey, $sent, $url)"
//}
//
//object ImageData {
//
//  private implicit val logTag: LogTag = logTagFor(ImageData)
//
//  //TODO how likely am I to get conflicting cache keys with v3 assets?
//  def cacheKey(remoteId: Option[RAssetDataId], url: Option[String] = None) = s"image-data://${remoteId.getOrElse("")}/${url.getOrElse("")}"
//
//  def apply(prev: Proto.Asset.Preview, id: Option[RAssetDataId]): ImageData = prev match {
//    case Proto.Asset.Preview(mime, size, key, sha, None) =>
//      ImageData(Tag.Preview, mime.str, 0, 0, 0, 0, size.toInt, id, None, sent = true, None, None, Some(key), Some(sha))
//    case Proto.Asset.Preview(mime, size, key, sha, Some(AssetMetaData.Image(d, t))) =>
//      ImageData(t.getOrElse(Tag.Preview), mime.str, d.width, d.height, d.width, d.height, size.toInt, id, None, sent = true, None, None, Some(key), Some(sha))
//  }
//
//  object Tag {
//    val Preview = "preview"
//    val Medium = "medium"
//    val MediumPreview = "mediumPreview" // for still video or gif preview
//    val SmallProfile = "smallProfile"
//  }
//
//  implicit lazy val ImageDataOrdering: Ordering[ImageData] = new Ordering[ImageData] {
//    import com.waz.model.ImageData.Tag._
//
//    override def compare(x: ImageData, y: ImageData): Int = {
//      if (x.width == y.width) {
//        if (x.tag == Tag.Preview || y.tag == Medium) -1
//        else if (x.tag == Medium || y.tag == Tag.Preview) 1
//        else Ordering.Int.compare(x.size, y.size)
//      } else Ordering.Int.compare(x.width, y.width)
//    }
//  }
//
//  import JsonDecoder._
//
//  implicit lazy val ImageDataDecoder: JsonDecoder[ImageData] = new JsonDecoder[ImageData] {
//    override def apply(implicit js: JSONObject): ImageData = {
//      val key = js.opt("otrKey") match {
//        case o: JSONObject => Try(implicitly[JsonDecoder[SignalingKey]].apply(o).encKey).toOption
//        case str: String => Some(AESKey(str))
//        case _ => None
//      }
//
//      new ImageData('tag, 'mime, 'width, 'height, 'orig_width, 'orig_height, 'size, decodeOptId[RAssetDataId]('remoteId), 'data, 'sent, 'url, 'proxyPath, key, decodeOptString('sha256).map(Sha256(_)))
//    }
//  }
//
//  implicit lazy val ImageDataEncoder: JsonEncoder[ImageData] = new JsonEncoder[ImageData] {
//    override def apply(image: ImageData): JSONObject = JsonEncoder { o =>
//      o.put("tag", image.tag)
//      o.put("mime", image.mime)
//      o.put("width", image.width)
//      o.put("height", image.height)
//      o.put("orig_width", image.origWidth)
//      o.put("orig_height", image.origHeight)
//      o.put("size", image.size)
//      image.remoteId.foreach(id => o.put("remoteId", id.str))
//      image.data64.foreach(o.put("data", _))
//      o.put("sent", image.sent)
//      image.url.foreach(o.put("url", _))
//      image.proxyPath.foreach(o.put("proxyPath", _))
//      image.otrKey.foreach { k => o.put("otrKey", k.str) }
//      image.sha256.foreach { s => o.put("sha256", s.str) }
//    }
//  }
//}

//case class ImageAssetData(id: AssetId, convId: RConvId, versions: Seq[ImageData]) extends AssetData {
//  require(id.str.nonEmpty, "ImageAssetData id can not be empty")
//
//  override def assetType: AssetType = AssetType.Image
//
//  def updated(image: ImageData) = {
//    val versionsMap: Map[String, ImageData] = versions.map(v => v.tag -> v)(collection.breakOut)
//    copy(versions = (versionsMap + (image.tag -> image)).values.toSeq.sorted)
//  }
//
//  def width = versions.lastOption.fold(0)(_.origWidth)
//  def height = versions.lastOption.fold(0)(_.origHeight)
//
//  def isEmpty = versions.isEmpty
//  def nonEmpty = versions.nonEmpty
//}

//object ImageAssetData {
//  import ImageData._
//  import JsonDecoder._
//  val Empty = ImageAssetData(AssetId("empty"), RConvId(), Nil)
//
//  def apply(uri: Uri): ImageAssetData = ImageAssetData(AssetId(uri.toString), RConvId(), Seq(ImageData("full", "image/*", 0, 0, 0, 0, 0, url = Some(uri.toString))))
//
//  implicit lazy val ImageAssetDataDecoder: JsonDecoder[ImageAssetData] = new JsonDecoder[ImageAssetData] {
//    override def apply(implicit js: JSONObject): ImageAssetData = ImageAssetData(decodeId[AssetId]('id), decodeId[RConvId]('convId), decodeSeq[ImageData]('versions))
//  }
//
//  implicit lazy val ImageAssetDataEncoder: JsonEncoder[ImageAssetData] = new JsonEncoder[ImageAssetData] {
//    override def apply(data: ImageAssetData): JSONObject = JsonEncoder { o =>
//      o.put("id", data.id.str)
//      o.put("convId", data.convId.str)
//      o.put("versions", JsonEncoder.arr(data.versions)(ImageData.ImageDataEncoder))
//    }
//  }
//}

//case class AnyAssetData(id:               AssetId,
//                        convId:           RConvId,
//                        mimeType:         Mime,
//                        sizeInBytes:      Long,
//                        name:             Option[String],
//                        metaData:         Option[AssetMetaData],
//                        preview:          Option[AssetPreviewData],
//                        source:           Option[Uri],
//                        originalMimeType: Option[Mime],
//                        status:           AssetStatus,
//                        lastUpdate:       Instant
//                       ) extends AssetData {
//
//  override def assetType: AssetType = AssetType.Any
//  def cacheKey = status.key.fold2(id.str, _.remoteId.fold(_.str, _.str))
//  val messageId = MessageId(id.str)
//  val uid = Uid(id.str)
//
//  def updated(asset: AnyAssetData): AnyAssetData = {
//    val mime = if (mimeType.isEmpty) asset.mimeType else mimeType
//    val st = if (asset.status == AssetStatus.UploadInProgress || lastUpdate.isAfter(asset.lastUpdate)) status else asset.status
//    val size = math.max(sizeInBytes, asset.sizeInBytes)
//    AnyAssetData(id, convId, mime, size, name.orElse(asset.name), metaData.orElse(asset.metaData),
//      preview.orElse(asset.preview), source orElse asset.source, asset.originalMimeType, st, lastUpdate max asset.lastUpdate)
//  }
//
//  def loadRequest = status match {
//    case AssetStatus(_, Some(key)) =>
//      AnyAssetRequest(cacheKey, id, convId, key, mimeType, name)
//    case _ =>
//      CachedAssetRequest(cacheKey, mimeType, name)
//  }
//
//  def withDownloadFailed(): AnyAssetData = copy(status = status.key.fold(status)(DownloadFailed))
//
//  def clearDownloadState(): AnyAssetData = copy(status = status.key.fold(status)(UploadDone))
//}
//
//object AnyAssetData {
//  val Empty = AnyAssetData(AssetId("empty"), RConvId(), Mime.Default, 0, None, None, None, None, None, AssetStatus.UploadNotStarted, Instant.EPOCH)
//
//  def apply(id: AssetId, convId: RConvId, mime: Mime, size: Long, name: Option[String], source: Uri, time: Instant = Instant.EPOCH, originalMimeType: Option[Mime] = None, metaData: Option[AssetMetaData] = None): AnyAssetData =
//    new AnyAssetData(id, convId, mime, size, name, metaData, None, Some(source), originalMimeType, AssetStatus.UploadNotStarted, time)
//
//  def apply(id: AssetId, convId: RConvId, asset: Proto.Asset, dataId: Option[RAssetDataId], time: Instant): AnyAssetData = {
//    val (mime, size, name, loudness) = Option(asset.original) match {
//      case Some(Proto.Asset.Original(m, s, n, _, p)) => (m, s, n, p)
//      case _ => (Mime.Unknown, 0L, None, None)
//    }
//    val preview = loudness.orElse(Option(asset.preview) map { p => AssetPreviewData.Image(ImageData(p, dataId)) })
//    val status = Proto.Asset.Status(asset, dataId)
//    val metaData = Proto.Asset.MetaData(asset)
//    AnyAssetData(id, convId, mime, size, name, metaData, preview, None, None, status, time)
//  }
//
//  import JsonDecoder._
//
//  implicit lazy val AnyAssetDataDecoder: JsonDecoder[AnyAssetData] = new JsonDecoder[AnyAssetData] {
//    override def apply(implicit js: JSONObject): AnyAssetData = AnyAssetData(decodeId[AssetId]('id),
//      decodeId[RConvId]('convId), Mime('mimeType), 'sizeInBytes, 'name, opt[AssetMetaData]('metaData),
//      opt[AssetPreviewData]('preview), decodeOptString('source).map(Uri.parse), decodeOptString('originalMimeType).map(Mime(_)),
//      JsonDecoder[AssetStatus]('status), 'lastUpdate)
//  }
//
//  implicit lazy val AnyAssetDataEncoder: JsonEncoder[AnyAssetData] = new JsonEncoder[AnyAssetData] {
//    override def apply(data: AnyAssetData): JSONObject = JsonEncoder { o =>
//      o.put("id", data.id.str)
//      o.put("convId", data.convId.str)
//      o.put("mimeType", data.mimeType.str)
//      o.put("sizeInBytes", data.sizeInBytes)
//      data.name.foreach(n => o.put("name", n))
//      data.metaData.foreach(md => o.put("metaData", JsonEncoder.encode(md)))
//      data.preview.foreach(p => o.put("preview", JsonEncoder.encode(p)))
//      data.source.foreach(u => o.put("source", u.toString))
//      data.originalMimeType.foreach(m => o.put("originalMimeType", m.str))
//      o.put("status", JsonEncoder.encode(data.status))
//      o.put("lastUpdate", JsonEncoder.encodeInstant(data.lastUpdate))
//    }
//  }
//}

//object InputAssetData {
//  def unapply(a: AssetData): Option[Uri] = a match {
//    case a: AnyAssetData => a.source
//    case _ => None
//  }
//}
