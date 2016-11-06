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
import com.waz.service.downloads.DownloadRequest.{CachedAssetRequest, WireAssetRequest}
import com.waz.utils.JsonDecoder.{apply => _, opt => _}
import com.waz.utils._
import org.json.JSONObject

case class AssetData(id:          AssetId               = AssetId(), //TODO make independent of message id - will now be cache key
                     status:      AssetStatus           = AssetStatus.UploadNotStarted,
                     mime:        Mime                  = Mime.Unknown,
                     sizeInBytes: Long                  = 0L, //will be for metadata only??
                     name:        Option[String]        = None,
                     previewId:   Option[AssetId]       = None,
                     metaData:    Option[AssetMetaData] = None,
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

  lazy val remoteId = assetKey.flatMap(_.remoteId)

  def downloadFailed() = copy(status = status.key.fold(status)(DownloadFailed))

  //When download is finished, return to upload complete
  def downloadDone() = copy(status = status.key.fold(status)(UploadDone))

  def loadRequest = status match {
    case AssetStatus(_, Some(key)) => WireAssetRequest(id, key, convId, mime, name)
    case _ => CachedAssetRequest(id, mime, name)
  }

  val (isImage, isVideo, isAudio) = this match {
    case IsImage(_, _) => (true, false, false)
    case IsVideo()     => (false, true, false)
    case IsAudio()     => (false, false, true)
    case _             => (false, false, false)
  }

  val tag = this match {
    case IsImage(_, t) => t
    case _ => ""
  }

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

  object IsVideo {
    def unapply(asset: AssetData): Boolean = {
      asset match {
        case AssetData(_, _, _, _, _, _, Some(AssetMetaData.Video(_, _)), _, _, _, _) => true
        case _ => false
      }
    }
  }

  object IsAudio {
    def unapply(asset: AssetData): Boolean = {
      asset match {
        case AssetData(_, _, _, _, _, _, Some(AssetMetaData.Audio(_, _)), _, _, _, _) => true
        case _ => false
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

  object HasMetaData {
    def unapply(asset: AssetData): Option[AssetMetaData] = {
      asset match {
        case AssetData(_, _, _, _, _, _, metaData, _, _, _, _) => metaData
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

  object Status {
    def unapply(asset: AssetData): Option[AssetStatus] = {
      asset match {
        case AssetData(_, status, _, _, _, _, _, _, _, _, _) => Some(status)
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

case class AssetKey(remoteId: Option[RAssetId]    = None, //v2 assets don't need
                    token:    Option[AssetToken]  = None, //public assets don't need
                    otrKey:   Option[AESKey]      = None, //public assets don't need
                    sha256:   Option[Sha256]      = None)

object AssetKey extends ((Option[RAssetId], Option[AssetToken], Option[AESKey], Option[Sha256]) => AssetKey) {
  import JsonDecoder._

  implicit lazy val AssetKeyDecoder: JsonDecoder[AssetKey] = new JsonDecoder[AssetKey] {
    override def apply(implicit js: JSONObject): AssetKey = AssetKey(
      //TODO figure out where this gets saved and see if we can remove the if-else
      if (js.has("remoteId")) Some(decodeRAssetDataId('remoteId)) else Some(decodeRAssetDataId('remoteKey)),
      decodeOptString('token).map(AssetToken(_)),
      decodeOptString('otrKey).map(AESKey(_)),
      decodeOptString('sha256).map(Sha256(_))
    )
  }

  implicit lazy val AssetKeyEncoder: JsonEncoder[AssetKey] = new JsonEncoder[AssetKey] {
    override def apply(data: AssetKey): JSONObject = JsonEncoder { o =>
      data.remoteId foreach(v => o.put("remoteId", v.str))
      data.token foreach(v => o.put("token", v.str))
      data.otrKey foreach(v => o.put("otrKey", v.str))
      data.sha256 foreach(v => o.put("sha256", v.str))
    }
  }
}

