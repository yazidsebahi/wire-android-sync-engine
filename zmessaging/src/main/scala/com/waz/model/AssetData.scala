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
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.verbose
import com.waz.db.Col._
import com.waz.db.Dao
import com.waz.model.AssetStatus.UploadDone
import com.waz.service.downloads.DownloadRequest._
import com.waz.utils.JsonDecoder.{apply => _, opt => _}
import com.waz.utils._
import org.json.JSONObject
import org.threeten.bp.Duration
import com.waz.ZLog.ImplicitTag._

//Things still borked:
//TODO sending from camera doesn't work
//TODO gallery images are blocking adapter
//TODO audio messages don't play
//TODO video messages don't render
//TODO profile pictures - get back to normal
//TODO Giphy loading
//TODO Test souncloud/spotify/youtube/linkpreviews/locations
//TODO send in v2 flag?

case class AssetData(id:          AssetId               = AssetId(),
                     mime:        Mime                  = Mime.Unknown,
                     sizeInBytes: Long                  = 0L, //will be for metadata only??
                     status:      AssetStatus           = AssetStatus.UploadNotStarted,
                     remoteId:    Option[RAssetId]      = None,
                     token:       Option[AssetToken]    = None,
                     otrKey:      Option[AESKey]        = None,
                     sha:         Option[Sha256]        = None,
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

  //TODO Dean - otrKey and sha are printing as AESKey(AESKey(kdjfl)) - is this going to corrupt data?
  override def toString: String =
    s"""
       |AssetData:
       | id:            $id
       | mime:          $mime
       | sizeInBytes:   $sizeInBytes
       | status:        $status
       | rId:           $remoteId
       | token:         $token
       | otrKey:        $otrKey
       | sha:           $sha
       | preview:       $previewId
       | metaData:      $metaData
       | convId:        $convId
       | other fields:  $name, $source, $proxyPath, $data64
    """.stripMargin

  lazy val data = data64.flatMap(data => LoggedTry(Base64.decode(data, Base64.DEFAULT)).toOption)

  lazy val size = data.fold(sizeInBytes)(_.length)

  lazy val fileExtension = mime.extension

  lazy val remoteData = (remoteId, token, otrKey, sha) match {
    case (None, None, None, None) => Option.empty[RemoteData]
    case _ => Some(RemoteData(remoteId, token, otrKey, sha))
  }

  def loadRequest = {
    val req = (remoteData, source, proxyPath) match {
      case (Some(rData), _, _)                                                        => WireAssetRequest(id, rData, convId, mime)
      case (_, Some(uri), _) if Option(uri.getScheme).forall(! _.startsWith("http"))  => External(id, uri)
      case (_, Some(uri), _)                                                          => LocalAssetRequest(id, uri, mime, name)
      case (_, None, Some(path))                                                      => Proxied(id, path)
      case _                                                                          => CachedAssetRequest(id, mime, name)
    }
    verbose(s"loadRequest returning: $req")
    req
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

  def copyWithRemoteData(remoteData: RemoteData) = {
    copy(
      remoteId  = remoteData.remoteId,
      token     = remoteData.token,
      otrKey    = remoteData.otrKey,
      sha       = remoteData.sha256
    )
  }

  //useful for receiving parts of an asset message. Note, THIS keeps the ID and only merges non-defined properties
  def merge(that: AssetData): AssetData = {
    val res = this.copy(
      mime        = if (this.mime == Mime.Unknown)  that.mime         else mime,
      sizeInBytes = if (this.sizeInBytes == 0)      that.sizeInBytes  else sizeInBytes,
      remoteId    = if (this.remoteId.isEmpty)      that.remoteId     else remoteId,
      token       = if (this.token.isEmpty)         that.token        else token,
      otrKey      = if (this.otrKey.isEmpty)        that.otrKey       else otrKey,
      sha         = if (this.sha.isEmpty)           that.sha          else sha,
      name        = if (this.name.isEmpty)          that.name         else name,
      previewId   = if (this.previewId.isEmpty)     that.previewId    else previewId,
      metaData    = if (this.metaData.isEmpty)      that.metaData     else metaData,
      proxyPath   = if (this.proxyPath.isEmpty)     that.proxyPath    else proxyPath,
      source      = if (this.source.isEmpty)        that.source       else source,
      convId      = if (this.convId.isEmpty)        that.convId       else convId,
      data64      = if (this.data64.isEmpty)        that.data64       else data64
      //TODO giphy source and caption
    )
    //After merging the two asset data objects, update the resulting status if we now have remote data
    res.copy(status = res.remoteData.fold(res.status)(_ => UploadDone))
  }

}

object AssetData {

  //simplify handling remote data from asset data
  case class RemoteData(remoteId: Option[RAssetId]    = None,
                        token:    Option[AssetToken]  = None,
                        otrKey:   Option[AESKey]      = None,
                        sha256:   Option[Sha256]      = None
                       )

  //needs to be def to create new id each time
  def NewImageAsset(id: AssetId = AssetId()) = AssetData(id = id, metaData = Some(AssetMetaData.Image(Dim2(0, 0))))

  object IsImage {
    def unapply(asset: AssetData): Option[(Dim2, String)] = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(AssetMetaData.Image(dims, tag)), _, _, _, _) => Some((dims, tag))
        case _ => None
      }
    }
  }

  object IsVideo {
    def unapply(asset: AssetData): Boolean = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(AssetMetaData.Video(_, _)), _, _, _, _) => true
        case _ => false
      }
    }
  }

  object IsAudio {
    def unapply(asset: AssetData): Boolean = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(AssetMetaData.Audio(_, _)), _, _, _, _) => true
        case _ => false
      }
    }
  }

  object WithData {
    def unapply(asset: AssetData): Option[Array[Byte]] = asset.data
  }

  object WithMetaData {
    def unapply(asset: AssetData): Option[AssetMetaData] = asset.metaData
  }

  object WithDimensions {
    def unapply(asset: AssetData): Option[Dim2] = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(AssetMetaData.HasDimensions(dimensions)), _, _, _, _) => Some(dimensions)
        case _ => None
      }
    }
  }

  object WithDuration {
    def unapply(asset: AssetData): Option[Duration] = {
      asset match {
        case AssetData(_, _, _, _, _, _, _, _, _, _, Some(AssetMetaData.HasDuration(duration)), _, _, _, _) => Some(duration)
        case _ => None
      }
    }
  }

  object WithPreview {
    def unapply(asset: AssetData): Option[AssetId] = asset.previewId
  }

  object WithRemoteId {
    def unapply(asset: AssetData): Option[RAssetId] = asset.remoteId
  }

  object WithStatus {
    def unapply(asset: AssetData): Option[AssetStatus] = Some(asset.status)
  }

  val MaxAllowedAssetSizeInBytes = 26214383L
  // 25MiB - 32 + 15 (first 16 bytes are AES IV, last 1 (!) to 16 bytes are padding)
  val MaxAllowedBackendAssetSizeInBytes = 26214400L

  // 25MiB

  case class FetchKey(id: AssetId)

  case class UploadKey(id: AssetId)

  implicit object AssetDataDao extends Dao[AssetData, AssetId] {
    val Id    = id[AssetId]('_id, "PRIMARY KEY").apply(_.id)
    //TODO remove in migration
    val Asset = text('asset_type, "").apply(_ => "")
    val Data = text('data)(JsonEncoder.encodeString(_))

    override val idCol = Id
    override val table = Table("Assets", Id, Asset, Data)

    override def apply(implicit cursor: Cursor): AssetData = JsonDecoder.decode(Data)(AssetDataDecoder)
  }

  implicit lazy val AssetDataDecoder: JsonDecoder[AssetData] = new JsonDecoder[AssetData] {

    import JsonDecoder._

    override def apply(implicit js: JSONObject): AssetData =
      AssetData(
        'id,
        Mime('mime),
        'sizeInBytes,
        JsonDecoder[AssetStatus]('status),
        if (js.has("remoteId")) Some(decodeRAssetDataId('remoteId)) else Some(decodeRAssetDataId('remoteKey)),
        decodeOptString('token).map(AssetToken(_)),
        decodeOptString('otrKey).map(AESKey(_)),
        decodeOptString('sha256).map(Sha256(_)),
        'name,
        'preview,
        opt[AssetMetaData]('metaData),
        decodeOptString('source).map(Uri.parse),
        'proxyPath,
        'convId,
        'data64
      )
  }

  implicit lazy val AssetDataEncoder: JsonEncoder[AssetData] = new JsonEncoder[AssetData] {
    override def apply(data: AssetData): JSONObject = JsonEncoder { o =>
      o.put("id",           data.id.str)
      o.put("mime",         data.mime.str)
      o.put("sizeInBytes",  data.sizeInBytes)
      o.put("status",       JsonEncoder.encode(data.status))
      data.remoteId   foreach (v => o.put("remoteId",   v.str))
      data.token      foreach (v => o.put("token",      v.str))
      data.otrKey     foreach (v => o.put("otrKey",     v.str))
      data.sha        foreach (v => o.put("sha256",     v.str))
      data.name       foreach (v => o.put("name",       v))
      data.previewId  foreach (v => o.put("preview",    v.str))
      data.metaData   foreach (v => o.put("metaData",   JsonEncoder.encode(v)))
      data.source     foreach (v => o.put("source",     v.toString))
      data.proxyPath  foreach (v => o.put("proxyPath",  v))
      data.convId     foreach (v => o.put("convId",     v.str))
      data.data64     foreach (v => o.put("data64",     v))
    }
  }
}

case class AssetToken(str: String) extends AnyVal

object AssetToken extends (String => AssetToken)

