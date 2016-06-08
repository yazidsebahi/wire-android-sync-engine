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
package com.waz.sync.client

import java.io.InputStream

import android.util.Base64
import com.koushikdutta.async.http.body.{Part, StreamPart, StringPart}
import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.cache.{CacheEntry, Expiration, LocalData}
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.sync.client.OtrClient._
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.JsonDecoder.{apply => _, _}
import com.waz.utils.{IoUtils, JsonEncoder, LoggedTry}
import com.waz.znet.ContentEncoder._
import com.waz.znet.Response._
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet.{BinaryResponse, FileResponse, JsonObjectResponse, _}
import com.wire.messages.nano.Otr
import org.json.JSONObject

import scala.concurrent.duration._
import scala.util.control.NonFatal

class AssetClient(netClient: ZNetClient) {
  import Threading.Implicits.Background
  import com.waz.sync.client.AssetClient._

  def loadAsset(req: Request[Unit]): ErrorOrResponse[CacheEntry] =
    netClient.withErrorHandling("loadAsset", req) {
      case Response(SuccessHttpStatus(), resp: BinaryResponse, _) => resp
      case Response(SuccessHttpStatus(), resp: FileResponse, _) => resp
    } flatMap {
      case Right(FileResponse(file, _)) => CancellableFuture successful Right(file)
      case Right(resp) => CancellableFuture successful Left(ErrorResponse.internalError(s"unexpected response: $resp"))
      case Left(err) => CancellableFuture successful Left(err)
    }

  def postImageAssetData(image: ImageData, assetId: AssetId, convId: RConvId, data: LocalData, nativePush: Boolean = true): ErrorOrResponse[ImageData] = {
    val content = new MultipartRequestContent(Seq(new JsonPart(imageMetadata(image, assetId, nativePush)), new AssetDataPart(data, image.mime)), "multipart/mixed")

    netClient.withErrorHandling("postImageAssetData", Request.Post(postAssetPath(convId), content)) {
      case Response(SuccessHttpStatus(), PostImageDataResponse(asset), _) =>
        debug(s"postImageAssetData completed with resp: $asset")
        asset
    }
  }

  def postOtrAsset(convId: RConvId, metadata: OtrAssetMetadata, data: LocalData, ignoreMissing: Boolean): ErrorOrResponse[OtrAssetResponse] = {
    val meta = OtrAssetMetadata.OtrMetaEncoder(metadata).asInstanceOf[ByteArrayRequestContent]
    val content = new MultipartRequestContent(Seq(new LocalDataPart(LocalData(meta.data), meta.contentType), new AssetDataPart(data, "application/octet-stream")), "multipart/mixed")

    verbose(s"postOtrAsset($metadata, data: $data)")
    netClient.withErrorHandling("postOtrAsset", Request.Post(postOtrAssetPath(convId, ignoreMissing), content))(otrAssetResponseHandler(h => RAssetDataId(h("Location").getOrElse(""))))
  }

  def postOtrAssetMetadata(dataId: RAssetDataId, convId: RConvId, metadata: OtrAssetMetadata, ignoreMissing: Boolean): ErrorOrResponse[OtrAssetResponse] =
    netClient.withErrorHandling("postOtrAssetMetadata", Request.Post(postOtrAssetPath(convId, dataId, ignoreMissing), metadata))(otrAssetResponseHandler(_ => dataId))

  private def otrAssetResponseHandler(assetId: Headers => RAssetDataId): PartialFunction[Response, OtrAssetResponse] = {
    case Response(SuccessHttpStatus(), ClientMismatchResponse(mismatch), headers) => OtrAssetResponse(assetId(headers), MessageResponse.Success(mismatch))
    case Response(HttpStatus(Status.PreconditionFailed, _), ClientMismatchResponse(mismatch), headers) => OtrAssetResponse(assetId(headers), MessageResponse.Failure(mismatch))
  }
}

object AssetClient {
  private implicit val logTag: LogTag = logTagFor[AssetClient]
  implicit val DefaultExpiryTime: Expiration = 1.hour

  def postAssetPath(conv: RConvId) = s"/conversations/$conv/assets"
  def postOtrAssetPath(conv: RConvId, ignoreMissing: Boolean) =
    if (ignoreMissing) s"/conversations/$conv/otr/assets?ignore_missing=true"
    else s"/conversations/$conv/otr/assets"

  def postOtrAssetPath(conv: RConvId, asset: RAssetDataId, ignoreMissing: Boolean) =
    if (ignoreMissing) s"/conversations/$conv/otr/assets/$asset?ignore_missing=true"
    else s"/conversations/$conv/otr/assets/$asset"

  def getAssetPath(conv: RConvId, asset: RAssetDataId) = s"/conversations/$conv/assets/$asset"
  def getOtrAssetPath(conv: RConvId, asset: RAssetDataId) = s"/conversations/$conv/otr/assets/$asset"

  def imageMetadata(image: ImageData, assetId: AssetId, nativePush: Boolean) = JsonEncoder { o =>
    o.put("width", image.width)
    o.put("height", image.height)
    o.put("original_width", image.origWidth)
    o.put("original_height", image.origHeight)
    o.put("inline", image.data.fold(false)(_.length < 10000))
    o.put("public", true)
    o.put("tag", image.tag)
    o.put("correlation_id", assetId)
    o.put("nonce", assetId)
    o.put("native_push", nativePush)
  }

  case class OtrAssetMetadata(sender: ClientId, recipients: EncryptedContent, nativePush: Boolean = true, inline: Boolean = false)

  object OtrAssetMetadata {

    implicit lazy val OtrMetaEncoder: ContentEncoder[OtrAssetMetadata] = new ContentEncoder[OtrAssetMetadata] {
      override def apply(meta: OtrAssetMetadata): RequestContent = {
        val data = new Otr.OtrAssetMeta
        data.sender = OtrClient.clientId(meta.sender)
        data.recipients = meta.recipients.userEntries
        data.isInline = meta.inline
        data.nativePush = meta.nativePush

        ContentEncoder.protobuf(data)
      }
    }
  }

  case class OtrAssetResponse(assetId: RAssetDataId, result: MessageResponse)

  object PostImageDataResponse {
    def decodeAssetAddEvent(implicit js: JSONObject) = {
      val data = js.getJSONObject("data")
      val info = data.getJSONObject("info")
      val mime = decodeString('content_type)(data)

      def imageData(implicit js: JSONObject) =
        new ImageData(info.getString("tag"), mime, info.getInt("width"), info.getInt("height"),
          info.getInt("original_width"), info.getInt("original_height"), 'content_length, decodeOptId[RAssetDataId]('id), decodeOptString('data).filter(_.nonEmpty), sent = true)

      imageData(data)
    }

    def unapply(response: ResponseContent): Option[ImageData] = try {
      response match {
        case JsonObjectResponse(js) =>
          js.optString("type", "") match {
            case "conversation.asset-add" => LoggedTry(decodeAssetAddEvent(js)).toOption
            case _ =>
              warn(s"unexpected event received when waiting for image asset add: $js")
              None
          }
        case _ => None
      }
    } catch {
      case NonFatal(e) =>
        warn(s"couldn't parse image add event from response: $response", e)
        None
    }
  }
}

class JsonPart(json: JSONObject) extends StringPart("", json.toString) {
  setContentType("application/json")
  getRawHeaders.set("Content-Length", length().toString)
  getRawHeaders.remove(Part.CONTENT_DISPOSITION)

  override def toString: LogTag = s"JsonPart($json)"
}

class LocalDataPart(data: LocalData, contentType: String) extends StreamPart("", data.length, null) {
  setContentType(contentType)
  getRawHeaders.set("Content-Length", length().toString)
  getRawHeaders.remove(Part.CONTENT_DISPOSITION)

  override def getInputStream: InputStream = data.inputStream

  override def toString: LogTag = s"LocalDataPart($data, $contentType)"
}

class AssetDataPart(data: LocalData, contentType: String) extends LocalDataPart(data, contentType) {
  getRawHeaders.add("Content-MD5", AssetDataPart.md5(data))

  override def toString: LogTag = s"AssetDataPart($data, $contentType)"
}

object AssetDataPart {
  /**
   * Computes base64 encoded md5 sum of image data.
   */
  def md5(data: LocalData): String = Base64.encodeToString(IoUtils.md5(data.inputStream), Base64.NO_WRAP)
}
