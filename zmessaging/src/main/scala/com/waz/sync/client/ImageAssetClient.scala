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

import java.io.{ByteArrayInputStream, InputStream}

import android.net.Uri
import android.util.Base64
import com.koushikdutta.async.http.body.{Part, StreamPart, StringPart}
import com.waz.ZLog._
import com.waz.cache.{CacheEntry, CacheService, Expiration, LocalData}
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.sync.client.OtrClient._
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.{IoUtils, JsonEncoder}
import com.waz.znet.ContentEncoder._
import com.waz.znet.Request.ProgressCallback
import com.waz.znet.Response.{Headers, HttpStatus, Status, SuccessHttpStatus}
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet.{BinaryResponse, FileResponse, JsonObjectResponse, _}
import com.wire.messages.nano.Otr
import org.json.JSONObject

import scala.concurrent.duration._
import scala.util.control.NonFatal

class ImageAssetClient(netClient: ZNetClient, cache: CacheService) {
  import Threading.Implicits.Background
  import com.waz.sync.client.ImageAssetClient._

  def loadImageAsset(req: Request[Unit], cb: ProgressCallback): CancellableFuture[Option[CacheEntry]] = {
    val cacheKey = req.resourcePath.getOrElse(req.absoluteUri.fold(Uid().str)(_.toString))
    netClient(req.copy(downloadCallback = Some(cb))) flatMap {
      case Response(SuccessHttpStatus(), BinaryResponse(data, mime), _) => cache.addStream(cacheKey, new ByteArrayInputStream(data)).map(Some(_))
      case Response(SuccessHttpStatus(), FileResponse(file, mime), _) => cache.addFile(cacheKey, file, moveFile = true).map(Some(_))
      case resp =>
        if (resp.status ne Response.Cancelled) warn(s"Unexpected response to loadImageAsset query: $resp")
        CancellableFuture.successful(None)
    }
  }

  def postImageAssetData(image: ImageData, assetId: AssetId, convId: RConvId, data: LocalData, nativePush: Boolean = true): ErrorOrResponse[AssetAddEvent] = {
    val content = new MultipartRequestContent(Seq(new JsonPart(imageMetadata(image, assetId, nativePush)), new ImageDataPart(data, image.mime)), "multipart/mixed")

    netClient.withErrorHandling("postImageAssetData", Request.Post(postAssetPath(convId), content)) {
      case Response(SuccessHttpStatus(), PostImageDataResponse(asset), _) =>
        debug(s"postImageAssetData completed with resp: $asset")
        asset
    }
  }

  def postOtrAsset(convId: RConvId, metadata: OtrAssetMetadata, data: LocalData, ignoreMissing: Boolean): ErrorOrResponse[OtrAssetResponse] = {
    val meta = OtrAssetMetadata.OtrMetaEncoder(metadata).asInstanceOf[ByteArrayRequestContent]
    val content = new MultipartRequestContent(Seq(new LocalDataPart(LocalData(meta.data), meta.contentType), new ImageDataPart(data, "application/octet-stream")), "multipart/mixed")

    verbose(s"postOtrAsset($metadata, data: $data)")
    netClient.withErrorHandling("postOtrAsset", Request.Post(postOtrAssetPath(convId, ignoreMissing), content))(otrAssetResponseHandler(h => RImageDataId(h("Location").getOrElse(""))))
  }

  def postOtrAssetMetadata(dataId: RImageDataId, convId: RConvId, metadata: OtrAssetMetadata, ignoreMissing: Boolean): ErrorOrResponse[OtrAssetResponse] =
    netClient.withErrorHandling("postOtrAssetMetadata", Request.Post(postOtrAssetPath(convId, dataId, ignoreMissing), metadata))(otrAssetResponseHandler(_ => dataId))

  private def otrAssetResponseHandler(assetId: Headers => RImageDataId): PartialFunction[Response, OtrAssetResponse] = {
    case Response(SuccessHttpStatus(), ClientMismatchResponse(mismatch), headers) => OtrAssetResponse(assetId(headers), MessageResponse.Success(mismatch))
    case Response(HttpStatus(Status.PreconditionFailed, _), ClientMismatchResponse(mismatch), headers) => OtrAssetResponse(assetId(headers), MessageResponse.Failure(mismatch))
  }
}

object ImageAssetClient {
  private implicit val logTag: LogTag = logTagFor[ImageAssetClient]
  implicit val DefaultExpiryTime: Expiration = 1.hour

  def postAssetPath(conv: RConvId) = s"/conversations/$conv/assets"
  def postOtrAssetPath(conv: RConvId, ignoreMissing: Boolean) =
    if (ignoreMissing) s"/conversations/$conv/otr/assets?ignore_missing=true"
    else s"/conversations/$conv/otr/assets"

  def postOtrAssetPath(conv: RConvId, asset: RImageDataId, ignoreMissing: Boolean) =
    if (ignoreMissing) s"/conversations/$conv/otr/assets/$asset?ignore_missing=true"
    else s"/conversations/$conv/otr/assets/$asset"

  def getAssetPath(conv: RConvId, asset: RImageDataId) = s"/conversations/$conv/assets/$asset"
  def getOtrAssetPath(conv: RConvId, asset: RImageDataId) = s"/conversations/$conv/otr/assets/$asset"

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

  case class OtrAssetResponse(assetId: RImageDataId, result: MessageResponse)

  object PostImageDataResponse {

    def unapply(response: ResponseContent): Option[AssetAddEvent] = try {
      response match {
        case JsonObjectResponse(js) =>
          Event.EventDecoder(js) match {
            case ev: AssetAddEvent => Some(ev)
            case ev =>
              warn(s"unexpected event received when waiting for image asset add: $ev")
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

class ImageDataPart(data: LocalData, contentType: String) extends LocalDataPart(data, contentType) {
  getRawHeaders.add("Content-MD5", ImageDataPart.md5(data))

  override def toString: LogTag = s"ImageDataPart($data, $contentType)"
}

object ImageDataPart {
  /**
   * Computes base64 encoded md5 sum of image data.
   */
  def md5(data: LocalData): String =
    Base64.encodeToString(IoUtils.md5(data.inputStream), Base64.NO_WRAP)
}
