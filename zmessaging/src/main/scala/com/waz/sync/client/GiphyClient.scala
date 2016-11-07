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

import java.net.URLEncoder

import android.net.Uri
import com.waz.ZLog._
import com.waz.model.{AssetData, AssetMetaData, Dim2, Mime}
import com.waz.threading.CancellableFuture
import com.waz.utils.{JsonDecoder, LoggedTry}
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet._
import org.json.JSONObject

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal

class GiphyClient(netClient: ZNetClient) {
  import GiphyClient._
  import com.waz.threading.Threading.Implicits.Background

  def loadRandom(): CancellableFuture[Seq[AssetData]] =
    netClient(Request.Get(path = RandomGifPath)) map {
      case Response(SuccessHttpStatus(), RandomGiphyResponse(images), _) => images
      case resp =>
        warn(s"unexpected response for load random: $resp")
        Nil
    }

  def loadTrending(offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[Seq[AssetData]]] =
    netClient(Request.Get(path = trendingPath(offset, limit))) map {
      case Response(SuccessHttpStatus(), SearchGiphyResponse(images), _) => images
      case resp =>
        warn(s"unexpected response for trending: $resp")
        Nil
    }


  def search(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[Seq[AssetData]]] =
    netClient(Request.Get(path = searchPath(keyword, offset, limit))) map {
      case Response(SuccessHttpStatus(), SearchGiphyResponse(images), _) => images
      case resp =>
        warn(s"unexpected response for search keyword '$keyword': $resp")
        Nil
    }
}

object GiphyClient {
  implicit val logTag: LogTag = logTagFor[GiphyClient]

  val BasePath = "/proxy/giphy/v1/gifs"

  val RandomGifPath = s"$BasePath/random"

  def searchPath(keyword: String, offset: Int, limit: Int) = s"$BasePath/search?q=${URLEncoder.encode(keyword, "utf8")}&offset=$offset&limit=$limit"

  def trendingPath(offset: Int, limit: Int) = s"$BasePath/trending?offset=$offset&limit=$limit"

  trait GiphyResponse[T] { self =>

    def decode(js: JSONObject): Option[T]

    def unapply(response: ResponseContent): Option[T] = try {
      response match {
        case JsonObjectResponse(js) => decode(js)
        case StringResponse(json) => Try(new JSONObject(json)).toOption.flatMap(decode)
        case _ => None
      }
    } catch {
      case NonFatal(e) =>
        warn(s"response: $response parsing failed", e)
        None
    }
  }

  object RandomGiphyResponse extends GiphyResponse[Seq[AssetData]] {

    lazy val Decoder: JsonDecoder[Seq[AssetData]] = new JsonDecoder[Seq[AssetData]] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): Seq[AssetData] = {
        Seq(
          ("medium",  'image_width,                     'image_height,                    'image_url),
          ("medium",  'fixed_height_downsampled_width,  'fixed_height_downsampled_height, 'fixed_height_downsampled_url),
          ("medium",  'fixed_width_downsampled_width,   'fixed_width_downsampled_height,  'fixed_width_downsampled_url),
          ("preview", 'fixed_height_small_width,        'fixed_height_small_height,       'fixed_height_small_still_url),
          ("preview", 'fixed_width_small_width,         'fixed_width_small_height,        'fixed_width_small_still_url)
        ).map {
          case (tag, w, h, url) =>
            AssetData(
              mime = Mime.Image.Gif,
              metaData = Some(AssetMetaData.Image(Dim2(decodeInt(w), decodeInt(h)), tag)),
              source = Some(decodeUri(url))
            )
        }
      }
    }

    override def decode(js: JSONObject): Option[Seq[AssetData]] = js.getJSONObject("meta").getInt("status") match {
      case 200 => Try(Decoder(js.getJSONObject("data"))).toOption
      case _ => None
    }
  }

  object SearchGiphyResponse extends GiphyResponse[Seq[Seq[AssetData]]] {

    object Decoder extends JsonDecoder[Option[AssetData]] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): Option[AssetData] = {
        val uri = decodeOptString('url).map(Uri.parse)
        uri map (uri => AssetData(mime = Mime.Image.Gif, metaData = Some(AssetMetaData.Image(Dim2('width, 'height))), sizeInBytes = 'size, source = Some(uri)))
      }
    }

    def parseImage(tag: String, data: JSONObject): Option[AssetData] = Decoder(data).map { a =>
      a.copy(metaData = Some(AssetMetaData.Image(a.dimensions, tag)))
    }

    override def decode(js: JSONObject) = js.getJSONObject("meta").getInt("status") match {
      case 200 => LoggedTry {
        JsonDecoder.array(js.getJSONArray("data"), { (arr, index) =>
          val images = arr.getJSONObject(index).getJSONObject("images")
          images.keys().asInstanceOf[java.util.Iterator[String]].asScala.flatMap(size => parseImage(if (size.endsWith("_still")) "preview" else "medium", images.getJSONObject(size))).toVector
        })
      } .toOption
      case _ => None
    }
  }
}
