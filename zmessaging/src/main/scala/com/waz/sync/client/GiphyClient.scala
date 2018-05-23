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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.model.AssetMetaData.Image.Tag
import com.waz.model.AssetMetaData.Image.Tag.{Medium, Preview}
import com.waz.model.{AssetData, AssetMetaData, Dim2, Mime}
import com.waz.service.BackendConfig
import com.waz.threading.CancellableFuture
import com.waz.utils.wrappers.URI
import com.waz.utils.{JsonDecoder, LoggedTry}
import com.waz.znet.{JsonObjectResponse, ResponseContent, StringResponse}
import com.waz.znet2.AuthRequestInterceptor
import com.waz.znet2.http.{HttpClient, RawBodyDeserializer, Request}
import org.json.JSONObject

import scala.util.Try
import scala.util.control.NonFatal

trait GiphyClient {
  def loadRandom(): CancellableFuture[(Option[AssetData], AssetData)]
  def loadTrending(offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[(Option[AssetData], AssetData)]]
  def search(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[(Option[AssetData], AssetData)]]
}

class GiphyClientImpl(implicit
                      private val backendConfig: BackendConfig,
                      private val httpClient: HttpClient,
                      private val authRequestInterceptor: AuthRequestInterceptor) extends GiphyClient {
  import BackendConfig.backendUrl
  import GiphyClient._
  import HttpClient.dsl._
  import com.waz.threading.Threading.Implicits.Background

  private implicit val giphyDeserializer: RawBodyDeserializer[(Option[AssetData], AssetData)] =
    RawBodyDeserializer[JSONObject].map(json => RandomGiphyResponse.unapply(JsonObjectResponse(json)).get)

  private implicit val giphySeqDeserializer: RawBodyDeserializer[Seq[(Option[AssetData], AssetData)]] =
    RawBodyDeserializer[JSONObject].map(json => SearchGiphyResponse.unapply(JsonObjectResponse(json)).get)

  override def loadRandom(): CancellableFuture[(Option[AssetData], AssetData)] = {
    val request = Request.withoutBody(url = backendUrl(RandomGifPath))
    Prepare(request)
      .withResultType[(Option[AssetData], AssetData)]
      .execute
      .recover { case err =>
        warn(s"unexpected response for load random: $err")
        (None, AssetData.Empty)
      }
  }

  override def loadTrending(offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[(Option[AssetData], AssetData)]] = {
    val request = Request.withoutBody(url = backendUrl(trendingPath(offset, limit)))
    Prepare(request)
      .withResultType[Seq[(Option[AssetData], AssetData)]]
      .execute
      .recover { case err =>
        warn(s"unexpected response for trending: $err")
        Nil
      }
  }

  override def search(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[(Option[AssetData], AssetData)]] = {
    val request = Request.withoutBody(url = backendUrl(searchPath(keyword, offset, limit)))
    Prepare(request)
      .withResultType[Seq[(Option[AssetData], AssetData)]]
      .execute
      .recover { case err =>
        warn(s"unexpected response for search keyword '$keyword': $err")
        Nil
      }
  }

}

object GiphyClient {
  val BasePath = "/proxy/giphy/v1/gifs"

  val RandomGifPath = s"$BasePath/random"

  def searchPath(keyword: String, offset: Int, limit: Int) = s"$BasePath/search?q=${URLEncoder.encode(keyword, "utf8")}&offset=$offset&limit=$limit"

  def trendingPath(offset: Int, limit: Int) = s"$BasePath/trending?offset=$offset&limit=$limit"

  implicit lazy val GiphyAssetOrdering: Ordering[AssetData] = new Ordering[AssetData] {
    override def compare(x: AssetData, y: AssetData): Int = {
      if (x.dimensions.width == y.dimensions.width) {
        if (x.tag == Preview || y.tag == Medium) -1
        else if (x.tag == Medium || y.tag == Preview) 1
        else Ordering.Int.compare(x.size.toInt, y.size.toInt)
      } else Ordering.Int.compare(x.dimensions.width, y.dimensions.width)
    }
  }

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

  object RandomGiphyResponse extends GiphyResponse[(Option[AssetData], AssetData)] {

    lazy val Decoder: JsonDecoder[(Option[AssetData], AssetData)] = new JsonDecoder[(Option[AssetData], AssetData)] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): (Option[AssetData], AssetData) = {
        val sorted = Seq(
          ("medium",  'image_width,                     'image_height,                    'image_url),
          ("medium",  'fixed_height_downsampled_width,  'fixed_height_downsampled_height, 'fixed_height_downsampled_url),
          ("medium",  'fixed_width_downsampled_width,   'fixed_width_downsampled_height,  'fixed_width_downsampled_url),
          ("preview", 'fixed_height_small_width,        'fixed_height_small_height,       'fixed_height_small_still_url),
          ("preview", 'fixed_width_small_width,         'fixed_width_small_height,        'fixed_width_small_still_url)
        ).map {
          case (tag, w, h, url) =>
            AssetData(
              mime = Mime.Image.Gif,
              metaData = Some(AssetMetaData.Image(Dim2(decodeInt(w), decodeInt(h)), Tag(tag))),
              source = Some(decodeUri(url))
            )
        }.sorted

        val preview = sorted.headOption
        val medium = sorted.lastOption.map(_.copy(previewId = preview.map(_.id))).getOrElse(AssetData.Empty)
        (preview, medium)
      }
    }

    override def decode(js: JSONObject): Option[(Option[AssetData], AssetData)] = js.getJSONObject("meta").getInt("status") match {
      case 200 => Try(Decoder(js.getJSONObject("data"))).toOption
      case _ => None
    }
  }

  object SearchGiphyResponse extends GiphyResponse[Seq[(Option[AssetData], AssetData)]] {

    object Decoder extends JsonDecoder[Option[AssetData]] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): Option[AssetData] = {
        val uri = decodeOptString('url).map(URI.parse)
        uri map (uri => AssetData(mime = Mime.Image.Gif, metaData = Some(AssetMetaData.Image(Dim2('width, 'height))), sizeInBytes = 'size, source = Some(uri)))
      }
    }

    def parseImage(tag: Tag, data: JSONObject): Option[AssetData] = Decoder(data).map { a =>
      a.copy(metaData = Some(AssetMetaData.Image(a.dimensions, tag)))
    }

    override def decode(js: JSONObject) = {
      js.getJSONObject("meta").getInt("status") match {
        case 200 => LoggedTry {
          JsonDecoder.array(js.getJSONArray("data"), { (arr, index) =>
            val images = arr.getJSONObject(index).getJSONObject("images")

            val assets = Seq("original_still", "original").flatMap { key =>
              parseImage(if (key.endsWith("_still")) Preview else Medium, images.getJSONObject(key))
            }

            val preview = assets.headOption
            (preview, assets.lastOption.map(_.copy(previewId = preview.map(_.id))).getOrElse(AssetData.Empty))
          })
        } .toOption
        case _ => None
      }
    }
  }
}
