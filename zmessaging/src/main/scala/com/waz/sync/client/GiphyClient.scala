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

import com.waz.ZLog._
import com.waz.model.ImageData
import com.waz.threading.CancellableFuture
import com.waz.utils.{LoggedTry, JsonDecoder}
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet._
import org.json.JSONObject

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal

class GiphyClient(netClient: ZNetClient) {
  import GiphyClient._
  import com.waz.threading.Threading.Implicits.Background

  def loadRandom(): CancellableFuture[Seq[ImageData]] =
    netClient(Request.Get(path = RandomGifPath)) map {
      case Response(SuccessHttpStatus(), RandomGiphyResponse(images), _) => images
      case resp =>
        warn(s"unexpected response for load random: $resp")
        Nil
    }

  def search(keyword: String, offset: Int = 0, limit: Int = 25): CancellableFuture[Seq[Seq[ImageData]]] =
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

  object RandomGiphyResponse extends GiphyResponse[Seq[ImageData]] {

    lazy val Decoder: JsonDecoder[Seq[ImageData]] = new JsonDecoder[Seq[ImageData]] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): Seq[ImageData] = {
        Seq(
          ImageData(ImageData.Tag.Medium, "image/gif", 'image_width, 'image_height, 'image_width, 'image_height, url = 'image_url),
          ImageData(ImageData.Tag.Medium, "image/gif", 'fixed_height_downsampled_width, 'fixed_height_downsampled_height, 'fixed_height_downsampled_width, 'fixed_height_downsampled_height, url = 'fixed_height_downsampled_url),
          ImageData(ImageData.Tag.Medium, "image/gif", 'fixed_width_downsampled_width, 'fixed_width_downsampled_height, 'fixed_width_downsampled_width, 'fixed_width_downsampled_height, url = 'fixed_width_downsampled_url),
          ImageData(ImageData.Tag.Preview, "image/gif", 'fixed_height_small_width, 'fixed_height_small_height, 'fixed_height_small_width, 'fixed_height_small_height, url = 'fixed_height_small_still_url),
          ImageData(ImageData.Tag.Preview, "image/gif", 'fixed_width_small_width, 'fixed_width_small_height, 'fixed_width_small_width, 'fixed_width_small_height, url = 'fixed_width_small_still_url)
        )
      }
    }

    override def decode(js: JSONObject): Option[Seq[ImageData]] = js.getJSONObject("meta").getInt("status") match {
      case 200 => Try(Decoder(js.getJSONObject("data"))).toOption
      case _ => None
    }
  }

  object SearchGiphyResponse extends GiphyResponse[Seq[Seq[ImageData]]] {

    object Decoder extends JsonDecoder[Option[ImageData]] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): Option[ImageData] = {
        val url: Option[String] = 'url
        url map (url => ImageData("", "image/gif", 'width, 'height, 'width, 'height, 'size, url = Some(url)))
      }
    }

    def parseImage(tag: String, data: JSONObject): Option[ImageData] = Decoder(data) map (_.copy(tag = tag))

    override def decode(js: JSONObject) = js.getJSONObject("meta").getInt("status") match {
      case 200 => LoggedTry {
        JsonDecoder.array(js.getJSONArray("data"), { (arr, index) =>
          val images = arr.getJSONObject(index).getJSONObject("images")
          images.keys().asInstanceOf[java.util.Iterator[String]].asScala.flatMap(size => parseImage(if (size.endsWith("_still")) ImageData.Tag.Preview else ImageData.Tag.Medium, images.getJSONObject(size))).toVector
        })
      } .toOption
      case _ => None
    }
  }
}
