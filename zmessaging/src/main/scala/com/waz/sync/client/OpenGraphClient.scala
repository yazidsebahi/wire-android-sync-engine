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

import android.net.Uri
import com.koushikdutta.async.ByteBufferList
import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.threading.CancellableFuture
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.Response.{ClientErrorStatus, HttpStatus, ResponseBodyDecoder, SuccessStatus}
import com.waz.znet.ResponseConsumer.{ConsumerState, EmptyResponseConsumer, StringConsumer}
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._
import org.json.JSONObject

class OpenGraphClient(netClient: ZNetClient) {
  import OpenGraphClient._
  import com.waz.threading.Threading.Implicits.Background

  def loadMetadata(uri: Uri): ErrorOrResponse[Option[OpenGraphData]] = {
    val req = Request[Unit](Request.HeadMethod, absoluteUri = Some(uri), decoder = Some(ResponseDecoder), requiresAuthentication = false, headers = Map(AsyncClient.UserAgentHeader -> DesktopUserAgent)) // using empty User-Agent to avoid getting mobile website version
    netClient(req) flatMap {
      case Response(SuccessStatus(), StringResponse(_), headers) => // this means that ResponseDecoder accepted the content type, we can proceed with GET
        netClient.withErrorHandling("loadOpenGraph", req.copy(httpMethod = Request.GetMethod)) {
          case Response(SuccessStatus(), OpenGraphDataResponse(data), _) => Some(data)
          case Response(SuccessStatus(), _, _) => None
        }
      case Response(SuccessStatus(), _, _) =>
        verbose(s"loadMetadata(), HEAD indicates unsupported content type for $uri")
        CancellableFuture successful Right(None)
      case resp @ Response(ClientErrorStatus(), _, _) =>
        verbose(s"loadMetadata(), HEAD request failed with client error for $uri")
        CancellableFuture successful Right(None)
      case resp @ Response(HttpStatus(code, msg), _, _) =>
        warn(s"loadMetadata(), unexpected response to HEAD: $resp")
        CancellableFuture successful Left(ErrorResponse(code, msg, "unexpected"))
    }
  }
}

object OpenGraphClient {
  private implicit val tag: LogTag = logTagFor[OpenGraphClient]
  val MaxHeaderLength = 8 * 1024 // maximum amount of data to load from website
  val DesktopUserAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

  case class OpenGraphData(title: String, description: String, image: Option[Uri], tpe: String, permanentUrl: Option[Uri])

  case class HtmlHeaderConsumer(len: Long) extends StringConsumer(math.min(len, MaxHeaderLength)) {
    def isDone = data.size() >= MaxHeaderLength || data.toString("utf8").toLowerCase.contains("""</head>""")

    override def consume(bb: ByteBufferList): ConsumerState = {
      super.consume(bb)

      if (isDone) ConsumerState.Done else ConsumerState.Default
    }
  }

  object ResponseDecoder extends ResponseBodyDecoder {
    override def apply(contentType: String, contentLength: Long) =
      if (contentType.toLowerCase().contains("text/html")) new HtmlHeaderConsumer(contentLength)
      else {
        verbose(s"dropping response with content type: $contentType, len: $contentLength")
        EmptyResponseConsumer
      }
  }

  object OpenGraphData extends ((String, String, Option[Uri], String, Option[Uri]) => OpenGraphData) {
    val Empty = OpenGraphData("", "", None, "", None)

    implicit object Decoder extends JsonDecoder[OpenGraphData] {
      import JsonDecoder._

      override def apply(implicit js: JSONObject): OpenGraphData = OpenGraphData('title, 'description, decodeOptString('image).map(Uri.parse), 'tpe, decodeOptString('url).map(Uri.parse))
    }

    implicit object Encoder extends JsonEncoder[OpenGraphData] {
      override def apply(v: OpenGraphData): JSONObject = JsonEncoder { o =>
        o.put("title", v.title)
        o.put("description", v.description)
        v.image foreach { uri => o.put("image", uri.toString) }
        v.permanentUrl foreach { uri => o.put("url", uri.toString) }
        o.put("tpe", v.tpe)
      }
    }
  }

  object OpenGraphDataResponse {
    val Title = "og:title"
    val Image = "og:image"
    val Type = "og:type"
    val Url = "og:url"
    val Description = "og:description"

    val MetaTag = """<\s*meta\s+[^>]+>""".r
    val Attribute = """(\w+)\s*=\s*"([^"]+)"""".r

    val TitlePattern = """<title[^>]*>(.*)</title>""".r

    val AcceptedTypes = Seq("", "article", "website") // will ignore other types for now

    def unapply(body: StringResponse): Option[OpenGraphData] = {

      def htmlTitle = TitlePattern.findFirstIn(body.value)

      val ogMeta = MetaTag.findAllIn(body.value) .flatMap { meta =>
        val attrs = Attribute.findAllMatchIn(meta) .map { m => m.group(1).toLowerCase -> m.group(2) } .toMap
        for {
          name <- attrs.get("property").orElse(attrs.get("name")) if name.toLowerCase.startsWith("og:")
          content <- attrs.get("content")
        } yield name.toLowerCase -> content
      } .toMap

      if ((ogMeta.contains(Title) || ogMeta.contains(Image)) && ogMeta.get(Type).forall { tpe => AcceptedTypes.contains(tpe.toLowerCase) }) {
        Some(OpenGraphData(ogMeta.get(Title).orElse(htmlTitle).getOrElse(""), ogMeta.getOrElse(Description, ""), ogMeta.get(Image).map(Uri.parse), ogMeta.getOrElse(Type, ""), ogMeta.get(Url).map(Uri.parse)))
      } else None
    }
  }
}
