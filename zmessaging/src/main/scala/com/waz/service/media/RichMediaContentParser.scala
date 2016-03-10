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
package com.waz.service.media

import java.net.URLDecoder

import android.net.Uri
import android.util.Patterns
import com.waz.ZLog._
import com.waz.api.Message.Part
import com.waz.model.MessageContent

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

class RichMediaContentParser {
  import Part.Type._
  import com.waz.service.media.RichMediaContentParser._

  private implicit val logTag: LogTag = logTagFor[RichMediaContentParser]

  def findMatches(content: String) = {

    val knownDomains = Map(
      "youtu.be" -> YOUTUBE,
      "youtube.com" -> YOUTUBE,
      "twitter.com" -> TWITTER,
      "soundcloud.com" -> SOUNDCLOUD,
      "open.spotify.com" -> SPOTIFY,
      "www.google.com.au" -> GOOGLE_MAPS // XXX only accept exactly this for now
    )

    def validate(uri: Uri, tpe: Part.Type): Boolean = tpe match {
      case YOUTUBE     => youtubeVideoId(uri).isDefined
      case SOUNDCLOUD  => Option(uri.getPath).exists(_.nonEmpty)
      case TWITTER     => uri.toString.matches(TwitterRegex.regex)
      case SPOTIFY     => SpotifyPathRegex.unapplySeq(uri.getPath).isDefined
      case GOOGLE_MAPS => GoogleMapsRegex.unapplySeq(uri.toString).isDefined
      case WEB_LINK    => WebLinkEnabled // accept any link here if weblink is enabled
      case _           => false
    }

    def matchDomain(host: String): Part.Type = knownDomains.getOrElse(host, {
      val dot = host.indexOf('.')
      if (dot >= 0) matchDomain(host.substring(dot + 1))
      else WEB_LINK
    })

    val m = Patterns.WEB_URL.matcher(content)
    Iterator.continually(m.find()).takeWhile(identity).map { _ =>
      val start = m.start
      val end = m.end
      if (start == 0 || content(start - 1) != '@') {
        val uri = Uri.parse(URLDecoder.decode(m.group(), "UTF-8"))
        Option(uri.getHost) map(_.toLowerCase) map matchDomain flatMap { tpe =>
          if (validate(uri, tpe)) Some((start, end, tpe))
          else None
        }
      } else None
    }.flatten
  }

  def splitContent(content: String) = {
    def prepend(str: String, acc: List[MessageContent]) = if (str.nonEmpty) MessageContent(TEXT, str) :: acc else acc

    try {
      findMatches(content).foldRight(Seq(MessageContent(TEXT, content))) {
        case ((start, end, tpe), MessageContent(TEXT, ct, _, _, _, _, _, _) :: tail) =>
          val prefix = ct.substring(0, start).trim
          val suffix = ct.substring(end).trim
          val link = MessageContent(tpe, ct.substring(start, end))

          prepend(prefix, link :: prepend(suffix, tail))
        case (_, ct) => ct // this should actually never happen
      }
    } catch {
      case e: Throwable =>
        error("got error while parsing message content", e)
        Seq(MessageContent(TEXT, content))
    }
  }

  def javaSplitContent(content: String) = splitContent(content).asJava
}

object RichMediaContentParser {
  case class GoogleMapsLocation(x: String, y: String, zoom: String)

  val WebLinkEnabled = false

  val SpotifyPathRegex = "(?i)/(artist|album|track|playlist)/[0-9A-Za-z-_]+/?".r
  val TwitterRegex = """(?i)(https?://)?(www\.)?twitter\.com/[0-9A-Za-z-_]+/status/\d*/?""".r
  val GoogleMapsRegex = """https://www\.google\.com\.au/maps/preview/@([0-9\.\-]+),([0-9\.\-]+),([0-9]+\+?)z""".r // XXX only accept exactly this for now

  def youtubeVideoId(youtubeUrl: String): Option[String] = decode(youtubeUrl)(youtubeVideoId)
  private def youtubeVideoId(uri: Uri): Option[String] = try {
    Option(uri.getQueryParameter("v")).orElse {
      Option(uri.getLastPathSegment)
    }.filter(_.length > 10) // currently id is always 11 chars, this may change in future
  } catch {
    case NonFatal(e) => None
  }

  def googleMapsLocation(mapsUrl: String): Option[GoogleMapsLocation] = decode(mapsUrl)(googleMapsLocation)
  private def googleMapsLocation(uri: Uri): Option[GoogleMapsLocation] = uri.toString match {
    case GoogleMapsRegex(x, y, zoom) => Some(GoogleMapsLocation(x, y, zoom))
    case _ => None
  }

  private def decode[T](url: String)(op: Uri => Option[T]): Option[T] = op(Uri.parse(URLDecoder.decode(url, "UTF-8")))
}
