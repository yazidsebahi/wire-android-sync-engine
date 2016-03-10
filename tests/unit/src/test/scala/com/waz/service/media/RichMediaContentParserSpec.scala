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

import com.waz.api.Message.Part.Type._
import com.waz.model.{MessageData, MessageContent}
import com.waz.service.media.RichMediaContentParser.GoogleMapsLocation
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{OptionValues, FeatureSpec, Matchers}

class RichMediaContentParserSpec extends FeatureSpec with Matchers with OptionValues with TableDrivenPropertyChecks {
  lazy val parser = new RichMediaContentParser

  feature("match links") {

    scenario("match correct youtube links") {
      val links = List(
        "http://youtube.com/watch?v=c0KYU2j0TM4",
        "http://www.youtube.com/watch?v=c0KYU2j0TM4",
        "http://www.youtube.com/watch?v=c0KYU2j0TM4#t=100",
        "https://www.youtube.com/watch?v=c0KYU2j0TM4",
        "https://es.youtube.com/watch?v=c0KYU2j0TM4",
        "http://youtube.com/watch?v=c0KYU2j0TM4&wadsworth=1",
        "http://youtube.com/watch?v=c0KYU2j0TM4&wadsworth=1#t=100",
        "http://m.youtube.com/watch%3Fv%3D84zY33QZO5o",
        "https://www.youtube.com/watch?v=HuvLUhuo52w&feature=youtu.be",
        "http://youtu.be/c0KYU2j0TM4#t=100"
      )

      links foreach { link =>
        parser.findMatches(link).toList shouldEqual List((0, link.length, YOUTUBE))
      }
    }

    scenario("don't match invalid youtube links") {
      val links = List(
        "http://m.youtube.com/watch?v=84zY33QZ&ved=0CB0QtwlwAA"
      )

      links foreach { link => parser.findMatches(link) should be(empty) }
    }

    scenario("match spotify link") {
      val link = "http://open.spotify.com/artist/5lsC3H1vh9YSRQckyGv0Up"
      parser.findMatches(link).toList shouldEqual List((0, link.length, SPOTIFY))
    }

    scenario("match soundcloud link") {
      val link = "https://soundcloud.com/majorlazer/major-lazer-dj-snake-lean-on-feat-mo"
      parser.findMatches(link).toList shouldEqual List((0, link.length, SOUNDCLOUD))
    }

    scenario("match weblinks") {
      if (RichMediaContentParser.WebLinkEnabled) {
        val link = "https://www.google.de/url?sa=t&source=web&rct=j&ei=s-EzVMzyEoLT7Qb7loC4CQ&url=http://m.youtube.com/watch%3Fv%3D84zY33QZO5o&ved=0CB0QtwIwAA&usg=AFQjCNEgZ6mQSXLbKY1HAhVOEiAwHtTIvA"
        parser.findMatches(link).toList shouldEqual List((0, link.length, WEB_LINK))
      }
    }

    scenario("match google maps links") {
      forAll(Table(
        ("URL", "Location"),
        ("https://www.google.com.au/maps/preview/@12.0,-0.12345,13%2Bz", Some(GoogleMapsLocation("12.0", "-0.12345", "13+"))),
        ("https://www.google.com.au/maps/preview/@5,.3,5z", Some(GoogleMapsLocation("5", ".3", "5"))),
        ("http://www.google.com.au/maps/preview/@12.0,-0.12345,13%2Bz", None)
      )) { (url: String, result: Option[GoogleMapsLocation]) =>
        val msg = MessageData.messageContent(url)
        if (result.isDefined) msg._1 shouldEqual com.waz.api.Message.Type.RICH_MEDIA
        val returned = parser.findMatches(url).toList
        returned.isEmpty shouldEqual result.isEmpty
        result foreach { _ => returned shouldEqual List((0, url.length, GOOGLE_MAPS)) }
        RichMediaContentParser.googleMapsLocation(url) shouldEqual result
      }
    }
  }

  feature("id parsing") {

    scenario("parse youtube id") {
      Map(
        "http://youtube.com/watch?v=c0KYU2j0TM4" -> "c0KYU2j0TM4",
        "http://www.youtube.com/watch?v=c0KYU2j0TM4" -> "c0KYU2j0TM4",
        "http://www.youtube.com/watch?v=c0KYU2j0TM4#t=100" -> "c0KYU2j0TM4",
        "https://www.youtube.com/watch?v=c0KYU2j0TM4" -> "c0KYU2j0TM4",
        "https://es.youtube.com/watch?v=c0KYU2j0TM4" -> "c0KYU2j0TM4",
        "http://youtube.com/watch?v=c0KYU2j0TM4&wadsworth=1" -> "c0KYU2j0TM4",
        "http://youtube.com/watch?wadsworth=1&v=c0KYU2j0TM4" -> "c0KYU2j0TM4",
        "http://youtube.com/watch?v=c0KYU2j0TM4&wadsworth=1#t=100" -> "c0KYU2j0TM4",
        "http://m.youtube.com/watch%3Fv%3D84zY33QZO5o" -> "84zY33QZO5o",
        "https://www.youtube.com/watch?v=HuvLUhuo52w&feature=youtu.be" -> "HuvLUhuo52w",
        "http://youtu.be/c0KYU2j0TM4#t=100" -> "c0KYU2j0TM4"
      ) foreach {
        case (url, id) => RichMediaContentParser.youtubeVideoId(url) shouldEqual Some(id)
      }
    }
  }

  feature("split content") {
    scenario("single youtube link") {
      parser.splitContent("https://www.youtube.com/watch?v=MWdG413nNkI") shouldEqual List(MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"))
    }

    scenario("text with youtube link") {
      parser.splitContent("Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI") shouldEqual List(MessageContent(TEXT, "Here is some text."), MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"))
    }

    scenario("don't split proper uri") {
      parser.splitContent("https://www.youtube.com/watch?v=HuvLUhuo52w&feature=youtu.be") shouldEqual List(MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=HuvLUhuo52w&feature=youtu.be"))
    }

    scenario("don't extract embeded url") {
      parser.splitContent("https://www.google.de/url?sa=t&source=web&rct=j&ei=s-EzVMzyEoLT7Qb7loC4CQ&url=http://m.youtube.com/watch%3Fv%3D84zY33QZO5o&ved=0CB0QtwIwAA&usg=AFQjCNEgZ6mQSXLbKY1HAhVOEiAwHtTIvA") shouldEqual List(MessageContent(if (RichMediaContentParser.WebLinkEnabled) WEB_LINK else TEXT, "https://www.google.de/url?sa=t&source=web&rct=j&ei=s-EzVMzyEoLT7Qb7loC4CQ&url=http://m.youtube.com/watch%3Fv%3D84zY33QZO5o&ved=0CB0QtwIwAA&usg=AFQjCNEgZ6mQSXLbKY1HAhVOEiAwHtTIvA"))
    }

    scenario("text interleaved with multiple youtube links") {
      parser.splitContent("Here is some text. https://www.youtube.com/watch?v=MWdG413nNkI more text https://www.youtube.com/watch?v=c0KYU2j0TM4 and even more") shouldEqual List(
        MessageContent(TEXT, "Here is some text."),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=MWdG413nNkI"),
        MessageContent(TEXT, "more text"),
        MessageContent(YOUTUBE, "https://www.youtube.com/watch?v=c0KYU2j0TM4"),
        MessageContent(TEXT, "and even more")
      )
    }
  }
}
