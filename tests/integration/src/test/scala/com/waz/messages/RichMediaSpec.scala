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
package com.waz.messages

import java.util

import android.graphics.Bitmap
import android.net.Uri
import com.waz.api.ImageAsset.BitmapCallback
import com.waz.api.MediaAsset.StreamingCallback
import com.waz.api.MessageContent.Text
import com.waz.api._
import com.waz.service.RemoteZmsSpec
import com.waz.testutils.Implicits._
import com.waz.utils.returning
import org.scalatest.{BeforeAndAfter, EitherValues, FeatureSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class RichMediaSpec extends FeatureSpec with Matchers with EitherValues with BeforeAndAfter with ProvisionedApiSpec with RemoteZmsSpec { test =>
  implicit val timeout: Timeout = 10.seconds
  import com.waz.threading.Threading.Implicits.Ui

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = {
    withDelay { conversations.size should be > 0 }
    conversations.find(_.getId == auto2Id.str).get
  }
  var lastKnock = None: Option[Message]

  lazy val msgs = conv.getMessages

  lazy val auto2 = createRemoteZms()
  lazy val auto2Id = provisionedUserId("auto2")

  scenario("init clients") {
    awaitUiFuture(auto2.login(provisionedEmail("auto2"), "auto2_pass"))
  }

  feature("Maps") {

    scenario("Receive message with google maps link") {
      val link = "https://www.google.com.au/maps/preview/@12.0,-0.12345,13%2Bz"

      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text(link))))

      lazy val msg = returning(msgs.getLastMessage) { m => m.getBody shouldEqual link }
      lazy val img = returning(msg.getParts.head.getImage) { _ should not be empty }

      withDelay {
        msgs should not be empty
        msg.getBody shouldEqual link
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts should not be empty
        msg.getParts.head.getPartType shouldEqual Message.Part.Type.GOOGLE_MAPS
        
        img should not be empty
      }

      info(s"got image: ${img.data}")
      val bitmap = new BitmapSpy(img)

      withDelay {
        bitmap.result shouldBe 'defined
      }
    }
  }

  feature("Youtube") {

    scenario("Receive message with youtube link") {
      val link = "https://www.youtube.com/watch?v=mTWfqi3-3qU"
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text(link))))

      lazy val msg = returning(msgs.getLastMessage) { m => m.getBody shouldEqual link }
      lazy val img = returning(msg.getParts.head.getMediaAsset.getArtwork) { _ should not be empty }

      withDelay {
        msgs should not be empty
        msg.getBody shouldEqual "https://www.youtube.com/watch?v=mTWfqi3-3qU"
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts should not be empty
        msg.getParts.head.getPartType shouldEqual Message.Part.Type.YOUTUBE

        img should not be empty
      }

      info(s"got image: ${img.data}")
      val bitmap = new BitmapSpy(img)

      withDelay {
        bitmap.result shouldBe 'defined
      }
    }
  }

  feature("SoundCloud") {

    scenario("Receive message with soundcloud link") {
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text("https://soundcloud.com/majorlazer/major-lazer-dj-snake-lean-on-feat-mo"))))

      var media: MediaAsset = null

      withDelay {
        msgs should not be empty
        val msg = msgs.getLastMessage
        msg.getBody shouldEqual "https://soundcloud.com/majorlazer/major-lazer-dj-snake-lean-on-feat-mo"
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts should not be empty

        val part = msg.getParts.head
        withClue(msg.getParts.map { p => (p.getPartType, p.getBody) }.mkString(", ")) {
          part.getPartType shouldEqual Message.Part.Type.SOUNDCLOUD
        }


        media = part.getMediaAsset
        media.isEmpty shouldEqual false

        val img = media.getArtwork
        img should not be null
        img should not be empty
      }

      var uris = Option.empty[Seq[Uri]]

      media.prepareStreaming(new StreamingCallback {
        override def onFailure(code: Int, message: String, label: String): Unit = {
          uris = Some(Nil)
          fail(s"$code, $message, $label")
        }
        override def onSuccess(us: util.List[Uri]): Unit = uris = Some(us.asScala)
      })

      withDelay {
        uris shouldBe 'defined
        uris.get should not be empty
      }
    }
  }

  feature("Spotify") {

    scenario("Receive message with spotify link") {
      awaitUiFuture(auto2.findConv(conv.data.remoteId).map(_.sendMessage(new Text("https://open.spotify.com/track/0sUyqewVzwv0e5tK3hS6vJ"))))

      var media: MediaAsset = null

      withDelay {
        msgs should not be empty
        val msg = msgs.getLastMessage
        msg.getBody shouldEqual "https://open.spotify.com/track/0sUyqewVzwv0e5tK3hS6vJ"
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts should not be empty

        val part = msg.getParts.head
        withClue(msg.getParts.map { p => (p.getPartType, p.getBody) }.mkString(", ")) {
          part.getPartType shouldEqual Message.Part.Type.SPOTIFY
        }

        media = part.getMediaAsset
        media.isEmpty shouldEqual false

        val img = media.getArtwork
        img should not be null
        img should not be empty
      }

      var uris = Option.empty[Seq[Uri]]

      media.prepareStreaming(new StreamingCallback {
        override def onFailure(code: Int, message: String, label: String): Unit = {
          uris = Some(Nil)
          fail(s"$code, $message, $label")
        }
        override def onSuccess(us: util.List[Uri]): Unit = uris = Some(us.asScala)
      })

      withDelay {
        uris shouldBe 'defined
        uris.get should not be empty
      }
    }
  }
}

class BitmapSpy(img: ImageAsset, size: Int = 600) {
  var failed = false
  var preview = Option.empty[Bitmap]
  var result = Option.empty[Bitmap]

  private var handle: LoadHandle = _

  load()

  img.addUpdateListener(new UpdateListener {
    override def updated(): Unit = load()
  })

  private def load() = {
    handle = img.getBitmap(size, new BitmapCallback {
      override def onBitmapLoadingFailed(): Unit = failed = true
      override def onBitmapLoaded(b: Bitmap, isPreview: Boolean): Unit = {
        if (isPreview) preview = Option(b)
        else result = Option(b)
      }
    })
  }
}
