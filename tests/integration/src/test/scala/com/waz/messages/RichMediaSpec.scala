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

import akka.pattern.ask
import android.graphics.Bitmap
import android.net.Uri
import com.waz.api.ImageAsset.BitmapCallback
import com.waz.api.MediaAsset.StreamingCallback
import com.waz.api.Message.Part
import com.waz.api.MessageContent.Text
import com.waz.api._
import com.waz.provision.ActorMessage.{AwaitSyncCompleted, Login, SendText, Successful}
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.utils.returning
import org.scalatest.{BeforeAndAfter, EitherValues, FeatureSpec, Matchers}

import scala.collection.JavaConverters._

class RichMediaSpec extends FeatureSpec with Matchers with EitherValues with BeforeAndAfter with ProvisionedApiSpec with ThreadActorSpec { test =>

  override val provisionFile = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = {
    withDelay { conversations should not be empty }
    conversations.find(_.getId == auto2Id.str).get
  }
  var lastKnock = None: Option[Message]

  lazy val msgs = conv.getMessages

  lazy val auto2 = registerDevice("auto2")
  lazy val auto2Id = provisionedUserId("auto2")

  scenario("init local") {
    withDelay {
      conversations should not be empty
      msgs should not be empty
      zmessaging.syncRequests.content.syncJobs.currentValue.map(_.isEmpty) shouldEqual Some(true)
    }
  }

  scenario("init clients") {
    auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    auto2 ? AwaitSyncCompleted should eventually(be(Successful))
  }

  feature("OpenGraph link") {

    scenario("Send message with single web link") {
      val count = msgs.size

      conv.sendMessage(new Text("http://www.wire.com"))

      withDelay {
        msgs should have size (count + 1)
        val lastMsg = msgs.getLastMessage
        lastMsg.getMessageStatus shouldEqual Message.Status.SENT
        lastMsg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        lastMsg.getParts should have size 1
        val part = lastMsg.getParts.head
        part.getPartType shouldEqual Message.Part.Type.WEB_LINK
        part.getTitle should not be empty
        part.getDescription should not be empty
        part.getImage should not be empty
      }

      val image = msgs.getLastMessage.getParts.head.getImage
      val imageSpy = new BitmapSpy(image)

      withDelay {
        imageSpy.failed shouldEqual false
        imageSpy.result shouldBe defined
      }
    }

    scenario("Send message with facebook link") {
      val count = msgs.size

      conv.sendMessage(new Text("Http://Facebook.com"))

      withDelay {
        msgs should have size (count + 1)
        val lastMsg = msgs.getLastMessage
        lastMsg.getMessageStatus shouldEqual Message.Status.SENT
        lastMsg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        lastMsg.getParts should have size 1
        val part = lastMsg.getParts.head
        part.getPartType shouldEqual Message.Part.Type.WEB_LINK
        part.getTitle should not be empty
        part.getImage should not be empty
      }

      val image = msgs.getLastMessage.getParts.head.getImage
      val imageSpy = new BitmapSpy(image)

      withDelay {
        imageSpy.failed shouldEqual false
        imageSpy.result shouldBe defined
      }
    }

    scenario("Send message with text and link") {
      val count = msgs.size

      conv.sendMessage(new Text("test http://www.github.com"))

      withDelay {
        msgs should have size (count + 1)
        val lastMsg = msgs.getLastMessage
        lastMsg.getMessageStatus shouldEqual Message.Status.SENT
        lastMsg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        lastMsg.getParts.map(_.getPartType).toSeq shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK)
      }
    }

    scenario("Receive message with link preview") {
      val count = msgs.size
      val link = "https://wire.com"

      auto2 ? SendText(conv.data.remoteId, link) should eventually(be(Successful))

      lazy val msg = returning(msgs.getLastMessage) { m =>
        m.getBody shouldEqual link
        m.getMessageType shouldEqual Message.Type.RICH_MEDIA
      }
      lazy val img = returning(msg.getParts.head.getImage) { _ should not be empty }

      withDelay {
        withClue(msgs.map(m => (m.getMessageType, m.getBody)).mkString(", ")) {
          msgs should have size (count + 1)
        }
        msg.getBody shouldEqual link
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts should not be empty
        msg.getParts.head.getPartType shouldEqual Message.Part.Type.WEB_LINK
        msg.getParts.head.getTitle should not be empty
        msg.getParts.head.getDescription should not be empty

        img should not be empty
      }

      val bitmap = new BitmapSpy(img)

      withDelay {
        bitmap.result shouldBe 'defined
      }
    }

    scenario("Receive message with multiple link previews") {
      val count = msgs.size
      val text = "Test message https://wire.com and then http://github.com"

      auto2 ? SendText(conv.data.remoteId, text) should eventually(be(Successful))

      lazy val msg = returning(msgs.getLastMessage) { m =>
        m.getBody shouldEqual text
        m.getMessageType shouldEqual Message.Type.RICH_MEDIA
      }

      withDelay {
        withClue(msgs.map(m => (m.getMessageType, m.getBody)).mkString(", ")) {
          msgs should have size (count + 1)
        }
        msg.getBody shouldEqual text
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts.map(_.getPartType).toSeq shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK, Part.Type.TEXT, Part.Type.WEB_LINK)
      }
    }

    scenario("Receive message with web and youtube link") {
      val count = msgs.size
      val text = "Test message https://wire.com https://www.youtube.com/watch?v=mTWfqi3-3qU"

      auto2 ? SendText(conv.data.remoteId, text) should eventually(be(Successful))

      lazy val msg = returning(msgs.getLastMessage) { m =>
        m.getBody shouldEqual text
        m.getMessageType shouldEqual Message.Type.RICH_MEDIA
      }

      withDelay {
        withClue(msgs.map(m => (m.getMessageType, m.getBody)).mkString(", ")) {
          msgs should have size (count + 1)
        }
        msg.getBody shouldEqual text
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts.map(_.getPartType).toSeq shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK, Part.Type.YOUTUBE)
      }
    }
  }

  feature("Youtube") {

    scenario("Receive message with youtube link") {
      val link = "https://www.youtube.com/watch?v=mTWfqi3-3qU"
      auto2 ? SendText(conv.data.remoteId, link) should eventually(be(Successful))

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
      auto2 ? SendText(conv.data.remoteId, "https://soundcloud.com/lescourgettesgivrees/le-bois-du-casier") should eventually(be(Successful))

      var media: MediaAsset = null

      withDelay {
        msgs should not be empty
        val msg = msgs.getLastMessage
        msg.getBody shouldEqual "https://soundcloud.com/lescourgettesgivrees/le-bois-du-casier"
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
      auto2 ? SendText(conv.data.remoteId, "https://open.spotify.com/track/0sUyqewVzwv0e5tK3hS6vJ") should eventually(be(Successful))

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
