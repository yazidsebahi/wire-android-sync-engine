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

import java.io.ByteArrayInputStream

import com.waz.api._
import com.waz.provision.ActorMessage.{ConvMessages, _}
import com.waz.utils._
import org.scalatest._
import com.waz.testutils._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest.concurrent.ScalaFutures
import akka.pattern.ask
import com.waz.api.Message.Part
import com.waz.api.impl.DoNothingAndProceed
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{AssetId, ImageData, MessageId, Mime}
import org.threeten.bp.Instant

import scala.util.Random
import scala.concurrent.duration._

class EphemeralMessageSpec extends FeatureSpec with BeforeAndAfter with Matchers with OptionValues
  with ProvisionedApiSpec with ThreadActorSpec with ScalaFutures with Inspectors with DefaultPatienceConfig {

  override val provisionFile: String = "/three_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = {
    withDelay { conversations should not be empty }
    conversations.find(_.getId == provisionedUserId("auto2").str).get
  }
  lazy val group = {
    withDelay { conversations should not be empty }
    conversations.find(_.getType == ConversationType.Group).get
  }
  lazy val messages = conv.getMessages
  lazy val grpMsgs = group.getMessages

  lazy val auto2 = registerDevice("auto2")
  lazy val auto3 = registerDevice("auto3")

  scenario("init") {
    soon {
      conversations should not be empty
      messages should have size 1
    }

    conv.setEphemeralExpiration(EphemeralExpiration.FIVE_SECONDS)
  }

  def withNewMessage(body: => Unit): Message = {
    val count = messages.size

    body

    soon {
      messages should have size (count + 1)
    }
    messages.getLastMessage
  }

  scenario("init remotes") {
    auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    auto3 ? Login(provisionedEmail("auto3"), "auto3_pass") should eventually(be(Successful))
    auto2 ? AwaitSyncCompleted should eventually(be(Successful))
    auto3 ? AwaitSyncCompleted should eventually(be(Successful))
    soon {
      val ConvMessages(ms) = (auto2 ? GetMessages(conv.data.remoteId)).await()
      ms should not be empty
    }
    soon {
      val ConvMessages(ms) = (auto2 ? GetMessages(conv.data.remoteId)).await()
      ms should not be empty
    }
  }

  feature("Sending") {

    var messageId = MessageId()

    def assertEphemeralSent(msg: Message) = {
      Set(Message.Status.SENT, Message.Status.DELIVERED) should contain (msg.getMessageStatus)
      msg.isEphemeral shouldEqual true
      msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
      val time = msg.getExpirationTime
      time should be > Instant.now
      time should be < (Instant.now + msg.getEphemeralExpiration.duration)
    }

    def sendMessage(content: MessageContent)(assert: Message => Unit)(implicit p: PatienceConfig) = {
      val msg = withNewMessage(conv.sendMessage(content))
      soon {
        assertEphemeralSent(msg)
        assert(msg)
      }
    }

    scenario("Send text message") {
      sendMessage(new MessageContent.Text("test msg 1")) { msg =>
        msg.getMessageType shouldEqual Message.Type.TEXT
        msg.getBody shouldEqual "test msg 1"
      }
    }

    scenario("Obfuscate text message when timer expires") {
      val msg = messages.getLastMessage
      msg.getBody shouldEqual "test msg 1"
      messageId = msg.data.id

      zmessaging.messagesStorage.update(msg.data.id, _.copy(expiryTime = Some(Instant.now))) // update expiry to make it faster

      soon {
        msg.getExpirationTime should be < Instant.now
        msg.isExpired shouldEqual true
        msg.getBody should not equal "test msg 1"
        msg.data.id shouldEqual messageId
      }

      // message still exists on receiver side
      val ConvMessages(ms) = (auto2 ? GetMessages(conv.data.remoteId)).await()
      ms.find(_.id == messageId) shouldBe defined
    }

    scenario("Delete message when it's read on receiver side") {
      val count = messages.size
      val msg = messages.getLastMessage

      auto2 ? MarkEphemeralRead(conv.data.remoteId, msg.data.id) should eventually(be(Successful))

      soon {
        msg.isDeleted shouldEqual true
        messages.size shouldEqual (count - 1)
      }(patience(10.seconds))
    }

    scenario("Send link") {
      sendMessage(new MessageContent.Text("test link: www.wire.com")) { msg =>
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getBody shouldEqual "test link: www.wire.com"
        msg.getParts.toSeq.map(_.getPartType) shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK)
      }
    }

    scenario("Send knock") {
      val count = messages.size
      conv.knock()

      soon {
        messages should have size (count + 1)
        val msg = messages.getLastMessage
        msg.getMessageType shouldEqual Message.Type.KNOCK
        assertEphemeralSent(msg)
      }
    }

    scenario("Send another knock") {
      val count = messages.size
      conv.knock()

      soon {
        messages should have size (count + 1)
        val msg = messages.getLastMessage
        msg.getMessageType shouldEqual Message.Type.KNOCK
        assertEphemeralSent(msg)
      }
    }

    scenario("Send image") {
      val asset = ImageAssetFactory.getImageAsset(IoUtils.toByteArray(getClass.getResourceAsStream("/images/penguin_128.png")))
      sendMessage(new MessageContent.Image(asset)) { msg =>
        msg.getMessageType shouldEqual Message.Type.ASSET
      } (PatienceConfig(15.seconds))
    }

    scenario("Send file asset") {
      val size = 100000
      val asset = impl.AssetForUpload(AssetId(), Some("asset"), Mime.Default, Some(size)) {
        val data = returning(Array.ofDim[Byte](size.toInt))(Random.nextBytes)
        _ => new ByteArrayInputStream(data)
      }

      sendMessage(new MessageContent.Asset(asset, DoNothingAndProceed)) { msg =>
        msg.getMessageType shouldEqual Message.Type.ANY_ASSET
      }
    }

    scenario("Send location") {
      sendMessage(new MessageContent.Location(50f, 20f, "location", 13)) { msg =>
        msg.getMessageType shouldEqual Message.Type.LOCATION
      }
    }

    scenario("Obfuscate location when timer expires") {
      val msg = messages.getLastMessage
      msg.getMessageType shouldEqual Message.Type.LOCATION
      zmessaging.messagesStorage.update(msg.data.id, _.copy(expiryTime = Some(Instant.now))) // update expiry to make it faster

      soon {
        msg.getExpirationTime should be < Instant.now
        msg.isExpired shouldEqual true
        msg.getLocation.getLatitude shouldEqual 0f
        msg.getLocation.getLongitude shouldEqual 0f
        msg.getLocation.getName should not be "location"
      }
    }
  }

  feature("Receiving") {

    var messageId = MessageId()
    scenario("Set remote conv to ephemeral") {
      soon {
        auto2 ? SetEphemeral(conv.data.remoteId, EphemeralExpiration.FIVE_SECONDS) should eventually(be(Successful))
      }
    }

    scenario("Receive text") {
      val count = messages.size
      (auto2 ? SendText(conv.data.remoteId, "test msg 2")).await() shouldEqual Successful

      soon {
        messages should have size (count + 1)
        val msg = messages.getLastMessage
        msg.getBody shouldEqual "test msg 2"
        msg.isEphemeral shouldEqual true
        msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
        msg.getExpirationTime shouldEqual Instant.MAX
      }
    }

    scenario("Start expiry timer when message is read - has update listener") {
      val msg = messages.get(messages.size - 1)
      msg.getExpirationTime shouldEqual Instant.MAX

      msg.addUpdateListener(new UpdateListener {
        override def updated(): Unit = ()
      })

      soon {
        msg.getExpirationTime should be < (Instant.now + msg.getEphemeralExpiration.duration)
      }
    }

    scenario("Delete message once the timer expires") {
      val msg = messages.getLastMessage
      msg.getBody shouldEqual "test msg 2"
      messageId = msg.data.id

      soon {
        msg.isDeleted shouldEqual true
        messages.getLastMessage.getId should not equal msg.getId
      }
    }

    scenario("Expired message should also be deleted on sending side") {
      soon {
        val ConvMessages(ms) = (auto2 ? GetMessages(conv.data.remoteId)).await()
        ms.find(_.id == messageId) shouldBe empty
      }
    }

    scenario("Receive knock") {
      val msg = withNewMessage {
        auto2 ? Knock(conv.data.remoteId) should eventually(be(Successful))
      }
      msg.getMessageType shouldEqual Message.Type.KNOCK
      msg.isEphemeral shouldEqual true
      msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
      msg.getExpirationTime shouldEqual Instant.MAX
    }

    scenario("Receive link") {
      val msg = withNewMessage {
        auto2 ? SendText(conv.data.remoteId, "link: www.wire.com") should eventually(be(Successful))
      }
      soon {
        msg.getMessageType shouldEqual Message.Type.RICH_MEDIA
        msg.getParts.toSeq.map(_.getPartType) shouldEqual Seq(Part.Type.TEXT, Part.Type.WEB_LINK)
        msg.isEphemeral shouldEqual true
        msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
        msg.getExpirationTime shouldEqual Instant.MAX
      }
    }

    scenario("Receive image") {
      val msg = withNewMessage {
        auto2 ? SendImageData(conv.data.remoteId, IoUtils.toByteArray(getClass.getResourceAsStream("/images/penguin.png"))) should eventually(be(Successful))
      }
      msg.getMessageType shouldEqual Message.Type.ASSET
      msg.isEphemeral shouldEqual true
      msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
      msg.getExpirationTime shouldEqual Instant.MAX
    }

    scenario("Start timer when full image is uploaded") {
      val msg = messages.get(messages.size - 1)
      msg.getMessageType shouldEqual Message.Type.ASSET

      var image = msg.getImage

      msg.addUpdateListener(new UpdateListener {
        override def updated(): Unit = image = msg.getImage
      })

      soon {
        image should not be null
        withClue(s"$image, data: ${image.data}") {
          image.data.versions.find(_.tag == ImageData.Tag.Medium) shouldBe defined // full image received
        }
        msg.getExpirationTime should be < (Instant.now + msg.getEphemeralExpiration.duration)
      } (DefaultPatience.PatienceConfig(15.seconds))
    }

    scenario("Delete asset when timer expires") {
      val msg = messages.getLastMessage
      msg.getMessageType shouldEqual Message.Type.ASSET

      val image = msg.getImage

      soon {
        msg.isDeleted shouldEqual true
        messages.getLastMessage.getId should not equal msg.getId
      }

      msg.getImage.isEmpty shouldEqual true

      // image data should be removed from local storage, and there should be no way to download it anymore (no remoteId)
      val spy = new BitmapSpy(image)
      soon {
        spy.failed shouldEqual true
      }
    }

    lazy val assetData = returning(Array.ofDim[Byte](100000))(Random.nextBytes)

    scenario("Receive file") {
      val msg = withNewMessage {
        val Successful(mid) = (auto2 ? SendAsset(conv.data.remoteId, assetData, "application/octet-stream", "random.dat")).await()
        messageId = MessageId(mid)
      }
      msg.getMessageType shouldEqual Message.Type.ANY_ASSET
      msg.isEphemeral shouldEqual true
      msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
      msg.getExpirationTime shouldEqual Instant.MAX
    }

    scenario("Start timer when file is uploaded") {
      val msg = messages.get(messages.size - 1)
      msg.getMessageType shouldEqual Message.Type.ANY_ASSET

      var asset = msg.getAsset

      msg.addUpdateListener(new UpdateListener {
        override def updated(): Unit = asset = msg.getAsset
      })

      soon {
        asset.getStatus shouldEqual AssetStatus.UPLOAD_IN_PROGRESS
      }
      msg.getExpirationTime shouldEqual Instant.MAX

      soon {
        asset.getStatus shouldEqual AssetStatus.UPLOAD_DONE
        msg.getExpirationTime should be < (Instant.now + msg.getEphemeralExpiration.duration)
      }
    }

    scenario("Receive location") {
      val msg = withNewMessage {
        val Successful(mid) = (auto2 ? SendLocation(conv.data.remoteId, 51f, 20f, "location", 13)).await()
        messageId = MessageId(mid)
      }
      msg.getMessageType shouldEqual Message.Type.LOCATION
      msg.isEphemeral shouldEqual true
      msg.getEphemeralExpiration shouldEqual EphemeralExpiration.FIVE_SECONDS
      msg.getExpirationTime shouldEqual Instant.MAX
      msg.getLocation shouldEqual new MessageContent.Location(51f, 20f, "location", 13)
    }
  }

  feature("Group messages") {

  }
}
