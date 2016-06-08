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
package com.waz.tracking

import java.io.{ByteArrayInputStream, InputStream}

import android.content.Context
import com.waz.api._
import com.waz.api.impl.{CancelOnWarning, DoNothingAndProceed}
import com.waz.content.Mime
import com.waz.model._
import com.waz.service.ZMessaging.Factory
import com.waz.service.tracking.TrackingEventsService
import com.waz.testutils.DefaultPatienceConfig
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import org.scalatest._

import scala.concurrent.Future

class AssetTrackingSpec extends FeatureSpec with BeforeAndAfter with Matchers with OptionValues with ProvisionedApiSpec with Inspectors with DefaultPatienceConfig {
  import com.waz.api.KindOfTrackingEvent._

  override val provisionFile: String = "/two_users_connected.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val conv = conversations.head
  lazy val messages = conv.getMessages
  lazy val errors = api.getErrors

  var events = Seq.empty[TrackingEvent]

  override lazy val zmessagingFactory: Factory = new ApiZmessaging(_, _, _) {

    override lazy val trackingEvents: TrackingEventsService = new TrackingEventsService(handlerFactory, assetsStorage, messagesStorage, assetDownloader) {
      override def track(event: => TrackingEvent): Future[Unit] = {
        events = events :+ event
        Future.successful(())
      }
    }
  }

  before {
    events = Nil
    errors foreach { _.dismiss() }
  }

  def sendAsset(asset: AssetForUpload, handler: com.waz.api.MessageContent.Asset.ErrorHandler = DoNothingAndProceed) = {
    val content = new com.waz.api.MessageContent.Asset(asset, handler)
    zmessaging.convsUi.sendMessage(conv.data.id, content).await()
  }

  scenario("Try sending too large asset - with known size") {
    sendAsset(new TestAssetForUpload(Array.ofDim[Byte](26*1024*1024), Mime.Audio.MP4))

    errors should not be empty
    events.map(_.getKind) shouldEqual Seq(ASSET_UPLOAD_STARTED, ASSET_UPLOAD_FAILED)
  }


  scenario("Try sending too large asset - with initially unknown size") {
    sendAsset(new TestAssetForUpload(Array.ofDim[Byte](26*1024*1024), Mime.Audio.MP4) {
      override def sizeInBytes: Future[Option[Long]] = Future successful None
    })

    soon {
      errors should not be empty
      events.map(_.getKind) shouldEqual Seq(ASSET_UPLOAD_STARTED, ASSET_UPLOAD_FAILED)
    }
  }

  scenario("Send big asset on 3G network") {
    zmessaging.network.networkMode ! NetworkMode._3G
    sendAsset(new TestAssetForUpload(Array.ofDim[Byte](10*1024*1024), Mime.Audio.MP4))

    soon {
      errors shouldBe empty
      events.map(_.getKind) shouldEqual Seq(ASSET_UPLOAD_STARTED, ASSET_UPLOAD_SUCCESSFUL)
    }
  }
  
  scenario("Cancel big asset sending on 3G") {
    zmessaging.network.networkMode ! NetworkMode._3G
    sendAsset(new TestAssetForUpload(Array.ofDim[Byte](10*1024*1024), Mime.Audio.MP4), CancelOnWarning)

    soon {
      errors shouldBe empty
      events.map(_.getKind) shouldEqual Seq(ASSET_UPLOAD_STARTED, ASSET_UPLOAD_CANCELLED)
    }
  }

  class TestAssetForUpload(data: Array[Byte], mime: Mime, n: Option[String] = None) extends impl.AssetForUpload(AssetId()) {
    override def name: Future[Option[String]] = Future.successful(n)
    override def sizeInBytes: Future[Option[Long]] = Future successful Some(data.length.toLong)
    override def openDataStream(context: Context): InputStream = new ByteArrayInputStream(data)
    override def mimeType: Future[Mime] = Future.successful(mime)
  }
}
