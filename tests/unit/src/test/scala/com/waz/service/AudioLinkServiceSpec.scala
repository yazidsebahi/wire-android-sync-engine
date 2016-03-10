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
package com.waz.service

import java.nio.{ByteBuffer, ByteOrder}
import java.util.UUID

import android.database.sqlite.SQLiteDatabase
import com.waz.RobolectricUtils
import com.waz.model.{EmailAddress, UserId, ZUser}
import com.waz.testutils.MockZMessaging
import com.waz.utils.events.{EventContext, Subscription}
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class AudioLinkServiceSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfter with RobolectricTests with RobolectricUtils { test =>
  implicit val timeout: FiniteDuration = 2.seconds
  implicit val ev = EventContext.Global

  val selfUserId = UserId()

  var initialized = false
  var sending = Option.empty[Array[Byte]]
  var listening = false
  var nearby = Seq.empty[UserId]
  var subscription = Option.empty[Subscription]

  lazy val service = new MockZMessaging(zuser =ZUser(EmailAddress("email"), "passwd").copy(emailVerified = true)) {
    override lazy val audiolink = new AudioLinkService(context, metadata, users, lifecycle) {
      override lazy val audioLink: AudioLink = new AudioLink {
        initialized = true
        override def startSending(msg: Array[Byte]): Unit = sending = Some(msg)
        override def stopSending(): Unit = sending = None
        override def startListening(): Unit = listening = true
        override def stopListening(): Unit = listening = false
      }
    }

    override def metadata: MetaDataService = new MetaDataService(context) {
      override lazy val audioLinkEnabled: Boolean = true
    }
  }

  implicit def db: SQLiteDatabase = service.storage.dbHelper.getWritableDatabase

  def subscribe() = {
    subscription = Some(service.audiolink.nearbyUsers { users => nearby = users })
  }

  feature("SoundLink initialization") {

    scenario("Don't initialize sound link when not needed") {
      awaitUi(200.millis)
      initialized shouldEqual false
    }
  }

  feature("Receiving") {

    scenario("Start listening when nearbyUsers signal is wired") {
      subscribe()
      withDelay(listening shouldEqual true)
    }

    scenario("Report received user Id") {
      val uid = UUID.randomUUID()
      val bytes = ByteBuffer.allocate(16).order(ByteOrder.BIG_ENDIAN).putLong(uid.getMostSignificantBits).putLong(uid.getLeastSignificantBits).array()
      service.audiolink.messageReceived(bytes)
      withDelay {
        nearby shouldEqual Seq(UserId(uid.toString))
      }
    }

    scenario("Stop listening when nearbyUsers signal is unwired") {
      subscription.get.destroy()
      withDelay(listening shouldEqual false)
    }
  }

  feature("Sending") {

    scenario("Start sending only if self user id is set") {
      Await.result(service.users.selfUserId(), 1.second) shouldEqual UserService.SelfUserId
      service.lifecycle.acquireUi("test")
      awaitUi(100.millis)
      sending shouldEqual None
      service.users.selfUserId := selfUserId
      withDelay {
        sending should not be empty
        AudioLinkService.decode(sending.get) shouldEqual Some(selfUserId)
      }
      service.lifecycle.releaseUi("test")
      withDelay(sending shouldEqual None)
    }

    scenario("Start sending self user id when ui is activated") {
      service.lifecycle.acquireUi("test")
      withDelay {
        sending should not be empty
        AudioLinkService.decode(sending.get) shouldEqual Some(selfUserId)
      }
    }

    scenario("Stop sending when ui is paused") {
      service.lifecycle.releaseUi("test")
      withDelay {
        sending shouldEqual None
      }
    }
  }
}
