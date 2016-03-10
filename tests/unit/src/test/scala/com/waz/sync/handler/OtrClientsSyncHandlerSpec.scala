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
package com.waz.sync.handler

import com.waz.RobolectricUtils
import com.waz.api.Verification
import com.waz.model.otr.{Client, ClientId, SignalingKey}
import com.waz.model.{UserData, UserId, ZUserId}
import com.waz.sync.SyncResult
import com.waz.sync.client.OtrClient
import com.waz.testutils.Matchers._
import com.waz.testutils.{DefaultPatienceConfig, MockZMessaging}
import com.waz.threading.CancellableFuture
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.wire.cryptobox.PreKey
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class OtrClientsSyncHandlerSpec extends FeatureSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll with OptionValues with RobolectricTests with RobolectricUtils with ScalaFutures with DefaultPatienceConfig { test =>
  import com.waz.threading.Threading.Implicits.Background

  lazy val selfUser = UserData("test")
  lazy val clientId = ClientId()

  var clients = Seq.empty[Client]
  var remainingKeys = Seq.empty[Int]
  var loadSelfClientsRequested = false
  var updatedKeys = Seq.empty[PreKey]
  var postClientRequest = Option.empty[(Client, PreKey, Seq[PreKey], Option[String])]

  class TestService extends MockZMessaging() {

    override lazy val otrClient: OtrClient = new OtrClient(znetClient) {
      override def postClient(user: ZUserId, client: Client, lastKey: PreKey, keys: Seq[PreKey], password: Option[String]): ErrorOrResponse[Client] = {
        postClientRequest = Some((client, lastKey, keys, password))
        CancellableFuture.successful(Right(client.copy(clientId)))
      }

      override def loadClients(): ErrorOrResponse[Seq[Client]] = {
        loadSelfClientsRequested = true
        CancellableFuture.successful(Right(clients))
      }

      override def loadClients(user: UserId): ErrorOrResponse[Seq[Client]] = {
        CancellableFuture.successful(Right(clients))
      }

      override def updateKeys(id: ClientId, prekeys: Seq[PreKey], lastKey: Option[PreKey], sigKey: Option[SignalingKey]): ErrorOrResponse[Unit] = {
        updatedKeys = prekeys
        CancellableFuture.successful(Right(()))
      }

      override def loadRemainingPreKeys(id: ClientId): ErrorOrResponse[Seq[Int]] = CancellableFuture.successful(Right(remainingKeys))

      override def loadPreKeys(users: Map[UserId, Seq[ClientId]]): ErrorOrResponse[Map[UserId, Seq[(ClientId, PreKey)]]] = CancellableFuture.lift(cryptoBox { cb =>
        Future successful Right(users.map { case (u, cs) => u -> cs.map(c => c -> cb.newPreKeys(100, 1).head)})
      } map (_.get))
    }

    usersStorage.addOrOverwrite(selfUser).futureValue
    (users.selfUserId := selfUser.id).futureValue
  }

  lazy val service = new TestService

  before {
    loadSelfClientsRequested = false
    clients = Seq.empty[Client]
    remainingKeys = Seq.empty[Int]
    updatedKeys = Seq.empty[PreKey]
    postClientRequest = None
  }

  feature("Client registration") {

    scenario("Register client on first start") {
      service.otrClientsSync.syncSelfClients() should eventually(be(SyncResult.Success))
      service.otrClientsService.getSelfClient should eventually(be('defined))

      val client = service.otrClientsService.getSelfClient.futureValue.get
      client.id shouldEqual clientId
      client.signalingKey shouldBe 'defined
      client.verified shouldEqual Verification.VERIFIED

      postClientRequest should not be empty
      postClientRequest.get._3 should have size 100
      postClientRequest.get._3.last.id shouldEqual 99
      updatedKeys shouldBe empty
      service.otrService.lastPreKeyId().futureValue shouldEqual 99
    }

    scenario("Sync clients on restart") {
      remainingKeys = Seq.tabulate(100)(identity)
      clients = Seq(Client(clientId, "label", "model", None), Client(ClientId(), "label1", "model1", None))

      service.otrClientsSync.syncSelfClients() should eventually(be(SyncResult.Success))
      service.otrClientsService.getSelfClient should eventually(be('defined))

      val client = service.otrClientsService.getSelfClient.futureValue.get
      client.signalingKey shouldBe 'defined
      client.verified shouldEqual Verification.VERIFIED

      updatedKeys shouldBe empty
    }

    scenario("Register new client and sync previous on first start") {
      remainingKeys = Seq.empty
      clients = Seq(Client(ClientId("client1"), "label", "model", None), Client(ClientId("client2"), "label1", "model1", None))

      val service1 = new TestService
      service1.otrClientsSync.syncSelfClients() should eventually(be(SyncResult.Success))
      service1.otrClientsService.getSelfClient.futureValue.map(_.id) shouldEqual Some(clientId)

      val cs = service1.otrClientsStorage.signal(selfUser.id)
      withDelay(cs.currentValue shouldBe 'defined)
      cs.currentValue.map(_.clients).get should have size 3

      postClientRequest should not be empty
      postClientRequest.get._3 should have size 100
      postClientRequest.get._3.last.id shouldEqual 99
      updatedKeys shouldBe empty
      service1.otrService.lastPreKeyId().futureValue shouldEqual 99
    }
  }

  feature("PreKeys upload") {

    scenario("Upload more prekeys when needed") {
      remainingKeys = (67 to 99).toSeq
      clients = Seq(Client(clientId, "label", "model", None), Client(ClientId(), "label1", "model1", None))

      service.otrClientsSync.syncSelfClients() should eventually(be(SyncResult.Success))

      updatedKeys.map(_.id) should have size 67
      updatedKeys.head.id shouldEqual 100
      updatedKeys.last.id shouldEqual 166
      service.otrService.lastPreKeyId().futureValue shouldEqual 166
    }
  }

  feature("Client info sync") {

    scenario("Sync clients for current user should use self clients endpoint") {
      service.otrClientsSync.syncClients(selfUser.id) should eventually(be(SyncResult.Success))

      loadSelfClientsRequested shouldEqual true
    }
  }
}
