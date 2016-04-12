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
package com.waz.testutils

import com.waz.api.Message
import com.waz.model.MessageData.MessageDataDao
import com.waz.model.ZUser.ZUserDao
import com.waz.model._
import com.waz.model.otr.Client
import com.waz.service._
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.OtrClient
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.ui.UiModule
import com.waz.utils.returning
import com.waz.znet.{AsyncClient, ClientWrapper, TestClientWrapper}
import com.waz.znet.AuthenticationManager.Token
import com.waz.znet.ZNetClient.{EmptyAsyncClient, ErrorOrResponse}
import com.wire.cryptobox.PreKey
import net.hockeyapp.android.Constants
import org.robolectric.Robolectric

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class MockGlobalModule extends GlobalModule(Robolectric.application, BackendConfig.EdgeBackend) {
  override lazy val client: AsyncClient = new EmptyAsyncClient
  override lazy val clientWrapper: ClientWrapper = TestClientWrapper
  ZMessaging.context = context
  Constants.loadFromContext(context)
}

class MockInstance(global: GlobalModule = new MockGlobalModule, factory: ZMessaging.Factory = new MockZMessaging(_, _, _)) extends InstanceService(Robolectric.application, global, factory)

class MockZMessaging(instance: InstanceService = new MockInstance, zuser: ZUser = ZUser(EmailAddress("email"), "passwd"), token: Option[Token] = None) extends ZMessaging(instance, zuser, token) {
  override lazy val sync: SyncServiceHandle = new EmptySyncService
  import Threading.Implicits.Background

  var timeout = 5.seconds

  override lazy val mediamanager: MediaManagerService = new MediaManagerService(instance.global.context, prefs) {
    override lazy val mediaManager = None
  }

  override lazy val otrClient: OtrClient = new OtrClient(znetClient) {
    var client = Option.empty[Client]

    override def loadClients(): ErrorOrResponse[Seq[Client]] = CancellableFuture.successful(Right(client.toSeq))

    override def postClient(userId: ZUserId, client: Client, lastKey: PreKey, keys: Seq[PreKey], password: Option[String]): ErrorOrResponse[Client] = {
      this.client = Some(client)
      CancellableFuture.successful(Right(client))
    }
  }

  def insertConv(conv: ConversationData) = Await.result(convsStorage.insert(conv), timeout)
  def getConv(id: ConvId): Option[ConversationData] = Await.result(convsContent.convById(id), timeout)
  def getConv(id: RConvId): Option[ConversationData] = Await.result(convsContent.convByRemoteId(id), timeout)
  def getConv(id: UserId): Option[ConversationData] = getConv(ConvId(id.str))
  def listConvs = Await.result(convsStorage.list, timeout)
  def deleteAllConvs() = Await.result(
    for {
      cs <- convsStorage.list.map(_.map(_.id))
      _ <- Future.traverse(cs) { convsStorage.remove }
      _ <- Future.traverse(cs) { messagesStorage.deleteAll }
    } yield (), timeout)

  def listMembers(conv: ConvId) = Await.result(membersStorage.get(conv), timeout)

  def insertUsers(users: Seq[UserData]) = users map { user => Await.result(usersStorage.addOrOverwrite(user), timeout) }
  def insertUser(user: UserData) = Await.result(usersStorage.addOrOverwrite(user), timeout)
  def getUser(id: UserId) = Await.result(users.getUser(id), timeout)

  def dispatch(es: Event*) = eventPipeline(es)
  def dispatchEvent(es: Event*) = dispatchEvents(es)
  def dispatchEvents(es: Seq[Event]) = Await.result(eventPipeline(es), timeout)
  def connectToUser(user: UserData) = Await.result(connection.connectToUser(user.id, "Hi there", user.name), timeout)

  def addMember(conv: ConvId, user: UserId) = Await.result(membersStorage.add(conv, user), timeout)
  def removeMember(conv: ConvId, user: UserId) = Await.result(membersStorage.remove(conv, user), timeout)
  def listActiveMembers(conv: ConvId) = Await.result(membersStorage.getActiveUsers(conv), timeout).toList


  def addMessage(msg: MessageData) = Await.result(messagesStorage.addMessage(msg), timeout)
  def delMessages(conv: ConvId) = Await.result(messagesStorage.deleteAll(conv), timeout)
  def listMessages(conv: ConvId) = {
    Thread.sleep(100)
    Await.result(storage { db => MessageDataDao.list(MessageDataDao.findMessages(conv)(db)).sortBy(_.time) }, timeout)
  }
  def lastMessage(conv: ConvId) = Await.result(messagesStorage.getLastMessage(conv), timeout)
  def lastLocalMessage(conv: ConvId, tpe: Message.Type) = Await.result(messagesStorage.lastLocalMessage(conv, tpe), timeout)
}

class MockUiModule(zmessaging: ZMessaging) extends UiModule(zmessaging.instance) {
  import Threading.Implicits.Background
  ZMessaging.currentUi = this
  setCurrent(Some(zmessaging))

  def setCurrent(zms: Option[ZMessaging]) = zms match {
    case Some(z) =>
      instance.instanceMap(z.userId) = z
      global.storage { ZUserDao.insertOrReplace(z.user.user)(_) } map { _ =>
        instance.currentUserPref := z.userId.str
      }
    case None =>
      instance.currentUserPref := ""
  }
}

object MockUiModule {

  def apply(instance: InstanceService): UiModule = returning(new UiModule(instance)) { ui =>
    ZMessaging.currentUi = ui
  }

  def apply(zmessaging: ZMessaging): UiModule = new MockUiModule(zmessaging)
}
