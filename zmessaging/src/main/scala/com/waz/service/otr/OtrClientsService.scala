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
package com.waz.service.otr

import android.content.Context
import com.softwaremill.macwire._
import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.api.{ClientRegistrationState, Verification}
import com.waz.content.{ConversationStorage, MembersStorage, OtrClientsStorage, UsersStorage}
import com.waz.model.otr.{Client, ClientId, UserClients}
import com.waz.model.{OtrClientAddEvent, OtrClientEvent, OtrClientRemoveEvent, UserId}
import com.waz.service._
import com.waz.service.messages.MessagesService
import com.waz.sync.client.OtrClient
import com.waz.sync.otr.OtrClientsSyncHandler
import com.waz.sync.{SyncRequestService, SyncResult, SyncServiceHandle}
import com.waz.utils._
import com.waz.utils.events.Signal

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.duration._

class OtrClientsService(context: Context, users: UserService, zUsers: ZUserService, kvService: KeyValueService, cryptoBox: CryptoBoxService,
                        storage: OtrClientsStorage, content: OtrContentService, push: PushService, sync: SyncServiceHandle,
                        usersStorage: UsersStorage, apiClient: OtrClient, errors: ErrorsService, members: MembersStorage,
                        convs: ConversationStorage, messages: MessagesService, syncRequests: SyncRequestService,
                        clientsSync: => OtrClientsSyncHandler, timeouts: Timeouts) {

  import OtrClientsService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  private[waz] val updater = wire[VerificationStateUpdater]

  private lazy val lastSelfClientsSyncPref = kvService.keyValuePref("last_self_clients_sync_requested", 0L)

  zUsers.onVerifiedLogin {
    case Some(_) => requestSyncIfNeeded()
    case None =>
  }
  push.onSlowSyncNeeded { _ => sync.syncSelfClients() }

  val otrClientsProcessingStage = EventScheduler.Stage[OtrClientEvent] { (convId, events) =>
    users.withSelfUserFuture { selfUserId =>
      RichFuture.processSequential(events) {
        case OtrClientAddEvent(id, client) =>
          updateClients(Map(selfUserId -> Seq(client))).flatMap(_ => sync.syncPreKeys(selfUserId, Set(client.id)))
        case OtrClientRemoveEvent(id, clientId) =>
          removeClients(selfUserId, Seq(clientId)) flatMap { _ =>
            content.currentClientId flatMap {
              case Some(`clientId`) => onCurrentClientRemoved()
              case _ => Future.successful(())
            }
          }
      }
    }
  }

  def awaitClientRegistered(): Future[SyncResult] =
    getSelfClient flatMap {
      case None => clientsSync.syncSelfClients()
      case Some(c) =>
        verbose(s"awaitClientRegistered - already registered: $c, state: ${content.registrationStateSignal.currentValue}")
        requestSyncIfNeeded() flatMap { _ =>
          (content.registrationStatePref := ClientRegistrationState.REGISTERED) map { _ => SyncResult.Success }
        }
    }

  def requestSyncIfNeeded(retryInterval: FiniteDuration = 7.days) =
    getSelfClient.zip(lastSelfClientsSyncPref()) flatMap {
      case (Some(c), t) if t > System.currentTimeMillis() - retryInterval.toMillis => Future.successful(())
      case _ =>
        sync.syncSelfClients() flatMap { _ =>
          lastSelfClientsSyncPref := System.currentTimeMillis()
        }
    }

  def deleteClient(id: ClientId, password: String) = users.withSelfUserFuture { selfUser =>
    storage.get(selfUser) flatMap {
      case Some(cs) if cs.clients.contains(id) =>
        apiClient.deleteClient(id, password).future flatMap {
          case Right(_) => for {
            _ <- storage.update(selfUser, { uc => uc.copy(clients = uc.clients - id) })
            _ <- requestSyncIfNeeded()
          } yield Right(())
          case res => Future.successful(res)
        }
      case _ => Future.successful(Left(ErrorResponse.internalError("Client does not belong to current user or was already deleted")))
    }
  }

  def getClient(id: UserId, client: ClientId) = storage.get(id) map { _.flatMap(_.clients.get(client)) }

  def getOrCreateClient(id: UserId, client: ClientId) = {
    storage.get(id) flatMap {
      case Some(uc) if uc.clients.contains(client) => Future.successful(uc.clients(client))
      case _ =>
        def create = UserClients(id, Map(client -> Client(client, "")))
        def update(uc: UserClients) = uc.copy(clients = uc.clients.updated(client, uc.clients.getOrElse(client, Client(client, ""))))
        storage.updateOrCreate(id, update, create) flatMap { ucs =>
          val res = ucs.clients(client)
          if (res.isVerified) Future successful res
          else updater.awaitUpdated() map { _ => res } // force verification state processing to ensure that OTR_UNVERIFIED message is added before anything else
        }
    }
  }

  def updateClients(ucs: Map[UserId, Seq[Client]], replace: Boolean = false): Future[Set[UserClients]] = {

    def updateOrCreate(user: UserId, clients: Seq[Client]): (Option[UserClients] => UserClients) = {
      case Some(cs) =>
        val prev = cs.clients
        val updated: Map[ClientId, Client] = clients.map { c => c.id -> prev.get(c.id).fold(c)(_.updated(c)) }(breakOut)
        cs.copy(clients = if (replace) updated else prev ++ updated)
      case None =>
        UserClients(user, clients.map(c => c.id -> c)(breakOut))
    }

    // request clients location sync if some location has no name
    // location will be present only for self clients, but let's check that just to be explicit
    def requestLocationSyncIfNeeded(uss: Traversable[UserClients]) = {
      val needsSync = uss.filter(_.clients.values.exists(_.regLocation.exists(!_.hasName)))
      if (needsSync.nonEmpty)
      users.withSelfUserFuture { selfUserId =>
        if (needsSync.exists(_.user == selfUserId)) sync.syncClientsLocation() else Future.successful(())
      } .recoverWithLog()
    }

    storage.updateOrCreateAll(ucs.map { case (u, cs) => u -> updateOrCreate(u, cs) } (breakOut)) map { res =>
      requestLocationSyncIfNeeded(res)
      res
    }
  }

  def updateUserClients(user: UserId, clients: Seq[Client], replace: Boolean = false) = {
    verbose(s"updateUserClients($user, $clients, $replace)")
    updateClients(Map(user -> clients), replace).map(_.head)
  }

  def onCurrentClientRemoved() =
    for {
      _ <- zUsers.logout()
      _ <- content.registrationStatePref := ClientRegistrationState.UNKNOWN
      _ <- content.clientIdPref := None
      _ <- cryptoBox.deleteCryptoBox()
    } yield ()

  def removeClients(user: UserId, clients: Seq[ClientId]) =
    storage.update(user, { cs =>
      cs.copy(clients = cs.clients -- clients)
    })

  def updateSelfClients(clients: Seq[Client], replace: Boolean = true) = users.getSelfUser flatMap {
    case Some(user) => updateUserClients(user.id, clients, replace)
    case None =>
      throw new Exception(s"No self user found in updateClients($clients)")
  }

  def updateClientLabel(id: ClientId, label: String) =
    users.withSelfUserFuture { selfUserId =>
      storage.update(selfUserId, { cs =>
        cs.clients.get(id).fold(cs) { client =>
          cs.copy(clients = cs.clients.updated(id, client.copy(label = label)))
        }
      })
    } flatMap {
      case Some(_) =>
        verbose(s"clientLabel updated, client: $id, label: $label")
        sync.postClientLabel(id, label)
      case None =>
        verbose(s"client label was not updated ($id, $label)")
        Future.successful(())
    }

  def selfClient = content.currentClientIdSignal flatMap { _ =>
    Signal.future(getSelfClient) collect {
      case Some(client) => client
    }
  }

  def getSelfClient: Future[Option[Client]] = content.currentClientId flatMap {
    case None => Future.successful(None)
    case Some(clientId) =>
      verbose(s"getSelfClient(), clientId: $clientId")
      users.getSelfUser flatMap {
        case Some(selfUser) =>
          storage.get(selfUser.id) map {
            case Some(cs) =>
              verbose(s"self clients: $cs")
              cs.clients.get(clientId)
            case _ => None
          }
        case None => Future.successful(None)
      }
  }

  def incomingClientsSignal: Signal[Seq[Client]] =
    content.currentClientIdSignal flatMap {
      case None => Signal const Seq.empty[Client]
      case Some(clientId) =>
        for {
          userId <- users.selfUserId.signal
          ucs <- getClientsSignal(userId)
        } yield
          ucs.clients.get(clientId).flatMap(_.regTime).fold(Seq.empty[Client]) { current =>
            ucs.clients.values.filter(c => c.verified == Verification.UNKNOWN && c.regTime.exists(_.isAfter(current))).toVector
          }
    }

  def getClients(user: UserId) = storage.get(user).map(_.fold(Seq.empty[Client])(_.clients.values.toVector))

  def getClientsSignal(user: UserId) = storage.signal(user)

  def updateVerified(userId: UserId, clientId: ClientId, verified: Boolean) = storage.update(userId, { uc =>
    uc.clients.get(clientId) .fold (uc) { client =>
      uc.copy(clients = uc.clients + (client.id -> client.copy(verified = if (verified) Verification.VERIFIED else Verification.UNVERIFIED)))
    }
  })
}

object OtrClientsService {
  private implicit val tag: LogTag = logTagFor[OtrClientsService]
}
