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
package com.waz.sync.otr

import android.content.Context
import android.location.Geocoder
import com.waz.ZLog._
import com.waz.api.impl.ErrorResponse
import com.waz.api.{ClientRegistrationState, ZmsVersion}
import com.waz.content.OtrClientsStorage
import com.waz.model.UserId
import com.waz.model.otr.{Client, ClientId, Location, UserClients}
import com.waz.service._
import com.waz.service.otr.{OtrContentService, OtrService}
import com.waz.sync.client.OtrClient
import com.waz.sync.{SyncResult, SyncServiceHandle}
import com.waz.threading.Threading
import com.waz.utils.{Locales, LoggedTry, Serialized}
import com.waz.znet.Response.Status

import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.concurrent.Future

class OtrClientsSyncHandler(context: Context, user: ZUserService, users: UserService, content: OtrContentService, client: OtrClient, otr: OtrService, storage: OtrClientsStorage, errors: ErrorsService, instance: InstanceService, sync: SyncServiceHandle, kvService: KeyValueService) {
  import ClientRegistrationState._
  import OtrClientsSyncHandler._
  import com.waz.threading.Threading.Implicits.Background

  // keeps ZMS_MAJOR_VERSION number of client registration
  // this is used to detect problematic version updates
  lazy val clientRegVersion = kvService.keyValuePref("otr_client_reg_version", 0)

  lazy val geocoder = new Geocoder(context, Locales.currentLocale)

  def syncSelfClients(): Future[SyncResult] = Serialized.future("sync-self-clients") { // serialized to avoid races between sync service and OtrClientsService.awaitClientRegistered (used from login)
    verbose(s"syncSelfClients")

    def updatePreKeys(id: ClientId) =
      client.loadRemainingPreKeys(id).future flatMap {
        case Right(ids) =>
          verbose(s"remaining prekeys: $ids")
          otr.generatePreKeysIfNeeded(ids) flatMap {
            case keys if keys.isEmpty => Future.successful(SyncResult.Success)
            case keys => client.updateKeys(id, keys).future map {
              case Right(_) => SyncResult.Success
              case Left(error) => SyncResult(error)
            }
          }
        case Left(error) => Future.successful(SyncResult(error))
      }

    client.loadClients().future flatMap {
      case Right(clients) =>
        verbose(s"loaded clients: $clients")
        otr.clients.updateSelfClients(clients) flatMap { ucs =>
          content.currentClientId flatMap {
            case None => registerClient()
            case Some(clientId) =>
              if (ucs.clients.contains(clientId)) updatePreKeys(clientId)
              else registerClient() // FIXME: should we try to register in that case, if we have an id but backend doesn't that means our device was deleted from it
          }
        }
      case Left(error) => Future.successful(SyncResult(error))
    }
  }

  private def registerClient() = otr.createClient() flatMap {
    case None => Future successful SyncResult(ErrorResponse.internalError("CryptoBox missing"))
    case Some((c, lastKey, keys)) =>
      client.postClient(user.user.id, c, lastKey, keys, user.user.password).future flatMap {
        case Right(cl) =>
          for {
            _ <- otr.clients.updateSelfClients(Seq(cl), replace = false)
            _ <- content.clientIdPref := Some(cl.id)
            _ <- content.registrationStatePref := REGISTERED
            _ <- clientRegVersion := ZmsVersion.ZMS_MAJOR_VERSION
            _ = sync.registerGcm()
          } yield
            SyncResult.Success
        case Left(error @ ErrorResponse(Status.Forbidden, _, "missing-auth")) =>
          warn(s"client registration not allowed: $error, password missing")
          (content.registrationStatePref := PASSWORD_MISSING) map { _ => SyncResult.aborted() }
        case Left(error @ ErrorResponse(Status.Forbidden, _, "too-many-clients")) =>
          warn(s"client registration not allowed: $error")
          (content.registrationStatePref := LIMIT_REACHED) map { _ => SyncResult.aborted() }
        case Left(error) =>
          Future.successful(SyncResult(error))
      }
  }

  def syncClients(user: UserId): Future[SyncResult] =
    users.withSelfUserFuture { selfUserId =>
      verbose(s"syncClients")

      def loadClients = (if (user == selfUserId) client.loadClients() else client.loadClients(user)).future

      def currentClientId = if (user == selfUserId) content.currentClientId else Future successful None

      def withoutSession(currentId: Option[ClientId], clients: Iterable[ClientId]) =
        Future.traverse(clients) { client =>
          if (currentId.contains(client)) Future successful None
          else otr.hasSession(user, client) map { if (_) None else Some(client) }
        } map { _.flatten.toSeq }

      def syncSessionsIfNeeded(clients: Iterable[ClientId]) =
        for {
          currentId <- currentClientId
          toSync <- withoutSession(currentId, clients)
          err <- if (toSync.isEmpty) Future successful None else syncSessions(Map(user -> toSync))
        } yield
          err.fold[SyncResult](SyncResult.Success)(SyncResult(_))

      loadClients flatMap {
        case Left(error) => Future successful SyncResult(error)
        case Right(clients) =>
          otr.clients.updateUserClients(user, clients, replace = true) flatMap { ucs =>
            syncSessionsIfNeeded(ucs.clients.keys)
          }
      }
    }

  def postLabel(id: ClientId, label: String): Future[SyncResult] =
    client.postClientLabel(id, label).future map {
      case Right(_) => SyncResult.Success
      case Left(err) => SyncResult(err)
    }

  def syncPreKeys(clients: Map[UserId, Seq[ClientId]]): Future[SyncResult] = syncSessions(clients) map {
    case Some(error) => SyncResult(error)
    case None => SyncResult.Success
  }

  private[otr] def syncSessions(clients: Map[UserId, Seq[ClientId]]): Future[Option[ErrorResponse]] =
    client.loadPreKeys(clients).future
      .flatMap {
        case Left(error) => Future.successful(Some(error))
        case Right(us) =>
          for {
            _ <- otr.clients.updateClients(us.mapValues(_.map { case (id, key) => Client(id, "") }))
            prekeys = us.flatMap { case (u, cs) => cs map { case (c, p) => (OtrService.sessionId(u, c), p)} }
            _ <- Future.traverse(prekeys) { case (id, p) => otr.sessions.getOrCreateSession(id, p) }
            _ <- otr.clients.updater.awaitUpdated()
          } yield None
      }
      .recover {
        case e: Throwable => Some(ErrorResponse.internalError(e.getMessage))
      }

  def syncClientsLocation(): Future[SyncResult] = {

    def loadName(lat: Double, lon: Double) = Future {
      LoggedTry.local(geocoder.getFromLocation(lat, lon, 1).asScala).toOption.flatMap(_.headOption).flatMap { add =>
        Option(Seq(Option(add.getLocality), Option(add.getCountryCode)).flatten.mkString(", ")).filter(_.nonEmpty)
      }
    } (Threading.BlockingIO)

    def loadNames(locs: Iterable[Location]) =
      Future.traverse(locs) { l => loadName(l.lat, l.lon).map { (l.lat, l.lon) -> _ } }

    def updateClients(locs: Map[(Double, Double), String])(ucs: UserClients) =
      ucs.copy(clients = ucs.clients.mapValues { c =>
        c.regLocation.flatMap { l =>
          locs.get((l.lat, l.lon)).map(n => l.copy(name = n))
        }.fold(c) { loc => c.copy(regLocation = Some(loc)) }
      })

    users.withSelfUserFuture { selfUserId =>
      storage.get(selfUserId) flatMap {
        case None => Future successful SyncResult.Success
        case Some(ucs) =>
          val toSync = ucs.clients.values collect {
            case Client(_, _, _, _, Some(loc), _, _, _, _) if !loc.hasName => loc
          }
          if (toSync.isEmpty) Future successful SyncResult.Success
          else
            for {
              ls <- loadNames(toSync)
              locations: Map[(Double, Double), String] = ls.collect { case (k, Some(name)) => k -> name }(breakOut)
              update <- storage.update(selfUserId, updateClients(locations))
            } yield {
              update match {
                case Some((_, UserClients(_, cs))) if cs.values.forall(_.regLocation.forall(_.hasName)) => SyncResult.Success
                case _ =>
                  verbose(s"user clients were not updated, locations: $locations, toSync: $toSync")
                  SyncResult.failed()
              }
            }
      }
    }
  }
}

object OtrClientsSyncHandler {
  private implicit val tag: LogTag = logTagFor[OtrClientsSyncHandler]
}
