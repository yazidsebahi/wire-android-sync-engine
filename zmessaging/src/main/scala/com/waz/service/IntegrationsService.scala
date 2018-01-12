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

import com.waz.model._
import com.waz.sync.{SyncRequestService, SyncServiceHandle}
import com.waz.threading.Threading
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.returning

import scala.concurrent.Future

trait IntegrationsService {
  def searchIntegrations(startWith: String): Signal[Seq[IntegrationData]]
  def getIntegration(pId: ProviderId, iId: IntegrationId): Future[IntegrationData]
  def getProvider(id: ProviderId): Future[ProviderData]

  def onIntegrationsSynced(name: String, data: Seq[IntegrationData]): Future[Unit]
  def onProviderSynced(pId: ProviderId, data: ProviderData): Future[Unit]
  def onIntegrationSynced(pId: ProviderId, iId: IntegrationId, data: IntegrationData): Future[Unit]

  def addBot(cId: ConvId, pId: ProviderId, iId: IntegrationId): Future[Unit]
  def removeBot(cId: ConvId, botId: UserId): Future[Unit]
}

class IntegrationsServiceImpl(sync: SyncServiceHandle, syncRequestService: SyncRequestService) extends IntegrationsService {
  implicit val ctx = Threading.Background

  override def searchIntegrations(startWith: String) =
    integrationSearch.getOrElse(startWith, returning(Signal[Seq[IntegrationData]]()) { sig =>
      integrationSearch += (startWith -> sig)
      sync.syncIntegrations(startWith)
    })

  override def getIntegration(pId: ProviderId, iId: IntegrationId) =
    if (integrations.contains(iId)) Future.successful(integrations(iId))
    else sync.syncIntegration(pId, iId).flatMap(syncRequestService.scheduler.await).map(_ => integrations(iId))

  override def getProvider(pId: ProviderId) =
    if (providers.contains(pId)) Future.successful(providers(pId))
    else sync.syncProvider(pId).flatMap(syncRequestService.scheduler.await).map(_ => providers(pId))

  override def onIntegrationsSynced(name: String, data: Seq[IntegrationData]) = integrationSearch.get(name) match {
    case Some(signal) =>
      signal ! data
      Future.successful({})
    case None => Future.failed(new Exception(s"received sync data for unknown integrations name: $name"))
  }

  override def onIntegrationSynced(pId: ProviderId, iId: IntegrationId, data: IntegrationData) = {
    integrations += iId -> data
    Future.successful({})
  }

  override def onProviderSynced(id: ProviderId, data: ProviderData) = {
    providers += id -> data
    Future.successful({})
  }

  // pId here is redundant - we can take it from our 'integrations' map
  override def addBot(cId: ConvId, pId: ProviderId, iId: IntegrationId) = for {
    _ <- sync.postAddBot(cId, pId, iId)
  } yield ()

  override def removeBot(cId: ConvId, botId: UserId) = for {
    _ <- sync.postRemoveBot(cId, botId)
  } yield ()

  private var integrationSearch  = Map[String, SourceSignal[Seq[IntegrationData]]]()
  private var providers = Map[ProviderId, ProviderData]()
  private var integrations = Map[IntegrationId, IntegrationData]()
}
