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

import com.waz.model.{IntegrationData, ProviderData, ProviderId}
import com.waz.sync.SyncServiceHandle
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.returning

import scala.concurrent.Future

trait IntegrationsService {
  def searchIntegrations(startWith: String): Signal[Seq[IntegrationData]]
  def getProvider(id: ProviderId): Future[ProviderData]

  def onIntegrationsSynced(name: String, data: Seq[IntegrationData]): Future[Unit]
  def onProviderSynced(id: ProviderId, data: ProviderData): Future[Unit]
}

class IntegrationsServiceImpl(sync: SyncServiceHandle) extends IntegrationsService {

  override def searchIntegrations(startWith: String) =
    intDataMap.getOrElse(startWith, returning(Signal[Seq[IntegrationData]]()) { sig =>
      intDataMap += (startWith -> sig)
      sync.syncIntegrations(startWith)
    })

  override def getProvider(id: ProviderId) =
    provDataMap.getOrElse(id, returning(Signal[ProviderData]()) { sig =>
      provDataMap += (id -> sig)
      sync.syncProvider(id)
    }).head

  override def onIntegrationsSynced(name: String, data: Seq[IntegrationData]) = intDataMap.get(name) match {
    case Some(signal) =>
      signal ! data
      Future.successful({})
    case None => Future.failed(new Exception(s"received sync data for unknown integrations name: $name"))
  }

  override def onProviderSynced(id: ProviderId, data: ProviderData) = provDataMap.get(id) match {
    case Some(signal) =>
      signal ! data
      Future.successful({})
    case None => Future.failed(new Exception(s"received sync data for unknown provider: $id"))
  }

  // TODO: switch to storage
  private var intDataMap = Map[String, SourceSignal[Seq[IntegrationData]]]()
  private var provDataMap = Map[ProviderId, SourceSignal[ProviderData]]()


}
