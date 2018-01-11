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

import com.waz.ZLog.debug
import com.waz.ZLog.ImplicitTag._
import com.waz.model.ProviderId
import com.waz.service.IntegrationsService
import com.waz.sync.SyncResult
import com.waz.sync.client.IntegrationsClient
import com.waz.threading.Threading

import scala.concurrent.Future

trait IntegrationsSyncHandler {
  def syncProvider(id: ProviderId): Future[SyncResult]
  def syncIntegrations(name: String): Future[SyncResult]
}

class IntegrationsSyncHandlerImpl(client: IntegrationsClient, service: IntegrationsService) extends IntegrationsSyncHandler {
  import Threading.Implicits.Background

  override def syncProvider(id: ProviderId) = client.getProvider(id).future.flatMap {
    case Right(data) =>
      debug(s"quering for provider with id $id returned $data")
      service.onProviderSynced(id, data).map(_ => SyncResult.Success)
    case Left(error) =>
      debug(s"quering for provider with id $id returned $error")
      Future.successful(SyncResult(error))
  }

  override def syncIntegrations(name: String) = client.getIntegrations(name).future.flatMap {
    case Right(data) =>
      debug(s"quering for integrations with name $name returned $data")
      service.onIntegrationsSynced(name, data).map(_ => SyncResult.Success)
    case Left(error) =>
      debug(s"quering for integrations with name $name returned $error")
      Future.successful(SyncResult(error))
  }
}
