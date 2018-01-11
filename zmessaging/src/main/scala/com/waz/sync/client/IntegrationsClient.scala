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
package com.waz.sync.client

import com.waz.ZLog.warn
import com.waz.ZLog.ImplicitTag._
import com.waz.model._
import com.waz.threading.Threading
import com.waz.utils.JsonDecoder
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet._
import com.waz.znet.ZNetClient.ErrorOrResponse

import scala.util.Try

trait IntegrationsClient {
  def getProvider(id: ProviderId): ErrorOrResponse[ProviderData]
  def getIntegrations(name: String): ErrorOrResponse[Seq[IntegrationData]]
}

class IntegrationsClientImpl(netClient: ZNetClient) extends IntegrationsClient {
  import IntegrationsClient._
  import Threading.Implicits.Background

  def getProvider(id: ProviderId) =
    netClient.withErrorHandling("getProvider", Request.Get(providerPath(id))) {
      case Response(SuccessHttpStatus(), ProviderResponse(data), _) => data
    }

  def getIntegrations(name: String) =
    netClient.withErrorHandling("getIntegrations", Request.Get(integrationsPath(name))) {
      case Response(SuccessHttpStatus(), IntegrationResponse(data), _) => data
    }
}

object IntegrationsClient {

  def apply(netClient: ZNetClient): IntegrationsClient = new IntegrationsClientImpl(netClient)

  val IntegrationsPath = "/services"
  val DefaultTag = "tutorial"
  val ProvidersPath = "/providers"

  def integrationsPath(name: String): String =
    if (name.isEmpty) Request.query(IntegrationsPath, "tags" -> DefaultTag)
    else Request.query(IntegrationsPath, "tags" -> DefaultTag, "name" -> name)

  object IntegrationResponse {
    import JsonDecoder._

    def unapply(resp: ResponseContent): Option[Seq[IntegrationData]] = resp match {
      case JsonObjectResponse(js) if js.has("services") => Try(decodeSeq('services)(js, IntegrationData.Decoder)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

  def providerPath(id: ProviderId): String = s"$ProvidersPath/$id"

  object ProviderResponse {
    def unapply(resp: ResponseContent): Option[ProviderData] = resp match {
      case JsonObjectResponse(js) => Try(ProviderData.Decoder(js)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }
}
