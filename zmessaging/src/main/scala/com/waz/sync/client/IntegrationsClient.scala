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
import com.waz.model.{AssetId, EmailAddress, IntegrationId, ProviderId}
import com.waz.sync.client.IntegrationsClient.{IntegrationEntry, ProviderEntry}
import com.waz.threading.Threading
import com.waz.utils.JsonDecoder
import com.waz.utils.wrappers.URI
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet.{Request, _}
import com.waz.znet.ZNetClient.ErrorOrResponse
import org.json.JSONObject

import scala.util.Try

trait IntegrationsClient {
  def getProvider(id: ProviderId): ErrorOrResponse[ProviderEntry]
  def getIntegrations(name: String): ErrorOrResponse[Seq[IntegrationEntry]]
}

class IntegrationsClientImpl(netClient: ZNetClient) extends IntegrationsClient {
  import IntegrationsClient._
  import Threading.Implicits.Background

  def getProvider(id: ProviderId) =
    netClient.withErrorHandling("getProvider", Request.Get(providerPath(id))) {
      case Response(SuccessHttpStatus(), ProviderEntryResponse(data), _) => data
    }

  def getIntegrations(name: String) =
    netClient.withErrorHandling("getProvider", Request.Get(integrationsPath(name))) {
      case Response(SuccessHttpStatus(), IntegrationEntryResponse(data), _) => data
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

  case class IntegrationAsset(assetType: String, id: AssetId)
  case class IntegrationEntry(id: IntegrationId,
                              provider: ProviderId,
                              name: String,
                              description: String,
                              assets: Seq[IntegrationAsset],
                              tags: Seq[String],
                              enabled: Boolean)

  object IntegrationEntry {
    import JsonDecoder._

    implicit lazy val AssetDecoder: JsonDecoder[IntegrationAsset] = new JsonDecoder[IntegrationAsset] {
      override def apply(implicit js: JSONObject): IntegrationAsset = IntegrationAsset('type, decodeId[AssetId]('key))
    }

    implicit lazy val Decoder: JsonDecoder[IntegrationEntry] = new JsonDecoder[IntegrationEntry] {
      override def apply(implicit js: JSONObject): IntegrationEntry =
        IntegrationEntry(
          decodeId[IntegrationId]('id),
          decodeId[ProviderId]('provider),
          'name,
          'description,
          decodeSeq[IntegrationAsset]('assets),
          decodeStringSeq('tags),
          'enabled
        )
    }
  }

  object IntegrationEntryResponse {
    import JsonDecoder._

    def unapply(resp: ResponseContent): Option[Seq[IntegrationEntry]] = resp match {
      case JsonObjectResponse(js) if js.has("services") => Try(decodeSeq('services)(js, IntegrationEntry.Decoder)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

  def providerPath(id: ProviderId): String = s"$ProvidersPath/$id"

  case class ProviderEntry(id: ProviderId, name: String, email: EmailAddress, url: URI, description: String)

  object ProviderEntry {
    import JsonDecoder._

    implicit lazy val Decoder: JsonDecoder[ProviderEntry] = new JsonDecoder[ProviderEntry] {
      override def apply(implicit js: JSONObject): ProviderEntry =
        ProviderEntry(decodeId[ProviderId]('id), 'name, EmailAddress('email), URI.parse('uri), 'description)
    }
  }

  object ProviderEntryResponse {
    def unapply(resp: ResponseContent): Option[ProviderEntry] = resp match {
      case JsonObjectResponse(js) => Try(ProviderEntry.Decoder(js)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }
}
