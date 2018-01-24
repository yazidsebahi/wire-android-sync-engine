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

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.model._
import com.waz.sync.client.ConversationsClient.ConversationsPath
import com.waz.threading.Threading
import com.waz.utils.{Json, JsonDecoder}
import com.waz.znet.Response.SuccessHttpStatus
import com.waz.znet.ZNetClient.ErrorOrResponse
import com.waz.znet._

import scala.util.Try

trait IntegrationsClient {
  def searchIntegrations(startWith: String): ErrorOrResponse[Seq[IntegrationData]]
  def getIntegration(providerId: ProviderId, integrationId: IntegrationId): ErrorOrResponse[IntegrationData]
  def getProvider(id: ProviderId): ErrorOrResponse[ProviderData]

  def addBot(rConvId: RConvId, pId: ProviderId, iId: IntegrationId): ErrorOrResponse[ConversationEvent]
  def removeBot(rConvId: RConvId, botId: UserId): ErrorOrResponse[ConversationEvent]
}

class IntegrationsClientImpl(netClient: ZNetClient) extends IntegrationsClient {
  import IntegrationsClient._
  import Threading.Implicits.Background

  def searchIntegrations(startWith: String) =
    netClient.withErrorHandling("searchIntegrations", Request.Get(integrationsSearchPath(startWith))) {
      case Response(SuccessHttpStatus(), IntegrationsSearchResponse(data), _) => data
    }

  def getIntegration(pId: ProviderId, iId: IntegrationId) =
    netClient.withErrorHandling("getIntegration", Request.Get(integrationPath(pId, iId))) {
      case Response(SuccessHttpStatus(), IntegrationResponse(data), _) => data
    }

  def getProvider(pId: ProviderId) =
    netClient.withErrorHandling("getProvider", Request.Get(providerPath(pId))) {
      case Response(SuccessHttpStatus(), ProviderResponse(data), _) => data
    }

  def addBot(rConvId: RConvId, pId: ProviderId, iId: IntegrationId) = {
    debug(s"addBot: rConvId: $rConvId, providerId: $pId, integrationId: $iId")
    netClient.withErrorHandling("addBot", Request.Post(s"$ConversationsPath/${rConvId.str}/bots", Json("provider" -> pId.str, "service" -> iId.str))) {
      case Response(SuccessHttpStatus(), AddRemoveBotResponse(data), _) => data
    }
  }

  def removeBot(rConvId: RConvId, botId: UserId) = {
    debug(s"removeBot: convId: $rConvId, botId: $botId")

    import com.waz.znet.ContentEncoder.RequestContentEncoder

    netClient.withErrorHandling("addBot", Request.Delete(s"$ConversationsPath/${rConvId.str}/bots/$botId")) {
      case Response(SuccessHttpStatus(), AddRemoveBotResponse(data), _) => data
    }
  }
}

object IntegrationsClient {
  import JsonDecoder._
  import com.waz.model.ConversationEvent.ConversationEventDecoder

  def apply(netClient: ZNetClient): IntegrationsClient = new IntegrationsClientImpl(netClient)

  val IntegrationsSearchPath = "/services"
  val DefaultTag = "tutorial"
  val ProvidersPath = "/providers"
  val IntegrationConvPath = "/conversations"

  def integrationsSearchPath(startWith: String): String =
    if (startWith.isEmpty) Request.query(IntegrationsSearchPath, "tags" -> DefaultTag)
    else Request.query(IntegrationsSearchPath, "tags" -> DefaultTag, "start" -> startWith)

  def integrationPath(providerId: ProviderId, integrationId: IntegrationId): String = s"$ProvidersPath/$providerId/services/$integrationId"

  def providerPath(id: ProviderId): String = s"$ProvidersPath/$id"

  object IntegrationsSearchResponse {
    def unapply(resp: ResponseContent): Option[Seq[IntegrationData]] = resp match {
      case JsonObjectResponse(js) if js.has("services") => Try(decodeSeq('services)(js, IntegrationData.Decoder)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

  object IntegrationResponse {
    def unapply(resp: ResponseContent): Option[IntegrationData] = resp match {
      case JsonObjectResponse(js) => Try(IntegrationData.Decoder(js)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

  object ProviderResponse {
    def unapply(resp: ResponseContent): Option[ProviderData] = resp match {
      case JsonObjectResponse(js) => Try(ProviderData.Decoder(js)).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

  object AddRemoveBotResponse {
    def unapply(resp: ResponseContent): Option[ConversationEvent] = resp match {
      case JsonObjectResponse(js) if js.has("event") => Try(ConversationEventDecoder(js.getJSONObject("event"))).toOption
      case response =>
        warn(s"Unexpected response: $response")
        None
    }
  }

}
