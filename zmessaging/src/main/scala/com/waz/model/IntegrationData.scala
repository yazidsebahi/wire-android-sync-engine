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
package com.waz.model

import com.waz.utils.JsonDecoder
import org.json.JSONObject

case class IntegrationAsset(assetType: String, id: AssetId)

case class IntegrationData(id: IntegrationId,
                           provider: ProviderId,
                           name: String,
                           summary: String,
                           description: String,
                           assets: Seq[IntegrationAsset],
                           tags: Seq[String],
                           enabled: Boolean)

object IntegrationData {
  import JsonDecoder._

  implicit lazy val AssetDecoder: JsonDecoder[IntegrationAsset] = new JsonDecoder[IntegrationAsset] {
    override def apply(implicit js: JSONObject): IntegrationAsset = IntegrationAsset('type, decodeId[AssetId]('key))
  }

  implicit lazy val Decoder: JsonDecoder[IntegrationData] = new JsonDecoder[IntegrationData] {
    override def apply(implicit js: JSONObject): IntegrationData =
      IntegrationData(
        decodeId[IntegrationId]('id),
        decodeId[ProviderId]('provider),
        decodeString('name),
        decodeString('summary),
        decodeString('description),
        decodeSeq[IntegrationAsset]('assets),
        decodeStringSeq('tags),
        decodeBool('enabled)
      )
  }
}
