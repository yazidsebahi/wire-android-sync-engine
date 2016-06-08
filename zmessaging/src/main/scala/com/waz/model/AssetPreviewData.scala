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

import com.waz.utils.{JsonDecoder, JsonEncoder}
import org.json.JSONObject

sealed abstract class AssetPreviewData(val jsonTypeTag: Symbol)
object AssetPreviewData {
  implicit lazy val AssetPreviewDataEncoder: JsonEncoder[AssetPreviewData] = new JsonEncoder[AssetPreviewData] {
    override def apply(data: AssetPreviewData): JSONObject = JsonEncoder { o =>
      o.put("type", data.jsonTypeTag.name)
      data match {
        case Empty => // nothing to add
        case Image(img) =>
          o.put("img", JsonEncoder.encode(img))
      }
    }
  }

  implicit lazy val AssetPreviewDataDecoder: JsonDecoder[AssetPreviewData] = new JsonDecoder[AssetPreviewData] {
    import JsonDecoder._

    override def apply(implicit o: JSONObject): AssetPreviewData = decodeSymbol('type) match {
      case 'empty => Empty
      case 'image =>
        Image(JsonDecoder[ImageData]('img))
      case other =>
        throw new IllegalArgumentException(s"unsupported meta data type: $other")
    }
  }

  case class Image(img: ImageData) extends AssetPreviewData('image)
  case object Empty extends AssetPreviewData('empty)
}
