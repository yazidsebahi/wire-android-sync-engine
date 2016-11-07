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

import com.waz.api.impl.AccentColor
import com.waz.model.AssetStatus.UploadDone
import com.waz.utils.{JsonDecoder, JsonEncoder}
import com.waz.znet.ContentEncoder
import com.waz.znet.ContentEncoder.JsonContentEncoder
import org.json
import org.json.{JSONArray, JSONObject}
import com.waz.utils.returning

case class UserInfo(id: UserId, name: Option[String] = None, accentId: Option[Int] = None, email: Option[EmailAddress] = None, phone: Option[PhoneNumber] = None, picture: Option[AssetData] = None, trackingId: Option[TrackingId] = None, deleted: Boolean = false)

object UserInfo {
  import JsonDecoder._

  implicit object Decoder extends JsonDecoder[UserInfo] {

    def imageData(js: JSONObject) = {
      val mime = decodeString('content_type)(js)
      val size = decodeInt('content_length)(js)
      val data = decodeOptString('data)(js)
      val id = RAssetId(decodeString('id)(js))
      implicit val info = js.getJSONObject("info")

      AssetData(
        status = UploadDone(AssetKey(Some(id))),
        sizeInBytes = size,
        mime = Mime(mime),
        metaData = Some(AssetMetaData.Image(Dim2('width, 'height), 'tag)),
        data64 = data
      )

    }

    def picture(implicit js: JSONObject): AssetData = {
      val pic = js.getJSONArray("picture")
      if (pic.length() == 0) AssetData()
      else {
        val versions = Seq.tabulate(pic.length())(i => imageData(pic.getJSONObject(i)))
        val id = decodeOptString('correlation_id)(pic.getJSONObject(0).getJSONObject("info")).fold(AssetId())(AssetId(_))
        AssetData(id, status = UploadDone(AssetKey(Some(decodeId[RAssetId]('id)))), versions)
      }
    }

    override def apply(implicit js: JSONObject): UserInfo = {
      val accentId = decodeOptInt('accent_id).orElse {
        decodeDoubleSeq('accent) match {
          case Seq(r, g, b, a) => Some(AccentColor(r, g, b, a).id)
          case _ => None
        }
      }

      UserInfo('id, 'name, accentId, 'email, 'phone,
        if (js.has("picture")) Some(picture) else None,
        decodeOptString('tracking_id) map (TrackingId(_)),
        deleted = 'deleted
      )
    }
  }

  def encodeImage(data: AssetData): JSONArray =
    returning(new json.JSONArray()) { arr =>
      data.versions foreach { im =>
        arr.put(JsonEncoder { o =>
          o.put("content_type", im.mime)
          o.put("content_length", im.size)
          im.data64.foreach(o.put("data", _))
          im.remoteId.foreach(id => o.put("id", id.str))
          o.put("info", JsonEncoder { info =>
            info.put("tag", im.tag)
            info.put("width", im.width)
            info.put("height", im.height)
            info.put("original_width", im.origWidth)
            info.put("original_height", im.origHeight)
            info.put("correlation_id", data.id.str)
            info.put("nonce", data.id.str)
            info.put("public", true)
          })
        })
      }
    }

  implicit lazy val Encoder: JsonEncoder[UserInfo] = new JsonEncoder[UserInfo] {
    override def apply(info: UserInfo): JSONObject = JsonEncoder { o =>
      o.put("id", info.id.str)
      info.name.foreach(o.put("name", _))
      info.phone.foreach(p => o.put("phone", p.str))
      info.email.foreach(e => o.put("email", e.str))
      info.accentId.foreach(o.put("accent_id", _))
      info.trackingId.foreach(id => o.put("tracking_id", id.str))
      info.picture.foreach(pic => o.put("picture", encodeImage(pic)))
    }
  }

  implicit lazy val ContentEncoder: ContentEncoder[UserInfo] = JsonContentEncoder.map { (info: UserInfo) =>
    JsonEncoder { o =>
      info.name.foreach(o.put("name", _))
      info.accentId.foreach(o.put("accent_id", _))
      info.picture.foreach(pic => o.put("picture", encodeImage(pic)))
    }
  }
}
