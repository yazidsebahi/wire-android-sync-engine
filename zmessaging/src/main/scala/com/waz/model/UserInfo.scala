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

import scala.util.Try

case class UserInfo(id: UserId, name: Option[String] = None, accentId: Option[Int] = None, email: Option[EmailAddress] = None, phone: Option[PhoneNumber] = None, picture: Option[AssetData] = None, trackingId: Option[TrackingId] = None, deleted: Boolean = false)

object UserInfo {
  import JsonDecoder._

  implicit object Decoder extends JsonDecoder[UserInfo] {

    def imageData(userId: UserId, js: JSONObject) = {
      val mime = decodeString('content_type)(js)
      val size = decodeInt('content_length)(js)
      val data = decodeOptString('data)(js)
      val id = RAssetId(decodeString('id)(js))
      implicit val info = js.getJSONObject("info")

      AssetData(
        status = UploadDone,
        sizeInBytes = size,
        mime = Mime(mime),
        metaData = Some(AssetMetaData.Image(Dim2('width, 'height), 'tag)),
        data = data.map(AssetData.decodeData),
        convId = Some(RConvId(userId.str)), //v2 asset needs user conv for downloading
        v2ProfileId = Some(id)
      )

    }

    def getAssets(implicit js: JSONObject): Option[AssetData] = fromArray(js, "assets") flatMap { assets =>
      Seq.tabulate(assets.length())(assets.getJSONObject).map { js =>
        AssetData(
          remoteId = decodeOptRAssetId('key)(js),
          metaData = Some(AssetMetaData.Image(Dim2(0, 0), decodeString('size)(js)))
        )
        //TODO Dean - do we want to bring back small pictures for users?
      }.find { case AssetData.IsImage(_, tag) => tag == "complete"; case _ => false }
    }

    def getPicture(userId: UserId)(implicit js: JSONObject): Option[AssetData] = fromArray(js, "picture") flatMap { pic =>
      val id = decodeOptString('correlation_id)(pic.getJSONObject(0).getJSONObject("info")).fold(AssetId())(AssetId(_))
      val pics = Seq.tabulate(pic.length())(i => imageData(userId, pic.getJSONObject(i)))

      //TODO Dean - do we want to bring back small pictures for users?
      val medium = pics.find { case AssetData.IsImage(_, tag) => tag == "medium"; case _ => false }.map(_.copy(id = id))
      medium
    }

    private def fromArray(js: JSONObject, name: String) = Try(js.getJSONArray(name)).toOption.filter(_.length() > 0)

    override def apply(implicit js: JSONObject): UserInfo = {
      val accentId = decodeOptInt('accent_id).orElse {
        decodeDoubleSeq('accent) match {
          case Seq(r, g, b, a) => Some(AccentColor(r, g, b, a).id)
          case _ => None
        }
      }
      val id = UserId('id)
      //prefer v3 ("assets") over v2 ("picture") - this will prevent unnecessary uploading of v3 if a v2 also exists
      val pic = getAssets.orElse(getPicture(id))
      UserInfo(id, 'name, accentId, 'email, 'phone, pic, decodeOptString('tracking_id) map (TrackingId(_)), deleted = 'deleted)
    }
  }

  def encodePicture(assets: Seq[AssetData]): JSONArray = {
    val arr = new json.JSONArray()
      assets.collect {
        case a@AssetData.IsImage(Dim2(w, h), tag) =>
          JsonEncoder { o =>
            o.put("id", a.v2ProfileId.map(_.str).getOrElse(""))
            o.put("content_type", a.mime.str)
            o.put("content_length", a.size)
            a.data64.foreach(o.put("data", _))
            o.put("info", JsonEncoder { info =>
              info.put("tag", tag)
              info.put("width", w)
              info.put("height", h)
              info.put("original_width", w)
              info.put("original_height", h)
              info.put("correlation_id", a.id.str)
              info.put("nonce", a.id.str)
              info.put("public", true)
            })
          }
      }.foreach(arr.put)
    arr
  }


  def encodeAsset(assets: Seq[AssetData]): JSONArray = {
    val arr = new json.JSONArray()
    assets.collect {
      case AssetData.WithRemoteId(rId) =>
        JsonEncoder { o =>
          o.put("size", "complete")
          o.put("key", rId.str)
          o.put("type", "image")
        }
    }.foreach(arr.put)
    arr
  }

  implicit lazy val Encoder: JsonEncoder[UserInfo] = new JsonEncoder[UserInfo] {
    override def apply(info: UserInfo): JSONObject = JsonEncoder { o =>
      o.put("id", info.id.str)
      info.name.foreach(o.put("name", _))
      info.phone.foreach(p => o.put("phone", p.str))
      info.email.foreach(e => o.put("email", e.str))
      info.accentId.foreach(o.put("accent_id", _))
      info.trackingId.foreach(id => o.put("tracking_id", id.str))
      o.put("assets", encodeAsset(info.picture.toSeq))
      o.put("picture", encodePicture(info.picture.toSeq))
    }
  }

  implicit lazy val ContentEncoder: ContentEncoder[UserInfo] = JsonContentEncoder.map { (info: UserInfo) =>
    JsonEncoder { o =>
      info.name.foreach(o.put("name", _))
      info.accentId.foreach(o.put("accent_id", _))
      o.put("assets", encodeAsset(info.picture.toSeq))
      o.put("picture", encodePicture(info.picture.toSeq))
    }
  }
}
