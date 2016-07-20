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
package com.waz.utils

import android.content.Context
import android.net.Uri
import android.provider.OpenableColumns._
import com.waz.ZLog._
import com.waz.content.Mime
import com.waz.threading.Threading

import scala.concurrent.Future
import scala.util.Try

object ContentURIs {
  private implicit def logTag: LogTag = logTagFor(ContentURIs)

  def queryContentUriMetaData(context: Context, uri: Uri): Future[MetaData] = Future {
    def mimeFromResolver = LoggedTry(Option(context.getContentResolver.getType(uri))).toOption.flatten.map(Mime(_)).filterNot(_.isEmpty)
    def mimeFromExtension = Option(uri.getLastPathSegment).map(Mime.fromFileName).filterNot(_.isEmpty)
    def mime = mimeFromResolver orElse mimeFromExtension getOrElse Mime.Default

    verbose(s"queryContentUriInfo($uri) - mimeFromResolver: $mimeFromResolver, mimeFromExtension: $mimeFromExtension")

    def nameFromUri = Option(uri.getLastPathSegment).filterNot(_.isEmpty)

    Option(context.getContentResolver.query(uri, Array(DISPLAY_NAME, SIZE), null, null, null)).filter(_.moveToFirst()) map { cursor =>
      def nameFromProvider = Try(Option(cursor.getString(0))).toOption.flatten.filterNot(_.isEmpty)
      def sizeFromProvider = Try(Option(cursor.getLong(1))).toOption.flatten.filter(_ >= 0L)

      MetaData(mime, nameFromProvider orElse nameFromUri, sizeFromProvider)
    } getOrElse MetaData(mime, nameFromUri, None)
  }(Threading.BlockingIO)

  case class MetaData(mime: Mime, name: Option[String], size: Option[Long])
}
