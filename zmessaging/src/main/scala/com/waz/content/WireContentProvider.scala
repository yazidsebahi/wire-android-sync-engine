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
package com.waz.content

import android.content.{ContentProvider, ContentResolver, ContentValues, Context}
import android.database.{Cursor, MatrixCursor}
import android.net.Uri
import android.os.ParcelFileDescriptor
import android.provider.OpenableColumns
import com.waz.ZLog._
import com.waz.cache.{CacheEntryData, Expiration}
import com.waz.model.AssetId
import com.waz.service.ZMessaging
import com.waz.threading.CancellableFuture
import com.waz.threading.Threading.Implicits.Background
import com.waz.utils.returning

import scala.concurrent.Await
import scala.concurrent.duration._

class WireContentProvider extends ContentProvider {
  import WireContentProvider._

  private val AsyncTimeout = 15.seconds

  private def cache = ZMessaging.currentGlobal.cache

  private def getEntry(key: AssetId) = Await.result(cache.getEntry(key), AsyncTimeout)

  override def getType(uri: Uri): String = {
    verbose(s"getType($uri)")
    uri match {
      case CacheUriExtractor(key) =>
        returning(getEntry(key).map(_.data.mimeType.str).orNull) { tpe => verbose(s"found entry type: $tpe")}
      case _ =>
        verbose(s"content not found")
        null
    }
  }

  override def update(uri: Uri, values: ContentValues, selection: String, selectionArgs: Array[String]): Int = 0

  override def insert(uri: Uri, values: ContentValues): Uri = null

  override def delete(uri: Uri, selection: String, selectionArgs: Array[String]): Int = 0

  override def onCreate(): Boolean = {
    verbose(s"onCreate")
    true
  }

  override def query(uri: Uri, projection: Array[String], selection: String, selectionArgs: Array[String], sortOrder: String): Cursor = {
    verbose(s"query($uri)")
    val columns = Option(projection).getOrElse(Array(OpenableColumns.DISPLAY_NAME, OpenableColumns.SIZE))
    val result = new MatrixCursor(columns)

    uri match {
      case CacheUriExtractor(key) =>
        getEntry(key) foreach { entry =>
          verbose(s"found entry: $entry")
          result.addRow(columns map {
            case OpenableColumns.DISPLAY_NAME => entry.data.fileName.getOrElse("")
            case OpenableColumns.SIZE => java.lang.Long.valueOf(entry.length)
            case _ => null.asInstanceOf[AnyRef]
          })
        }
      case _ => // ignore
    }

    result
  }

  override def openFile(uri: Uri, mode: String): ParcelFileDescriptor = {
    verbose(s"openFile($uri, $mode)")
    uri match {
      case CacheUriExtractor(key) =>
        Await.result(getDecryptedEntry(key), AsyncTimeout) match {
          case Some(entry) => ParcelFileDescriptor.open(entry.copyDataToFile(), ParcelFileDescriptor.MODE_READ_ONLY)
          case None => super.openFile(uri, mode)
        }
      case _ =>
        super.openFile(uri, mode)
    }
  }

  private def getDecryptedEntry(key: AssetId) =
    cache.getEntry(key) flatMap {
      case Some(entry) if entry.data.encKey.isDefined =>
        //TODO Dean: do we need to have different keys for encrypted/unencrypted cache entries?
        cache.getOrElse(key, cache.addStream(key, entry.inputStream, entry.data.mimeType, entry.data.fileName, Some(cache.intCacheDir))(Expiration.in(12.hours))) map (Some(_))
      case res => CancellableFuture successful res
    }

  object CacheUriExtractor {
    val extractor = CacheUri.unapply(getContext) _

    def unapply(uri: Uri): Option[AssetId] = extractor(uri)
  }
}

object WireContentProvider {
  private implicit val Tag: LogTag = logTagFor[WireContentProvider]
  private val Cache = "cache"

  object CacheUri {

    def builder(context: Context) = Uri.parse(ContentResolver.SCHEME_CONTENT + "://" + context.getPackageName).buildUpon()

    def apply(key: AssetId, context: Context): Uri = builder(context).appendEncodedPath(Cache).appendPath(key.str).build()

    def apply(entry: CacheEntryData, context: Context): Uri = {
      val b = builder(context).appendEncodedPath(Cache).appendPath(entry.key.str)
      entry.fileName foreach b.appendPath
      b.build()
    }

    def unapply(context: Context)(uri: Uri): Option[AssetId] =
      if (uri.getScheme != ContentResolver.SCHEME_CONTENT || uri.getAuthority != context.getPackageName) None
      else {
        val path = uri.getPathSegments
        if (path.size() >= 2 && path.get(0) == "cache") Some(AssetId(path.get(1))) else None
      }
  }
}
