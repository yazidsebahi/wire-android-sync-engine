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
package com.waz.service

import java.io._
import java.util.zip.{ZipFile, ZipOutputStream}

import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.verbose
import com.waz.model.UserId
import com.waz.service.AccountManager.{BackupMetadata, BackupMetadataDecoder, BackupMetadataEncoder}
import com.waz.utils.IoUtils.withResource
import com.waz.utils.{IoUtils, returning}
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object DatabaseUtils {

  val exportMetadataFileName = "export.json"
  def exportDbFileName(userId: UserId) = s"${userId.str}.db"
  def exportZipFileName(userHandle: String) = s"Wire-$userHandle-Backup_${Instant.now().toString}.Android_wbu"

  def exportDatabase(userId: UserId, userHandle: String, databasePath: File, targetDir: File)(implicit ec: ExecutionContext): Future[File] = Future {
    returning(new File(targetDir, exportZipFileName(userHandle))) { zipFile =>
      zipFile.deleteOnExit()

      println("AAAAA "+zipFile.getAbsolutePath)

      withResource(new ZipOutputStream(new FileOutputStream(zipFile))) { zip =>
        withResource(new BufferedInputStream(new FileInputStream(databasePath))) {
          IoUtils.writeZipEntry(_, zip, exportDbFileName(userId))
        }

        withResource(new ByteArrayInputStream(BackupMetadataEncoder(BackupMetadata(userId)).toString.getBytes("utf8"))) {
          IoUtils.writeZipEntry(_, zip, exportMetadataFileName)
        }
      }

      verbose(s"database export finished: ${zipFile.getAbsolutePath} . Data contains: ${zipFile.length} bytes")
    }
  }

  def importDatabase(userId: UserId, exportFile: File, databasePath: File)(implicit ec: ExecutionContext): Future[File] = Future {
    withResource(new ZipFile(exportFile)) { zip =>
      val metadataEntry = Option(zip.getEntry(exportMetadataFileName)).getOrElse {
        throw new IllegalArgumentException(s"Cannot find metadata zip entry.")
      }

      val metadataStr = withResource(zip.getInputStream(metadataEntry))(Source.fromInputStream(_).mkString)
      val metadata = BackupMetadataDecoder(new JSONObject(metadataStr))

      val isMetadataValid: Boolean = true
      if (!isMetadataValid) throw new IllegalArgumentException("Metadata is invalid!")

      val dbEntry = Option(zip.getEntry(exportDbFileName(userId))).getOrElse {
        throw new IllegalArgumentException(s"Cannot find database zip entry.")
      }

      returning(new File(databasePath, exportDbFileName(userId))) { dbFile =>
        withResource(new FileOutputStream(dbFile)) { out =>
          withResource(zip.getInputStream(dbEntry)) { in =>
            IoUtils.write(in, out)
          }
        }
      }
    }
  }


}
