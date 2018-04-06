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
package com.waz

import java.io.{File, PrintWriter}
import java.util.zip.{ZipEntry, ZipFile}

import com.waz.model.UserId
import com.waz.service.DatabaseUtils
import com.waz.utils.{IoUtils, returning}
import org.scalatest._
import com.waz.utils.IoUtils.withResource

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class DatabaseUtilsSpec extends WordSpec with BeforeAndAfterAll with BeforeAndAfterEach with MustMatchers {

  private val testUserId = UserId()
  private val testDirectory =
    new File(s"${System.getProperty("java.io.tmpdir")}/${getClass.getSimpleName}_${System.currentTimeMillis()}")

  private def createFakeDatabaseFileInsideTestDirectory(): File =
    returning(new File(testDirectory, DatabaseUtils.exportDbFileName(testUserId))) { file =>
      withResource(new PrintWriter(file)) { _.write("some content") }
  }

  def getEntries(zipFile: ZipFile): List[ZipEntry] = {
    val buffer = ArrayBuffer.empty[ZipEntry]
      while (zipFile.entries().hasMoreElements) {
        buffer.append(zipFile.entries().nextElement())
    }

    buffer.toList
  }

  override protected def beforeEach(): Unit = {
    if (!testDirectory.mkdir()) throw new RuntimeException("Cannot create directory for tests.")
  }

  override protected def afterEach(): Unit = {
    IoUtils.deleteRecursively(testDirectory)
  }

  "Exporting database" should {

    "create an export zip file with metadata and database files." in {
      val fakeDatabasePath = createFakeDatabaseFileInsideTestDirectory()
      val zipFile = Await.result(
        DatabaseUtils.exportDatabase(testUserId, userHandle = "TEST", databasePath = fakeDatabasePath, targetDir = testDirectory),
        Duration.Inf
      )

      zipFile.exists() mustBe true
      withResource(new ZipFile(zipFile)) { zip =>
        val entries = getEntries(zip)
        entries.size mustBe 2
        Option(zip.getEntry(DatabaseUtils.exportMetadataFileName)).isDefined mustBe true
        Option(zip.getEntry(DatabaseUtils.exportDbFileName(testUserId))).isDefined mustBe true
      }
    }


  }

}
