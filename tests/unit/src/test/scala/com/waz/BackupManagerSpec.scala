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
import java.util.zip.ZipFile

import com.waz.model.UserId
import com.waz.utils.IoUtils.withResource
import com.waz.utils.{IoUtils, returning}
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

class BackupManagerSpec extends WordSpec with BeforeAndAfterAll with BeforeAndAfterEach with MustMatchers {

  import com.waz.service.BackupManager._

  private val testUserId = UserId()
  private val testDirectory =
    new File(s"${System.getProperty("java.io.tmpdir")}/${getClass.getSimpleName}_${System.currentTimeMillis()}")

  private def createFakeDatabaseInsideTestDirectory(targetDirectory: File = testDirectory): File =
    returning(new File(targetDirectory, exportDbFileName(testUserId))) { file =>
      withResource(new PrintWriter(file)) { _.write("some content") }
  }

  private def createFakeDatabaseWal(targetDirectory: File = testDirectory): File =
    returning(new File(targetDirectory, exportWalFileName(testUserId))) { file =>
      withResource(new PrintWriter(file)) { _.write("some content") }
    }

  override protected def beforeEach(): Unit = {
    if (!testDirectory.mkdir()) throw new RuntimeException("Cannot create directory for tests.")
  }

  override protected def afterEach(): Unit = {
    IoUtils.deleteRecursively(testDirectory)
  }

  private def getZipFileEntryNames(zipFile: ZipFile): Set[String] = {
    val iterator = zipFile.entries()
    Stream.continually(Try(iterator.nextElement())).takeWhile(_.isSuccess).map(_.get.getName).toSet
  }

  private def getAllFileNamesInside(directory: File): Set[String] = {
    directory.listFiles().map(_.getName).toSet
  }

  "Exporting database" should {

    "create an export zip file with metadata and all database related files." in {
      val fakeDatabase = createFakeDatabaseInsideTestDirectory()
      createFakeDatabaseWal()
      val zipFile = Await.result(
        exportDatabase(testUserId, userHandle = "TEST", database = fakeDatabase, targetDir = testDirectory),
        5.seconds
      )

      withClue("Zip file should exist.") { zipFile.exists() mustBe true }
      withResource(new ZipFile(zipFile)) { zip =>
        withClue("Files inside test directory: " + getAllFileNamesInside(testDirectory)) {
          getZipFileEntryNames(zip) mustBe Set(
            exportMetadataFileName,
            exportDbFileName(testUserId),
            exportWalFileName(testUserId)
          )
        }
      }
    }

  }




}
