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
package com.waz.log

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.waz.api.ZmsVersion
import com.waz.specs.AndroidFreeSpec
import com.waz.utils.IoUtils

import scala.util.Random

class InternalLogSpec extends AndroidFreeSpec {
  val tag = "InternalLogSuite"
  val tempDir = System.getProperty("java.io.tmpdir")+"tmp"+System.nanoTime()

  def tempBasePath = tempDir + "/" + System.nanoTime()

  def filesNumberInTempDir = new File(tempDir).listFiles().length

  def overflow(log: BufferedLogOutput) = {
    var prevSize = -1L
    while (log.maxBufferSize > log.size && log.size > prevSize) {
      prevSize = log.size
      InternalLog.debug(Random.nextPrintableChar().toString, tag)
      Thread.sleep(100L) // simulating much longer logs and much bigger buffers
    }
  }

  def exists(fileName: String) = new File(fileName).exists

  def delete(f: File) : Unit = if (f.exists) {
    if (f.isDirectory) f.listFiles().foreach(delete)
    f.delete()
  }

  def read(path: String) = {
    val file = new File(path)
    val sb = StringBuilder.newBuilder
    IoUtils.withResource(new BufferedReader(new InputStreamReader(new FileInputStream(file)))) {
      reader => Iterator.continually(reader.readLine()).takeWhile(_ != null).foreach(line => sb.append(line).append('\n'))
    }
    sb.toString
  }

  before {
    InternalLog.reset()

    val file = new File(tempDir)
    delete(file)
    file.mkdir()
  }

  after {
    InternalLog.reset()
    delete(new File(tempDir))
  }

  feature("adding and removing log outputs") {
    scenario("adds and removes a buffered log output") {
      InternalLog.getOutputs.size shouldEqual(0)
      val basePath = tempBasePath
      InternalLog.add(new BufferedLogOutput(basePath))
      InternalLog.getOutputs.size shouldEqual(1)

      val log = InternalLog.get(basePath).getOrElse(fail(s"No log output: $basePath"))
      log.isInstanceOf[BufferedLogOutput] shouldEqual(true)

      InternalLog.remove(log)
      InternalLog.getOutputs.size shouldEqual(0)
    }

    scenario("adds and removes an Android log output") {
      InternalLog.getOutputs.size shouldEqual(0)
      InternalLog.add(new AndroidLogOutput)
      InternalLog.getOutputs.size shouldEqual(1)

      val log = InternalLog.get("android").getOrElse(fail(s"No log output for Android"))
      log.isInstanceOf[AndroidLogOutput] shouldEqual(true)

      InternalLog.remove(log)
      InternalLog.getOutputs.size shouldEqual(0)
    }

    scenario("reset log outputs") {
      InternalLog.getOutputs.size shouldEqual(0)
      InternalLog.add(new BufferedLogOutput(tempBasePath))
      InternalLog.add(new AndroidLogOutput)
      InternalLog.getOutputs.size shouldEqual(2)
      InternalLog.reset()
      InternalLog.getOutputs.size shouldEqual(0)
    }
  }

  feature("writing logs to the buffer") {
    scenario("creates an empty buffer") {
      val log = new BufferedLogOutput(tempBasePath)
      InternalLog.add(log)
      log.empty shouldEqual(true)
    }

    scenario("appends to the buffer") {
      val log = new BufferedLogOutput(tempBasePath)
      InternalLog.add(log)
      log.empty should equal(true)
      InternalLog.debug("something", tag)
      log.empty shouldEqual(false)
    }

    scenario("clears the buffer when full") {
      val log = new BufferedLogOutput(tempBasePath, 128L)
      InternalLog.add(log)
      log.empty shouldEqual(true)

      InternalLog.debug("!", tag)
      log.empty shouldEqual(false)

      overflow(log)

      log.empty shouldEqual(true)
    }
  }

  feature("writing logs to the file") {
    scenario("creates a log file") {
      val basePath = tempBasePath
      filesNumberInTempDir shouldEqual(0)

      val log = new BufferedLogOutput(basePath, 128L)
      InternalLog.add(log)
      filesNumberInTempDir shouldEqual(0)

      overflow(log)

      filesNumberInTempDir shouldEqual(1)
    }

    scenario("appends to the log file when the buffer is full") {
      val log = new BufferedLogOutput(tempBasePath, 128L)
      InternalLog.add(log)
      overflow(log)
      val fileSize1 = new File(log.currentPath).length()
      overflow(log)
      val fileSize2 = new File(log.currentPath).length()
      fileSize2 > fileSize1 shouldEqual(true)
    }
  }

  feature("connecting with ZLog") {

    scenario("receives logs written to ZLog") {
      ZmsVersion.DEBUG shouldEqual(true)
      val log = new BufferedLogOutput(tempBasePath)
      InternalLog.add(log)
      log.empty shouldEqual(true)

      import com.waz.ZLog._
      import com.waz.ZLog.ImplicitTag._

      verbose("something")

      log.empty shouldEqual(false)
    }
  }

  feature("log file rolling") {
    val maxBufferSize = 128L
    val maxFileSize = maxBufferSize * 4L

    scenario("creates a new file when the max file size limit is exceeded") {
      val log = new BufferedLogOutput(tempBasePath, maxBufferSize, maxFileSize)
      InternalLog.add(log)
      exists(log.currentPath) shouldEqual(false)
      filesNumberInTempDir shouldEqual(0)

      overflow(log)
      exists(log.currentPath) shouldEqual(true)
      filesNumberInTempDir shouldEqual(1)

      val firstPath = log.currentPath

      (1 to 4).foreach( _ => overflow(log) )

      exists(log.currentPath) shouldEqual(true)
      filesNumberInTempDir shouldEqual(2)
      log.currentPath should not equal(firstPath)
    }

    scenario("it's ok for the file to be a bit bigger than the limit") {
      val log = new BufferedLogOutput(tempBasePath, maxBufferSize, maxFileSize)
      InternalLog.add(log)

      val firstPath = log.currentPath

      (1 to 5).foreach( _ => overflow(log) )
      filesNumberInTempDir shouldEqual(2)

      val fileLen = new File(firstPath).length()

      (fileLen > maxFileSize) shouldEqual(true)
      (fileLen < 2L * maxFileSize) shouldEqual(true)
    }

    scenario("returns all file paths") {
      val log = new BufferedLogOutput(tempBasePath, maxBufferSize, maxFileSize)
      InternalLog.add(log)
      
      (1 to 5).foreach( _ => overflow(log) )
      if (new File(log.currentPath).exists()) { // the current file may not yet exist (if there was no flush)
        log.getPaths.size shouldEqual (filesNumberInTempDir)
      } else {
        log.getPaths.size shouldEqual (filesNumberInTempDir) + 1
      }

      (1 to 5).foreach( _ => overflow(log) )
      val paths = if (new File(log.currentPath).exists()) log.getPaths else log.getPaths.tail
      paths.size shouldEqual (filesNumberInTempDir)
      val pathsSorted = paths.sortBy(p => new File(p).lastModified())
      paths shouldEqual(pathsSorted)
    }

    scenario("deletes the oldest file and rolls the rest") {
      val maxRollFiles = 4
      val log = new BufferedLogOutput(tempBasePath, maxBufferSize, maxFileSize, maxRollFiles)
      InternalLog.add(log)

      while (filesNumberInTempDir < maxRollFiles) overflow(log)
      log.getPaths.size shouldEqual (filesNumberInTempDir)

      val oldPaths = log.getPaths

      oldPaths.size shouldEqual(maxRollFiles)
      val oldFiles = oldPaths.map(read)
      for (i <- 0 to oldFiles.size - 2) {
        oldFiles(i) should not equal oldFiles(i + 1)
      }

      (1 to 6).foreach( _ => overflow(log) )
      log.getPaths.size shouldEqual (filesNumberInTempDir)

      val newPaths = log.getPaths
      val newFiles = newPaths.map(read)
      for (i <- 0 to newFiles.size - 2) {
        newFiles(i) should not equal newFiles(i + 1)
      }

      newPaths.size shouldEqual(maxRollFiles)
      oldPaths shouldEqual(newPaths)

      for (i <- 0 to newFiles.size - 3) {
        oldFiles(i+1) shouldEqual(newFiles(i))
      }
    }
  }

}
