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

import java.io.File

import com.waz.api.ZmsVersion
import com.waz.specs.AndroidFreeSpec

class InternalLogSpec extends AndroidFreeSpec {
  val tag = "InternalLogSuite"
  val tempDir = System.getProperty("java.io.tmpdir")+"tmp"+System.nanoTime()

  def tempFileName = tempDir + "/" + System.nanoTime() + ".log"

  def overflow(log: BufferedLogOutput) = {
    var prevSize = -1L
    while (log.bufferSize > log.size && log.size > prevSize) {
      prevSize = log.size
      InternalLog.debug("!", tag)
    }
    Thread.sleep(500L) // give the buffer some time to flush
  }

  def exists(fileName: String) = new File(fileName).exists()

  before {
    InternalLog.reset()

    val file = new File(tempDir)
    if (!file.exists()) {
      file.mkdir()
    }
  }

  after {
    InternalLog.reset()

    val file = new File(tempDir)
    if (file.exists()) {
      file.delete()
    }
  }

  feature("adding and removing log outputs") {
    scenario("adds and removes a buffered log output") {
      InternalLog.getOutputs.size shouldEqual(0)
      val fileName = tempFileName
      InternalLog.addBufferedLog(fileName)
      InternalLog.getOutputs.size shouldEqual(1)

      val log = InternalLog.get(fileName).getOrElse(fail(s"No log output: $fileName"))
      log.isInstanceOf[BufferedLogOutput] shouldEqual(true)

      InternalLog.remove(log)
      InternalLog.getOutputs.size shouldEqual(0)
    }

    scenario("adds and removes an Android log output") {
      InternalLog.getOutputs.size shouldEqual(0)
      InternalLog.addAndroidLog()
      InternalLog.getOutputs.size shouldEqual(1)

      val log = InternalLog.get("android").getOrElse(fail(s"No log output for Android"))
      log.isInstanceOf[AndroidLogOutput] shouldEqual(true)

      InternalLog.remove(log)
      InternalLog.getOutputs.size shouldEqual(0)
    }

    scenario("reset log outputs") {
      InternalLog.getOutputs.size shouldEqual(0)
      InternalLog.addBufferedLog(tempFileName)
      InternalLog.addAndroidLog()
      InternalLog.getOutputs.size shouldEqual(2)
      InternalLog.reset()
      InternalLog.getOutputs.size shouldEqual(0)
    }
  }

  feature("writing logs to the buffer") {
    scenario("creates an empty buffer") {
      val log = InternalLog.addBufferedLog(tempFileName).asInstanceOf[BufferedLogOutput]
      log.empty shouldEqual(true)
    }

    scenario("appends to the buffer") {
      val log = InternalLog.addBufferedLog(tempFileName).asInstanceOf[BufferedLogOutput]
      log.empty should equal(true)
      InternalLog.debug("something", tag)
      log.empty shouldEqual(false)
    }

    scenario("clears the buffer when full") {
      val log = InternalLog.addBufferedLog(tempFileName, 128L).asInstanceOf[BufferedLogOutput]
      log.empty shouldEqual(true)

      InternalLog.debug("!", tag)
      log.empty shouldEqual(false)

      overflow(log)

      log.empty shouldEqual(true)
    }
  }

  feature("writing logs to the file") {
    scenario("creates a log file") {
      val fileName = tempFileName
      exists(fileName) shouldEqual(false)

      val log = InternalLog.addBufferedLog(fileName, 128L).asInstanceOf[BufferedLogOutput]
      exists(fileName) shouldEqual(false)

      overflow(log)

      exists(fileName) shouldEqual(true)
    }

    scenario("appends to the log file when the buffer is full") {
      val log = InternalLog.addBufferedLog(tempFileName, 128L).asInstanceOf[BufferedLogOutput]
      overflow(log)
      val fileSize1 = new File(log.fileName).length()
      overflow(log)
      val fileSize2 = new File(log.fileName).length()
      fileSize2 > fileSize1 shouldEqual(true)
    }
  }

  feature("connecting with ZLog") {

    scenario("receives logs written to ZLog") {
      ZmsVersion.DEBUG shouldEqual(true)
      val log = InternalLog.addBufferedLog(tempFileName).asInstanceOf[BufferedLogOutput]
      log.empty shouldEqual(true)

      import com.waz.ZLog._
      import com.waz.ZLog.ImplicitTag._

      verbose("something")

      log.empty shouldEqual(false)
    }
  }

}
