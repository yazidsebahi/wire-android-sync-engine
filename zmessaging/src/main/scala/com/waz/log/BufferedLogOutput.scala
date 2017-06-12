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

import java.io.{BufferedWriter, File, FileWriter, IOException}

import com.waz.ZLog.LogTag
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.returning

import scala.concurrent.Future
import scala.util.Random

class BufferedLogOutput(val fileName: String, val bufferSize: Long) extends LogOutput {
  override val id = fileName

  private implicit val dispatcher = new SerialDispatchQueue(Threading.IO, "BufferedLogOutput" + Random.nextInt().toHexString)

  private val buffer = StringBuilder.newBuilder

  override def log(str: String, level: InternalLog.LogLevel, tag: LogTag): Unit = this.synchronized {
    buffer.append(InternalLog.dateTag).append('/').append(level).append('/').append(tag).append(": ").append(str).append('\n')
    if (size > bufferSize) flush()
  }

  override def log(str: String, cause: Throwable, level: InternalLog.LogLevel, tag: LogTag): Unit = this.synchronized {
    buffer.append(InternalLog.dateTag).append('/').append(level).append("/").append(tag).append(": ").append(str).append('\n')
          .append(InternalLog.stackTrace(cause)).append('\n')
    if (size > bufferSize) flush()
  }

  override def close(): Future[Unit] = flush()

  def empty = buffer.isEmpty

  /*
 * This part (estimating the string length in bytes) of the Wire software
 * is based on an answer posted on the StackOverflow site.
 * (https://stackoverflow.com/a/4387559/2975925)
 *
 * That work is licensed under a Creative Commons Attribution-ShareAlike 2.5 Generic License.
 * (http://creativecommons.org/licenses/by-sa/2.5)
 *
 * Contributors on StackOverflow:
 *  - finnw (https://stackoverflow.com/users/12048/finnw)
 */
  def size = buffer.length * 2L

  // TODO: In this implementation we risk that writing to the file fails and we lose the contents.
  // But if we wait for the result of writeToFile, we risk that meanwhile someone will add something
  // to the buffer and we will lose that.
  override def flush(): Future[Unit] = this.synchronized {
    if (!empty) returning(Future { writeToFile(fileName, buffer.toString) }) { _ => buffer.clear() }
    else Future.successful {}
  }

  private def writeToFile(fileName: String, contents: String): Unit = this.synchronized {
    try {
      val file = new File(fileName)
      if (!file.exists) {
        file.getParentFile.mkdirs()
        file.createNewFile()
        file.setReadable(true)
        file.setWritable(true)
      }

      returning(new BufferedWriter(new FileWriter(file, true))) { writer =>
        writer.write(contents)
        writer.newLine()
        writer.flush()
        writer.close()
      }
    } catch {
      case ex: IOException => ex.printStackTrace()
    }
  }
}
