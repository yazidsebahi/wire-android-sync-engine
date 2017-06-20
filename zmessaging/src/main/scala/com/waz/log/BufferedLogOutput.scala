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

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Random

class BufferedLogOutput(val basePath: String,
                        val maxBufferSize: Long = BufferedLogOutput.defMaxBufferSize,
                        val maxFileSize: Long = BufferedLogOutput.defMaxFileSize,
                        val maxRollFiles: Int = BufferedLogOutput.defMaxRollFiles) extends LogOutput {
  assert(maxBufferSize < maxFileSize)
  assert(maxRollFiles > 0)

  override val id = basePath

  private implicit val dispatcher = new SerialDispatchQueue(Threading.IO, "BufferedLogOutput" + Random.nextInt().toHexString)

  private val buffer = StringBuilder.newBuilder
  private var paths = List[String](basePath + "0.log")

  def currentPath = paths.head
  def getPaths = paths.reverse // internally the first path is the youngest one, but to the outside we want to show paths from the oldest to the youngest

  override def log(str: String, level: InternalLog.LogLevel, tag: LogTag): Unit = this.synchronized {
    buffer.append(InternalLog.dateTag).append('/').append(level).append('/').append(tag).append(": ").append(str).append('\n')
    if (size > maxBufferSize) flush()
  }

  override def log(str: String, cause: Throwable, level: InternalLog.LogLevel, tag: LogTag): Unit = this.synchronized {
    buffer.append(InternalLog.dateTag).append('/').append(level).append("/").append(tag).append(": ").append(str).append('\n')
          .append(InternalLog.stackTrace(cause)).append('\n')
    if (size > maxBufferSize) flush()
  }

  override def close(): Future[Unit] = flush()

  def empty = buffer.isEmpty

  def size = buffer.length

  // TODO: In this implementation we risk that writing to the file fails and we lose the contents.
  // But if we wait for the result of writeToFile, we risk that meanwhile someone will add something
  // to the buffer and we will lose that.
  override def flush(): Future[Unit] = this.synchronized {
    if (!empty) {
      val contents = buffer.toString
      val path = currentPath
      if (new File(path).length() + size > maxFileSize) paths = (basePath + paths.size + ".log") :: paths
      returning(Future {
        writeToFile(path, contents)
        if (paths.size > maxRollFiles) {
          paths = BufferedLogOutput.roll(paths.reverse)
        }
      }) { _ => buffer.clear() }
    } else Future.successful {}
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
        writer.flush()
        writer.close()
      }
    } catch {
      case ex: IOException => ex.printStackTrace()
    }
  }

}

object BufferedLogOutput {
  val defMaxBufferSize = 256L * 1024L
  val defMaxFileSize = 4L * defMaxBufferSize
  val defMaxRollFiles = 10
  val defFileName = "internalLog"

  @tailrec
  private def roll(pathsSorted: List[String], newPaths: List[String] = Nil): List[String] = pathsSorted match {
    case first :: second :: tail =>
      new File(second).renameTo(new File(first))
      if (tail != Nil) roll(second :: tail, first :: newPaths) else first :: newPaths
    case _ => newPaths
  }

}
