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

import java.io._
import java.util.Calendar

import com.waz.ZLog.LogTag

import scala.collection.mutable

object InternalLog {
  sealed trait LogLevel
  case object Error   extends LogLevel { override def toString = "E" }
  case object Warn    extends LogLevel { override def toString = "W" }
  case object Info    extends LogLevel { override def toString = "I" }
  case object Debug   extends LogLevel { override def toString = "D" }
  case object Verbose extends LogLevel { override def toString = "V" }

  private val outputs = mutable.HashMap[String, LogOutput]()

  def getOutputs = outputs.values.toList

  def reset() = this.synchronized {
    outputs.values.foreach( _.close )
    outputs.clear
  }

  def flush() = outputs.values.foreach( _.flush )

  def get(id: String) = outputs.get(id)

  def add(output: LogOutput) = this.synchronized {
    get(output.id).getOrElse {
      outputs += (output.id -> output)
      output
    }
  }

  def remove(output: LogOutput) = this.synchronized { outputs.remove(output.id) match {
    case Some(output) => output.close()
    case _ =>
  } }

  def addAndroidLog() = add(new AndroidLogOutput)
  def addBufferedLog(fileName: String, bufferSize: Long = 1024L * 1024L) = add(new BufferedLogOutput(fileName, bufferSize))

  def init(basePath: String) = {
    addAndroidLog()
    addBufferedLog(basePath + "/internalLog.log")
  }

  def error(msg: String, cause: Throwable, tag: LogTag) = log(msg, cause, Error, tag)
  def error(msg: String, tag: LogTag)                   = log(msg, Error, tag)
  def warn(msg: String, cause: Throwable, tag: LogTag)  = log(msg, cause, Warn, tag)
  def warn(msg: String, tag: LogTag)                    = log(msg, Warn, tag)
  def info(msg: String, tag: LogTag)                    = log(msg, Info, tag)
  def debug(msg: String, tag: LogTag)                   = log(msg, Debug, tag)
  def verbose(msg: String, tag: LogTag)                 = log(msg, Verbose, tag)

  def stackTrace(cause: Throwable) = {
    val result = new StringWriter()
    val printWriter = new PrintWriter(result)
    cause.printStackTrace(printWriter)
    result.toString
  }

  def dateTag = {
    val cal = Calendar.getInstance
    StringBuilder.newBuilder
      .append(cal.get(Calendar.YEAR)).append('-')
      .append(twoc(cal.get(Calendar.MONTH)+1)).append('-')
      .append(twoc(cal.get(Calendar.DAY_OF_MONTH))).append('_')
      .append(twoc(cal.get(Calendar.HOUR_OF_DAY))).append(':')
      .append(twoc(cal.get(Calendar.MINUTE))).append(':')
      .append(twoc(cal.get(Calendar.SECOND))).append('.')
      .append(threec(cal.get(Calendar.MILLISECOND))).toString
  }

  private final def twoc(n: Int) =
    if(n < 10) "0"+n.toString
    else n.toString

  private final def threec(n: Int) =
    if (n < 10) "00"+n.toString
    else if (n < 100) "0" + n.toString
    else n.toString

  private def log(msg: String, level: LogLevel, tag: LogTag): Unit = outputs.values.foreach { _.log(msg, level, tag) }
  private def log(msg: String, cause: Throwable, level: LogLevel, tag: LogTag): Unit = outputs.values.foreach { _.log(msg, cause, level, tag) }
}