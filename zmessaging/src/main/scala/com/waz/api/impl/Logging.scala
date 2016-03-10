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
package com.waz.api.impl

import java.io.{PrintWriter, StringWriter}

import com.waz.ZLog._
import com.waz.api.{LogCallEvent, LogLevel => ApiLogLevel}
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils._

class Logging(implicit ui: UiModule) extends com.waz.api.Logging with UiObservable with SignalLoading {
  override def setEnabled(enabled: Boolean): Unit = () // unused atm

  override def isEnabled: Boolean = false

  override def log(level: ApiLogLevel, tag: String, message: String): Unit = log(level, tag, message, LogCallEvent.Other, null, null)

  override def log(level: ApiLogLevel, tag: String, message: String, t: Throwable): Unit =
    log(level, tag, message + "\n" + returning(new StringWriter) { s => t.printStackTrace(new PrintWriter(s)) }.toString)

  override def log(level: ApiLogLevel, tag: String, message: String, event: LogCallEvent, convId: String, sessionId: String): Unit = debug(message)(tag) // previously uploaded to S3, unused atm

  override def clear(): Unit = () // unused atm
  override def upload(): Unit = () // unused atm
}
