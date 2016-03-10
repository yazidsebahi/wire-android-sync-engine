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
package com.waz.api

/**
 * Logging to S3.
 */
trait Logging {

  def setEnabled(enabled: Boolean): Unit

  def isEnabled: Boolean

  /**
   * Log to a file that you can upload to S3. Be extra careful not to log too much here.
   */
  def log(level: LogLevel, tag: String, message: String): Unit
  def log(level: LogLevel, tag: String, message: String, event: LogCallEvent, convId: String, sessionId: String): Unit
  def log(level: LogLevel, tag: String, message: String, t: Throwable): Unit

  /**
   * Clear all current log outputs.
   */
  def clear(): Unit

  /**
   * Upload the current log file (and start a new logging session). Restricted to rather small files.
   */
  def upload(): Unit
}
