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
package com.waz.specs

import com.waz.ZLog
import com.waz.ZLog.LogLevel
import com.waz.utils.isTest
import com.waz.utils.wrappers.{Intent, JVMIntentBuilder, JavaURIUtil, URI}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait AndroidFreeSpec extends BeforeAndAfterAll { this: Suite =>

  //Ensures that Android wrappers are assigned with a non-Android implementation so that tests can run on the JVM
  override protected def beforeAll() = {
    isTest = true
    ZLog.testLogLevel = LogLevel.Verbose
    URI    = JavaURIUtil
    Intent = JVMIntentBuilder
  }

}
