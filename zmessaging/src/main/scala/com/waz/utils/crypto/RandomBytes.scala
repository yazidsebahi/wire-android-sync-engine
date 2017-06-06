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
package com.waz.utils.crypto

import java.security.SecureRandom

import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._

class RandomBytes {
  RandomBytes.ensureLibraryLoaded()

  // used as fallback if sodium fails to generate random data
  lazy val random = new SecureRandom()

  /**
    * Generates random byte array using libsodium
    * @param count - number of bytes to generate
    * @return - random bytes array
    */
  def apply(count: Int) : Array[Byte] = {
    val buffer = Array.ofDim[Byte](count)
    if (!randomBytes(buffer, count)) {
      warn(s"Libsodium failed to generate $count random bytes. Falling back to SecureRandom")
      random.nextBytes(buffer)
    }
    buffer
  }

  @native
  protected def randomBytes(buffer: Array[Byte], count: Int) : Boolean
}

object RandomBytes {

  private val loadLibrary = { System.loadLibrary("randombytes") }

  private def ensureLibraryLoaded() = loadLibrary

}
