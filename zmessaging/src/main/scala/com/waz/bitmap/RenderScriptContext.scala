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
package com.waz.bitmap

import android.support.v8.renderscript.{RSRuntimeException, RenderScript}
import com.waz.ZLog._
import com.waz.service.ZMessaging
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events.EventContext

object RenderScriptContext {
  private implicit val ev = EventContext.Global
  implicit val dispatcher = new SerialDispatchQueue(name = "RenderScriptContext")
  private implicit val logTag: LogTag = logTagFor(RenderScriptContext)

  case object RSNotAvailableException extends Exception("RenderScript not available")

  lazy val rs = try {
    RenderScript.create(ZMessaging.context.getApplicationContext)
  } catch {
    case e: RSRuntimeException =>
      warn(s"Renderscript context could not be instantiated.", e)
      null
  }

  def apply[A](body: RenderScript => A) = dispatcher { rs } flatMap {
    case null => CancellableFuture.failed(RSNotAvailableException)
    case r => CancellableFuture.successful(body(r))
  }
}
