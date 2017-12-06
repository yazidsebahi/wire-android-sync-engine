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
package com.waz.utils.events

import android.content.Context
import android.database.ContentObserver
import android.net.Uri
import com.waz.PermissionsService
import com.waz.api.Permission
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.returning
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.ref.WeakReference
import com.waz.ZLog.ImplicitTag._

class ContentObserverSignal(uri: Uri, permissions: PermissionsService, notifyForDescendents: Boolean = false)(implicit context: Context) extends SourceSignal[Option[Instant]](Some(None)) {
  @volatile private var observer = Option.empty[Forwarder]
  @volatile private var requestingPermissions = CancellableFuture.successful({})

  override protected def onWire(): Unit = {
    super.onWire()

    permissions.requiring(Set(Permission.READ_CONTACTS), delayUntilProviderIsSet = true)(Future.successful({}), returning(CancellableFuture {
      observer = Some(returning(new Forwarder(new WeakReference(this)))(o => context.getContentResolver.registerContentObserver(uri, notifyForDescendents, o)))
    } (Threading.Background))(requestingPermissions = _))
  }

  override protected def onUnwire(): Unit = {
    super.onUnwire()
    unsubscribe()
  }

  private def unsubscribe(): Unit = {
    requestingPermissions.cancel()
    observer foreach { o =>
      observer = None
      context.getContentResolver.unregisterContentObserver(o)
    }
  }

  override def finalize(): Unit = unsubscribe()
}

private class Forwarder(ref: WeakReference[SourceSignal[Option[Instant]]]) extends ContentObserver(null) {
  override def onChange(selfChange: Boolean): Unit = ref.get.foreach(_ ! Some(Instant.now))
}
