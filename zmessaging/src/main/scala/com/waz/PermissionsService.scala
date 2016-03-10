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
package com.waz

import java.util

import android.content.Context
import android.support.v4.content.ContextCompat
import com.waz.api.Permission.Status
import com.waz.api.Permission.Status._
import com.waz.api.{Permission, PermissionProvider}
import com.waz.threading.Threading
import com.waz.ZLog._
import com.waz.utils.events.{EventContext, Signal}

import scala.collection.JavaConverters._
import scala.concurrent.{Promise, Future}

class PermissionsService(context: Context) {
  private val logTag: LogTag = logTagFor[PermissionsService]
  private val providerSignal = Signal(Option.empty[PermissionProvider])

  def setProvider(p: PermissionProvider): Unit = {
    Threading.assertUiThread()
    providerSignal.publish(Some(p), Threading.Ui)
  }

  def clearProvider(): Unit = {
    Threading.assertUiThread()
    providerSignal.publish(None, Threading.Ui)
  }

  def isGranted(p: Permission): Boolean = ContextCompat.checkSelfPermission(context, p.id) == GRANTED.id

  def request(ps: Set[Permission], delayUntilProviderIsSet: Boolean): Future[Set[Permission]] = {
    val (alreadyGranted, remaining) = ps.partition(isGranted)

    if (remaining.isEmpty) Future.successful(alreadyGranted)
    else {
      val promisedResponse = Promise[Set[Permission]]

      val sub = providerSignal.on(Threading.Ui) {
        case Some(provider) =>
          provider.requestPermissions(remaining.asJava, new provider.ResponseHandler {
            override def handleResponse(r: util.Map[Permission, Status]): Unit = promisedResponse.trySuccess((r.asScala.keysIterator.filter(k => r.get(k) == GRANTED) ++ alreadyGranted).toSet)
          })
        case None =>
          if (! delayUntilProviderIsSet) promisedResponse.trySuccess(alreadyGranted)
      } (EventContext.Global)

      promisedResponse.future.andThen {
        case _ => sub.destroy()
      } (Threading.Background)
    }
  }

  def requiring[A](ps: Set[Permission], delayUntilProviderIsSet: Boolean)(ifDenied: => Future[A], ifGranted: => Future[A]): Future[A] =
    request(ps, delayUntilProviderIsSet).flatMap(granted => if (ps forall granted) ifGranted else ifDenied)(Threading.Background)
}
