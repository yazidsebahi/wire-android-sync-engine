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
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api.Permission.Status
import com.waz.api.Permission.Status._
import com.waz.api.{Permission, PermissionProvider}
import com.waz.threading.Threading
import com.waz.utils.events.Signal

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}

class PermissionsService(context: Context) {
  private val providers = Signal(Vector.empty[PermissionProvider])
  private val providerSignal = providers.map(_.lastOption)

  def setProvider(p: PermissionProvider): Unit = {
    Threading.assertUiThread()
    verbose(s"setProvider($p)")
    providers.mutate(_ :+ p)
  }

  def clearProvider(p: PermissionProvider): Unit = {
    Threading.assertUiThread()
    verbose(s"clearProvider")
    providers.mutate(_.filter(_ != p))
  }

  def isGranted(p: Permission): Boolean = ContextCompat.checkSelfPermission(context, p.id) == GRANTED.id

  def request(ps: Set[Permission], delayUntilProviderIsSet: Boolean): Future[Set[Permission]] = {

    def getProvider = providerSignal.filter(_.isDefined || !delayUntilProviderIsSet).head

    val (alreadyGranted, remaining) = ps.partition(isGranted)

    if (remaining.isEmpty) Future.successful(alreadyGranted)
    else getProvider.flatMap {
      case None => Future successful alreadyGranted
      case Some(provider) =>
        val promisedResponse = Promise[Set[Permission]]
        provider.requestPermissions(remaining.asJava, new provider.ResponseHandler {
          override def handleResponse(r: util.Map[Permission, Status]): Unit = promisedResponse.trySuccess((r.asScala.keysIterator.filter(k => r.get(k) == GRANTED) ++ alreadyGranted).toSet)
        })
        promisedResponse.future
    } (Threading.Ui)
  }

  def requiring[A](ps: Set[Permission], delayUntilProviderIsSet: Boolean)(ifDenied: => Future[A], ifGranted: => Future[A]): Future[A] =
    request(ps, delayUntilProviderIsSet).flatMap(granted => if (ps forall granted) ifGranted else ifDenied)(Threading.Background)
}
