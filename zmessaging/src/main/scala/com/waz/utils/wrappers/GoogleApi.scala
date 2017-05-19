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
package com.waz.utils.wrappers

import android.app.Activity
import com.google.android.gms.common.{ConnectionResult, GoogleApiAvailability}
import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.google.firebase.iid.FirebaseInstanceId
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog.warn
import com.waz.model.PushToken
import com.waz.service.BackendConfig
import com.waz.utils.LoggedTry
import com.waz.utils.events.Signal
import com.waz.utils.wrappers.GoogleApi.RequestGooglePlayServices

trait GoogleApi {
  def isGooglePlayServicesAvailable: Signal[Boolean]
  def checkGooglePlayServicesAvailable(activity: Activity): Unit
  def onActivityResult(requestCode: Int, resultCode: Int)
  def getPushToken: Option[PushToken]
  def deleteAllPushTokens(): Unit
}

class GoogleApiImpl(context: Context, beConfig: BackendConfig) extends GoogleApi {

  private val api = GoogleApiAvailability.getInstance()

  private val firebaseApp = beConfig.firebaseOptions(context)

  override val isGooglePlayServicesAvailable = Signal[Boolean](api.isGooglePlayServicesAvailable(context) == ConnectionResult.SUCCESS)

  override def checkGooglePlayServicesAvailable(activity: Activity) = api.isGooglePlayServicesAvailable(activity) match {
    case ConnectionResult.SUCCESS => isGooglePlayServicesAvailable ! true
    case code if api.isUserResolvableError(code) => api.getErrorDialog(activity, code, RequestGooglePlayServices)
    case code =>
      isGooglePlayServicesAvailable ! false
      warn(s"Google Play Services not available: error code: $code")
  }

  override def onActivityResult(requestCode: Int, resultCode: Int) = requestCode match {
    case RequestGooglePlayServices =>
      resultCode match {
        case Activity.RESULT_OK => isGooglePlayServicesAvailable ! true
        case _ =>
          isGooglePlayServicesAvailable ! false
          warn("Failed to update/install Google Play Services")
      }
    case _ => //
  }

  override def getPushToken =
    LoggedTry(FirebaseInstanceId.getInstance(firebaseApp).getToken(beConfig.pushSenderId, "FCM")).toOption.map(PushToken(_))

  override def deleteAllPushTokens(): Unit =
    LoggedTry(FirebaseInstanceId.getInstance(firebaseApp).deleteInstanceId())
}

object GoogleApi {
  val RequestGooglePlayServices = 7976
}
