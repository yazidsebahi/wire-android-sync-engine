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
package com.waz.service

import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.net.{ConnectivityManager, NetworkInfo}
import android.telephony.TelephonyManager
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{EventContext, Signal}
import com.waz.utils.returning

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class NetworkModeService(context: Context) {
  import NetworkModeService._

  private implicit val ev = EventContext.Global

  private lazy val connectivityManager = context.getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]
  private lazy val telephonyManager = context.getSystemService(Context.TELEPHONY_SERVICE).asInstanceOf[TelephonyManager]

  val networkMode = returning(Signal[NetworkMode](NetworkMode.OFFLINE)) { _.disableAutowiring() }

  val receiver = new BroadcastReceiver {
    override def onReceive(context: Context, intent: Intent): Unit = updateNetworkMode()
  }
  context.registerReceiver(receiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION))
  updateNetworkMode()
  CancellableFuture.delayed(scheduledNetworkCheckTimeout)(updateNetworkMode(true))

  def updateNetworkMode(scheduled: Boolean = false): Unit = {
    val network = Option(connectivityManager.getActiveNetworkInfo)

    network match {
      case Some(ni) => verbose(s"Detailed state: ${ni.getDetailedState.name()} => ${ni.getState.name()}, networkType: ${ni.getType}")
      case None => verbose("No network info available")
    }

    val mode = network match {
      case None => NetworkMode.OFFLINE
      case Some(info) if !info.isConnected => NetworkMode.OFFLINE
      case Some(info) => computeMode(info, telephonyManager)
    }
    verbose(s"updateNetworkMode: $mode")
    if (scheduled) {
      CancellableFuture.delayed(scheduledNetworkCheckTimeout)(updateNetworkMode(true))
      if (!(networkMode.currentValue contains mode)) HockeyApp.saveException(new RuntimeException("Missed network mode update"), s"mode: $mode")
    }
    networkMode.publish(mode, Threading.Background)
  }

  def isOfflineMode = networkMode.currentValue contains NetworkMode.OFFLINE
  def isOnlineMode = !isOfflineMode
}

object NetworkModeService {
  private implicit val logTag: LogTag = logTagFor(NetworkModeService)
  private val scheduledNetworkCheckTimeout = 15.seconds

  /*
   * This part (the mapping of mobile data network types to the networkMode enum) of the Wire software
   * uses source coded posted on the StackOverflow site.
   * (http://stackoverflow.com/a/18583089/1751834)
   *
   * That work is licensed under a Creative Commons Attribution-ShareAlike 2.5 Generic License.
   * (http://creativecommons.org/licenses/by-sa/2.5)
   *
   * Contributors on StackOverflow:
   *  - Anonsage (http://stackoverflow.com/users/887894/anonsage)
   */
  def computeMode(ni: NetworkInfo, tm: => TelephonyManager): NetworkMode = ni.getType match {
    case ConnectivityManager.TYPE_WIFI | ConnectivityManager.TYPE_ETHERNET => NetworkMode.WIFI
    case _ =>
      tm.getNetworkType match {
        case TelephonyManager.NETWORK_TYPE_GPRS |
             TelephonyManager.NETWORK_TYPE_1xRTT |
             TelephonyManager.NETWORK_TYPE_IDEN |
             TelephonyManager.NETWORK_TYPE_CDMA =>
          NetworkMode._2G
        case TelephonyManager.NETWORK_TYPE_EDGE =>
          NetworkMode.EDGE
        case TelephonyManager.NETWORK_TYPE_EVDO_0 |
             TelephonyManager.NETWORK_TYPE_EVDO_A |
             TelephonyManager.NETWORK_TYPE_HSDPA |
             TelephonyManager.NETWORK_TYPE_HSPA |
             TelephonyManager.NETWORK_TYPE_HSUPA |
             TelephonyManager.NETWORK_TYPE_UMTS |
             TelephonyManager.NETWORK_TYPE_EHRPD |
             TelephonyManager.NETWORK_TYPE_EVDO_B |
             TelephonyManager.NETWORK_TYPE_HSPAP =>
          NetworkMode._3G
        case TelephonyManager.NETWORK_TYPE_LTE =>
          NetworkMode._4G
        case _ =>
          info("Unknown network type, defaulting to Wifi")
          NetworkMode.WIFI
      }
  }
}
