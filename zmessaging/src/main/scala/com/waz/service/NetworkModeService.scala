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
import com.waz.ZLog._
import com.waz.api.NetworkMode
import com.waz.utils.events.{EventContext, Signal}

class NetworkModeService(context: Context) {
  import NetworkModeService._

  private implicit val ev = EventContext.Global

  private lazy val connectivityManager = context.getSystemService(Context.CONNECTIVITY_SERVICE).asInstanceOf[ConnectivityManager]
  private lazy val telephonyManager = context.getSystemService(Context.TELEPHONY_SERVICE).asInstanceOf[TelephonyManager]

  val networkMode = Signal[NetworkMode](NetworkMode.OFFLINE)

  val receiver = new BroadcastReceiver {
    override def onReceive(context: Context, intent: Intent): Unit = updateNetworkMode()
  }
  context.registerReceiver(receiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION))
  updateNetworkMode()

  def updateNetworkMode(): Unit = {
    val network = Option(connectivityManager.getActiveNetworkInfo)
    val mode = network match {
      case None => NetworkMode.OFFLINE
      case Some(info) if !info.isConnectedOrConnecting => NetworkMode.OFFLINE
      case Some(info) => computeMode(info, telephonyManager)
    }
    verbose(s"updateNetworkMode: $mode")
    networkMode ! mode
  }

  def isOfflineMode = networkMode.currentValue contains NetworkMode.OFFLINE
  def isOnlineMode = !isOfflineMode
}

object NetworkModeService {
  private implicit val logTag: LogTag = logTagFor(NetworkModeService)

  def computeMode(ni: NetworkInfo, tm: => TelephonyManager): NetworkMode = {
    if (ni.isConnected) {
      ni.getType match {
        case ConnectivityManager.TYPE_WIFI | ConnectivityManager.TYPE_ETHERNET => NetworkMode.WIFI
        case _ =>
          val nt = tm.getNetworkType
          if (nt == TelephonyManager.NETWORK_TYPE_UNKNOWN) {
            info("Unknown network type, defaulting to Wifi")
            NetworkMode.WIFI
          } else if (nt >= TelephonyManager.NETWORK_TYPE_LTE) NetworkMode._4G
          else if (nt >= TelephonyManager.NETWORK_TYPE_HSDPA) NetworkMode._3G
          else NetworkMode._2G
      }
    } else NetworkMode.OFFLINE
  }
}
