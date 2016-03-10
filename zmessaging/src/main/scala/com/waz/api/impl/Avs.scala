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
package com.waz.api.impl

import com.waz.ZLog._
import com.waz.api.Avs.VideoCallbacks
import com.waz.api.AvsLogLevel
import com.waz.service.call.FlowManagerService
import com.waz.service.call.FlowManagerService.AvsLogData
import com.waz.threading.Threading
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.EventContext

class Avs(implicit ui: UiModule) extends com.waz.api.Avs  with UiObservable with SignalLoading  {
  implicit val dispatcher = Threading.Ui
  private implicit val tag: LogTag = logTagFor[Avs]

  private var logData: AvsLogData = AvsLogData.Default

  private var videoListener = Option.empty[VideoCallbacks]

  addLoader(_.flowmanager.avsLogDataSignal) { logData =>
    if (this.logData != logData) {
      this.logData = logData
      notifyChanged()
    }
  }

  ui.zms { zms =>
    val fm = zms.flowmanager

    fm.onCreateVideoPreview.on(Threading.Ui) { _ => videoListener foreach (l => fm.setVideoPreview(l.onPreviewRequested())) } (EventContext.Global)
    fm.onCreateVideoView.on(Threading.Ui) { case (convId, partId) => videoListener foreach (l => fm.setVideoView(convId, partId, l.onViewRequested())) } (EventContext.Global)

    fm.onReleaseVideoPreview.on(Threading.Ui) { _ => videoListener foreach (_.onPreviewReleased()) } (EventContext.Global)
    fm.onReleaseVideoView.on(Threading.Ui) { _ => videoListener foreach (_.onViewReleased()) } (EventContext.Global)
  }

  override def setLoggingEnabled(enable: Boolean): Unit = withFlowManagerService { _.setLoggingEnabled(enable) }

  override def isLoggingEnabled: Boolean = logData.loggingEnabled
  override def areMetricsEnabled: Boolean = logData.metricsEnabled

  override def setLogLevel(logLevel: AvsLogLevel): Unit = withFlowManagerService { _.setLogLevel(logLevel) }
  override def getLogLevel: AvsLogLevel = logData.logLevel

  override def setVideoListener(listener: VideoCallbacks): Unit = videoListener = Option(listener)
  override def unsetVideoListener(): Unit = videoListener = None

  private def withFlowManagerService(op: FlowManagerService => Unit): Unit = ui.zms { zms => op(zms.flowmanager) }
}
