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

import android.net.Uri
import com.waz.ZLog._
import com.waz.api
import com.waz.api.Asset.LoadCallback
import com.waz.content.Mime
import com.waz.model.AssetMetaData.{HasDimensions, HasDuration}
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.ui.{SignalLoading, UiModule}
import com.waz.utils.events.Signal
import org.threeten.bp.Duration

import scala.PartialFunction.cond
import scala.util.Success

class Asset(id: AssetId, msg: MessageId)(implicit ui: UiModule) extends BaseAsset with SignalLoading {
  import Asset._
  import com.waz.threading.Threading.Implicits.Ui

  protected var asset = AnyAssetData.Empty
  protected var status = api.AssetStatus.UPLOAD_NOT_STARTED

  addLoader(_.assets.assetSignal(id)) {
    case (a, s) =>
      if (asset != a || status != s) {
        verbose(s"$asset -> $a, $status -> $s")
        asset = a
        status = s
        notifyChanged()
      }
    case other =>
      error(s"got unknown asset data: $other")
  }

  override def getId: String = id.str
  override def isEmpty: Boolean = false

  override def getContentUri(callback: LoadCallback[Uri]): Unit =
    ui.zms.flatMap(_.assets.getAssetUri(id)).onComplete {
      case Success(Some(uri)) => callback.onLoaded(uri)
      case _ => callback.onLoadFailed()
    }

  override def saveToDownloads(callback: LoadCallback[Uri]): Unit =
    ui.zms.flatMapFuture(_.assets.saveAssetToDownloads(id)).onComplete {
      case Success(Some(file)) => callback.onLoaded(Uri.fromFile(file))
      case _ => callback.onLoadFailed()
    }

  private def dimensions = asset.metaData.collect { case HasDimensions(d) => d } getOrElse Dim2.Empty

  override def getDuration: Duration = asset.metaData.collect { case HasDuration(d) => d } getOrElse Duration.ZERO
  override def getWidth: Int = dimensions.width
  override def getHeight: Int = dimensions.height

  override def getPlaybackControls(callback: LoadCallback[api.PlaybackControls]): Unit =
    if (isAudio) ui.zms.flatMap(_.assets.getAssetUri(id)).onComplete {
      case Success(Some(uri)) => callback.onLoaded(new PlaybackControls(id, uri, durationSignal))
      case _ => callback.onLoadFailed()
    }
    else callback.onLoadFailed()

  private def durationSignal(zms: ZMessaging): Signal[Duration] = zms.assets.assetSignal(id).map {
    case (AnyAssetData(_, _, _, _, _, Some(HasDuration(duration)), _, _, _, _), _) => duration
    case other => Duration.ZERO
  }

  override def getDownloadProgress: api.ProgressIndicator = new DownloadProgress(id)
  override def getUploadProgress: api.ProgressIndicator = new UploadProgress(id, msg)

  override def getStatus: api.AssetStatus = status

  override val toString: String = s"Asset($id)@$hashCode"
}

object Asset {
  private implicit val logTag: LogTag = logTagFor[Asset]

  object Empty extends BaseAsset {
    protected override def asset = AnyAssetData.Empty
    override def getId: String = asset.id.str
    override def isEmpty: Boolean = true
    override def getDuration: Duration = Duration.ZERO
    override def getWidth: Int = 0
    override def getHeight: Int = 0
    override def getContentUri(callback: LoadCallback[Uri]): Unit = callback.onLoadFailed()
    override def saveToDownloads(callback: LoadCallback[Uri]): Unit = callback.onLoadFailed()
    override def getDownloadProgress: api.ProgressIndicator = ProgressIndicator.Empty
    override def getUploadProgress: api.ProgressIndicator = ProgressIndicator.Empty
    override def getPlaybackControls(callback: LoadCallback[api.PlaybackControls]): Unit = callback.onLoadFailed()
  }

  class DownloadProgress(id: AssetId)(implicit ui: UiModule) extends ProgressIndicator with SignalLoading {
    addLoader(_.assets.downloadProgress(id))(set)
    override def cancel(): Unit = ui.zms(_.assets.cancelDownload(id))
  }

  class UploadProgress(id: AssetId, msg: MessageId)(implicit ui: UiModule) extends ProgressIndicator with SignalLoading {
    addLoader(_.assets.uploadProgress(id))(set)
    override def cancel(): Unit = ui.zms(_.assets.cancelUpload(id, msg))
  }
}

abstract class BaseAsset extends api.Asset with UiObservable {
  protected def asset: AnyAssetData

  override def getName: String = asset.name getOrElse ""
  override def getMimeType: String = asset.mimeType.orDefault.str
  override def isVideo: Boolean = cond(asset.mimeType.orDefault) { case Mime.Video() => true }
  override def isAudio: Boolean = cond(asset.mimeType.orDefault) { case Mime.Audio() => true }
  override def getSizeInBytes: Long = asset.sizeInBytes
  override def getStatus: api.AssetStatus = asset.status.status
}

class PlaybackControls(id: AssetId, contentUri: Uri, durationSource: ZMessaging => Signal[Duration])(implicit ui: UiModule) extends api.PlaybackControls with UiObservable with SignalLoading {
  private var playing = false
  private var playhead = Duration.ZERO
  private var duration = Duration.ZERO

  addLoader(zms => Signal(zms.global.recordingAndPlayback.isPlaying(id), zms.global.recordingAndPlayback.playhead(id), durationSource(zms))) {
    case (nextPlaying, nextPlayhead, nextDuration) =>
      if (nextPlaying != playing || nextPlayhead != playhead || nextDuration != duration) {
        playing = nextPlaying
        playhead = nextPlayhead
        duration = nextDuration
        notifyChanged()
      }
  }

  override def play: Unit = ui.global.recordingAndPlayback.play(id, contentUri)
  override def stop: Unit = ui.global.recordingAndPlayback.pause(id)

  override def isPlaying: Boolean = playing
  override def getDuration: Duration = duration

  override def getPlayhead: Duration = playhead
  override def setPlayhead(ph: Duration): Unit = ui.global.recordingAndPlayback.setPlayhead(id, contentUri, ph)
}
