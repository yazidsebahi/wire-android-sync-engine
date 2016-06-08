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

import com.waz.api
import com.waz.api.KindOfTrackingEvent._
import org.threeten.bp.Duration

case class TrackingEvent(kind: api.KindOfTrackingEvent, assetSizeInBytes: Option[Long] = None, assetMimeType: Option[String] = None,
    duration: Option[Duration] = None, convType: Option[api.IConversation.Type] = None, error: Option[api.ErrorResponse] = None) extends api.TrackingEvent {
  override def getKind = kind
  override lazy val getAssetSizeInBytes = Opt(assetSizeInBytes.map(Long.box))
  override lazy val getAssetMimeType = Opt(assetMimeType)
  override lazy val getDuration = Opt(duration)
  override lazy val getConversationType = Opt(convType)
  override lazy val getErrorResponse = Opt(error)
}

object TrackingEvent {
  def assetUploadStarted(size: Option[Long], mime: String, tpe: api.IConversation.Type) = TrackingEvent(ASSET_UPLOAD_STARTED, size, Some(mime), convType = Some(tpe))
  def assetUploadSuccessful(size: Long, mime: String, dur: Duration) = TrackingEvent(ASSET_UPLOAD_SUCCESSFUL, Some(size), Some(mime), Some(dur))
  def assetUploadCancelled(size: Option[Long], mime: String) = TrackingEvent(ASSET_UPLOAD_CANCELLED, size, Some(mime))
  def assetUploadFailed(error: api.ErrorResponse) = TrackingEvent(ASSET_UPLOAD_FAILED, error = Some(error))
  def assetDownloadStarted(size: Long) = TrackingEvent(ASSET_DOWNLOAD_STARTED, Some(size))
  def assetDownloadSuccessful(size: Long, mime: String) = TrackingEvent(ASSET_DOWNLOAD_SUCCESSFUL, Some(size), Some(mime))
  def assetDownloadFailed(size: Long) = TrackingEvent(ASSET_DOWNLOAD_FAILED, Some(size))
}
