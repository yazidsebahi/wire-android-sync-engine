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
package com.waz.service.assets

import java.io._
import java.util.concurrent.atomic.AtomicReference

import android.content.Context
import android.graphics.Bitmap
import android.net.Uri
import android.os.Environment
import com.waz.ZLog._
import com.waz.api.Permission
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.ProgressData
import com.waz.api.impl._
import com.waz.cache.{CacheEntry, CacheService, Expiration, LocalData}
import com.waz.content.WireContentProvider.CacheUri
import com.waz.content._
import com.waz.model.AssetData.{FetchKey, UploadKey}
import com.waz.model.AssetStatus.Order._
import com.waz.model.AssetStatus.{DownloadFailed, UploadCancelled, UploadDone, UploadFailed, UploadInProgress}
import com.waz.model.ErrorData.AssetError
import com.waz.model._
import com.waz.service.assets.GlobalRecordAndPlayService.AssetMediaKey
import com.waz.service.downloads.DownloadRequest._
import com.waz.service.downloads._
import com.waz.service.images.ImageAssetGenerator
import com.waz.service.{ErrorsService, PreferenceService}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import com.waz.utils.events.Signal
import com.waz.{PermissionsService, api}

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.Future.successful
import scala.concurrent.duration._
import scala.util.Random

class AssetService(val storage: AssetsStorage, generator: ImageAssetGenerator, cache: CacheService, context: Context,
    loader: AssetLoader, messages: MessagesStorage, downloader: DownloaderService, errors: ErrorsService,
    permissions: PermissionsService, streamLoader: Downloader[AssetFromInputStream], assetDownloader: AssetDownloader,
    metaService: MetaDataService, sync: SyncServiceHandle, media: GlobalRecordAndPlayService,
    prefs: PreferenceService) {

  import AssetService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  assetDownloader.onDownloadDone {
    case WireAssetRequest(_, id, _, _, _, _) => markDownloadDone(id)
    case _ =>
  }

  assetDownloader.onDownloadFailed {
    case (WireAssetRequest(_, id, _, _, _, _), _) => markDownloadFailed(id)
    case _ =>
  }

  messages.onMessageFailed { case (m, _) =>
    if (m.isAssetMessage) markUploadFailed(m.assetId, UploadFailed)
  }

  messages.onDeleted { msgs => storage.remove(msgs.map(id => AssetId(id.str))) }

  storage.onDeleted { assets => media.releaseAnyOngoing(assets.map(AssetMediaKey)(breakOut)) }

  errors.onErrorDismissed {
    case AssetError(ms) => Future.traverse(ms) { messages.delete }
  }

  def assetSignal(id: AssetId) = storage.signal(id).flatMap[(AssetData, api.AssetStatus)] {
    case asset @ AssetData.WithStatus(status) =>
      cache.cacheStorage.optSignal(asset.cacheKey).map(_.isDefined) flatMap {
        case true  =>
          verbose(s"asset in state DOWNLOAD_DONE, meta: ${asset.metaData}")
          Signal const (asset, api.AssetStatus.DOWNLOAD_DONE)
        case false =>
          downloader.getDownloadState(asset.cacheKey).map(_.state) map {
            case State.RUNNING    => (asset, api.AssetStatus.DOWNLOAD_IN_PROGRESS)
            case State.COMPLETED  => (asset, api.AssetStatus.DOWNLOAD_IN_PROGRESS) // reporting asset in progress since it should be added to cache before we change the state
            case _                => (asset, status.status)
          }
      }
    case _ => Signal.empty[(AssetData, api.AssetStatus)]
  }

  def downloadProgress(id: AssetId) = storage.signal(id) flatMap ( asset => downloader.getDownloadState(asset.cacheKey) )

  def cancelDownload(id: AssetId) = storage.get(id) flatMap {
    case Some(asset) => downloader.cancel(asset.cacheKey)
    case _ => Future successful (())
  }

  def uploadProgress(id: AssetId) = Signal const ProgressData.Indefinite // TODO

  def cancelUpload(id: AssetId, msg: MessageId): Future[Unit] =
    for {
      Some(asset) <- storage.get(id)
      _ <- downloader.cancel(asset.cacheKey)
      _ <- Cancellable.cancel(FetchKey(id))
      _ <- Cancellable.cancel(UploadKey(id))
      _ <- markUploadFailed(id, UploadCancelled)
    } yield ()

  def markUploadFailed(id: AssetId, status: AssetStatus.Syncable) =
    storage.updateAsset(id, { a => if (a.status > UploadInProgress) a else a.copy(status = status) }) flatMap {
      case Some(updated) =>
        storage.onUploadFailed ! updated
        messages.get(MessageId(id.str)) flatMap {
          case Some(m) => sync.postAssetStatus(m.id, m.convId, m.ephemeral, status)
          case None =>
            warn(s"No message found for asset upload: $id")
            Future.successful(())
        }
      case _ =>
        Future.successful(())
    }

  def addImageAsset(image: com.waz.api.ImageAsset, convId: RConvId, isSelf: Boolean): Future[AssetData] = {
    val convIdOpt = if (prefs.sendWithV3) None else Some(convId) //TODO Dean remove sending with v2 after testing
    image match {
        case img: ImageAsset =>
          val asset = img.data.copy(convId = convIdOpt)
          verbose(s"addImageAsset: $asset")
          val ref = new AtomicReference(image) // keep a strong reference until asset generation completes
          generator.generateWireAsset(asset, isSelf).future.flatMap { data =>
            storage.mergeOrCreateAsset(data) map (_ => data)
          } andThen { case _ => ref set null }
        case _ => Future.failed(new IllegalArgumentException(s"Unsupported ImageAsset: $image"))
      }
  }

  def updateAssets(data: Seq[AssetData]) =
    storage.updateOrCreateAll(data.map(d => d.id -> { (_: Option[AssetData]) => d })(collection.breakOut))

  def getAssetData(id: AssetId): CancellableFuture[Option[LocalData]] =
    CancellableFuture lift storage.get(id) flatMap {
      case None => CancellableFuture successful None
      case Some(asset) => loader.getAssetData(asset.loadRequest) map { res =>
        if (res.isEmpty) errors.addAssetFileNotFoundError(id)
        res
      }
    }

  def addAsset(a: AssetForUpload, conv: RConvId): Future[AssetData] = {
    val uri = a match {
      case ContentUriAssetForUpload(_, uri) => Some(uri)
      case _ => None
    }

    def loadData(asset: AssetData) = (a match {
      case TranscodedVideoAsset(_, data) => CancellableFuture lift cache.move(a.cacheKey, data, Mime.Video.MP4, if (asset.mime == Mime.Video.MP4) asset.name else asset.name.map(_ + ".mp4"), cacheLocation = Some(cache.cacheDir)) map { Some(_) }
      case _                             => loader.getAssetData(asset.loadRequest) //will ensure that PCM audio files get encoded
    }).flatMap {
      case Some(entry) => CancellableFuture.successful(entry)
      case None        =>
        errors.addAssetFileNotFoundError(a.id)
        CancellableFuture.failed(new NoSuchElementException("no data available after download"))
    }

    def previewAndMetaData(asset: AssetData) = for {
      entry    <- loadData(asset)
      (mime, nm) = entry match {
        case e: CacheEntry => (e.data.mimeType, e.data.fileName.orElse(asset.name))
        case _ => (asset.mime, asset.name)
      }
    //TODO Dean - I could have these running in parallel to make message generation quicker. Would then need to wait before sending though
      meta     <- metaService.loadMetaData(asset, entry)
      prev     <- metaService.loadPreview(asset, entry)
      updated  <- CancellableFuture lift storage.updateAsset(asset.id,
        _.copy(
          metaData = meta,
          mime = mime,
          name = nm,
          previewId = prev.map(_.id),
          sizeInBytes = entry.length))
    } yield updated

    for {
      m        <- a.mimeType
      size     <- a.sizeInBytes
      n        <- a.name
      asset    <- storage.insert(AssetData(
        a.id,
        mime = m,
        sizeInBytes = size.getOrElse(0),
        name = n,
        source = uri,
        convId = if (prefs.sendWithV3) None else Some(conv) //TODO turn off v2 toggling after transition period
      ))
      updated  <- Cancellable(FetchKey(a.id))(previewAndMetaData(asset)).future
    } yield returning(updated.getOrElse(asset))(a => verbose(s"created asset: $a"))
  }

  def markDownloadFailed(id: AssetId) = storage.updateAsset(id, _.copy(status = DownloadFailed))

  def markDownloadDone(id: AssetId) = storage.updateAsset(id, _.copy(status = UploadDone))

  def getAssetUri(id: AssetId): CancellableFuture[Option[Uri]] =
    CancellableFuture.lift(storage.get(id)) .flatMap {
      case Some(a: AssetData) =>
        verbose(s"getAssetUri for: $a")
        loader.getAssetData(a.loadRequest) flatMap {
          case Some(entry: CacheEntry) =>
            CancellableFuture successful {
              val uri = Some(CacheUri(entry.data, context))
              verbose(s"Created cache entry uri: $uri for asset: ${a.id}")
              uri
            }
          case Some(data) =>
            CancellableFuture lift cache.addStream(a.cacheKey, data.inputStream, a.mime, a.name, Some(cache.intCacheDir))(Expiration.in(12.hours)) map { e =>
              val uri = Some(CacheUri(e.data, context))
              verbose(s"Created cache entry, and then uri: $uri for asset: ${a.id}")
              uri
            }
          case None =>
            CancellableFuture successful None
        }
      case _ =>
        warn(s"asset not found: $id")
        CancellableFuture successful None
    }

  def saveAssetToDownloads(id: AssetId): Future[Option[File]] = storage.get(id).flatMapOpt(saveAssetToDownloads)

  def saveAssetToDownloads(asset: AssetData): Future[Option[File]] = {

    def nextFileName(baseName: String, retry: Int) =
      if (retry == 0) baseName else s"${retry}_$baseName"

    def getTargetFile(dir: File): Option[File] = {
      val baseName = asset.name.getOrElse("wire_downloaded_file." + asset.mime.extension) // XXX: should get default file name form resources
      // prepend a number to the name to get unique file name,
      // will try sequential numbers from 0 - 10 first, and then fallback to random ones
      // will give up after 100 tries
      val prefix = ((0 to 10).iterator ++ Iterator.continually(Random.nextInt(10000))).take(100)
      prefix.map(i => new File(dir, nextFileName(baseName, i))).find(!_.exists())
    }

    def saveAssetData(file: File) =
      loader.getAssetData(asset.loadRequest).future.map {
        case Some(data) =>
          IoUtils.copy(data.inputStream, new FileOutputStream(file))
          Some(file)
        case None =>
          None
      } (Threading.IO)

    val dir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS)
    if (dir.isDirectory)
      permissions.requiring(Set(Permission.WRITE_EXTERNAL_STORAGE), delayUntilProviderIsSet = false) (
        {
          warn("permission to save asset to downloads denied")
          successful(None)
        },
        getTargetFile(dir).fold(successful(Option.empty[File]))(saveAssetData)
      )
    else
      successful(None)
  }

}

object AssetService {
  private implicit val logTag: LogTag = logTagFor[AssetService]

  val SaveImageDirName = "Wire"

  lazy val SaveImageDir = {
    val path = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES) + File.separator + SaveImageDirName
    val dir = new File(path)
    dir.mkdirs()
    dir
  }

  def assetDir(context: Context) = new File(context.getFilesDir, "assets")

  def sanitizeFileName(name: String) = name.replace(' ', '_').replaceAll("[^\\w]", "")

  def saveImageFile(mime: Mime) = new File(SaveImageDir,  s"wire_${System.currentTimeMillis}.${mime.extension}")

  sealed trait BitmapResult
  object BitmapResult {
    case object Empty extends BitmapResult
    case class BitmapLoaded(bitmap: Bitmap, preview: Boolean, etag: Int = 0) extends BitmapResult {
      override def toString: LogTag = s"BitmapLoaded([${bitmap.getWidth}, ${bitmap.getHeight}], $preview, $etag)"
    }
    case class LoadingFailed(ex: Throwable) extends BitmapResult
  }

  sealed trait BitmapRequest {
    val width: Int
    val mirror: Boolean = false
  }
  object BitmapRequest {
    case class Regular(width: Int, override val mirror: Boolean = false) extends BitmapRequest
    case class Static(width: Int, override val mirror: Boolean = false) extends BitmapRequest
    case class Single(width: Int, override val mirror: Boolean = false) extends BitmapRequest
    case class Round(width: Int, borderWidth: Int = 0, borderColor: Int = 0) extends BitmapRequest
  }
}
