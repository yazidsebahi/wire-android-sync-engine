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
    metaService: MetaDataService, previewService: PreviewService, sync: SyncServiceHandle, media: GlobalRecordAndPlayService,
    prefs: PreferenceService) {

  import AssetService._
  import com.waz.threading.Threading.Implicits.Background
  import com.waz.utils.events.EventContext.Implicits.global

  assetDownloader.onDownloadDone {
    case WireAssetRequest(id, _, _, _, _) => markDownloadDone(id)
    case _ =>
  }

  assetDownloader.onDownloadFailed {
    case (WireAssetRequest(id, _, _, _, _), _) => markDownloadFailed(id)
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
      cache.cacheStorage.optSignal(id).map(_.isDefined) flatMap {
        case true  =>
          verbose(s"asset in state DOWNLOAD_DONE, meta: ${asset.metaData}")
          if (asset.metaData.isEmpty) metaService.getAssetMetadata(id) // asset downloaded but has no metadata, let's update
          Signal const (asset, api.AssetStatus.DOWNLOAD_DONE)
        case false =>
          downloader.getDownloadState(id).map(_.state) map {
            case State.RUNNING    => (asset, api.AssetStatus.DOWNLOAD_IN_PROGRESS)
            case State.COMPLETED  => (asset, api.AssetStatus.DOWNLOAD_IN_PROGRESS) // reporting asset in progress since it should be added to cache before we change the state
            case _                => (asset, status.status)
          }
      }
    case _ => Signal.empty[(AssetData, api.AssetStatus)]
  }

  def downloadProgress(id: AssetId) = downloader.getDownloadState(id)

  def cancelDownload(id: AssetId) = downloader.cancel(id)

  def uploadProgress(id: AssetId) = Signal const ProgressData.Indefinite // TODO

  def cancelUpload(id: AssetId, msg: MessageId): Future[Unit] =
    for {
      _ <- downloader.cancel(id)
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
    val asset = image match {
      case img: LocalImageAsset => Some(img.data.copy(convId = convIdOpt))
      case _: ImageAsset => Some(AssetData.NewImageAsset(AssetId(image.getId)).copy(convId = convIdOpt))
      case _ => None
    }

    asset match {
      case Some(asset) =>
        val ref = new AtomicReference(image) // keep a strong reference until asset generation completes
        generator.generateWireAsset(asset, isSelf).future.flatMap { data =>
          storage.updateOrCreateAsset(data) map (_ => data)
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

  def assetDataOrSource(asset: AssetData): CancellableFuture[Option[Either[LocalData, Uri]]] =
    CancellableFuture lift cache.getEntry(asset.id) flatMap {
      case Some(entry) => CancellableFuture successful Some(Left(entry))
      case None =>
        asset.source match {
          case Some(uri) => CancellableFuture successful Some(Right(uri))
          case None =>
            getAssetData(asset.id) map { _.map(Left(_)) }
        }
    }


  def addAsset(a: AssetForUpload, conv: RConvId): Future[AssetData] = {
    def addAssetFromUri(id: AssetId, uri: Uri, originalMimeType: Option[Mime], metaData: Option[AssetMetaData]) = {
      // it's enough to create AssetData with input uri, everything else can be updated lazily
      verbose(s"addAsset from uri: $uri")
      for {
        mime  <- a.mimeType
        size  <- a.sizeInBytes
        name  <- a.name
        asset <- storage.insert(
          AssetData(
            id,
            mime = mime,
            sizeInBytes = size.getOrElse(0),
            name = name,
            metaData = metaData,
            source = Some(uri),
            convId = if (prefs.sendWithV3) None else Some(conv) //TODO turn off v2 toggling after transition period
          ))
      } yield {
        verbose(s"asset added: $asset")
        // metadata and preview will be needed in UI, let's start generating them already
        metaService.getAssetMetadata(id) flatMap { _ => previewService.getAssetPreview(id) }
        asset
      }
    }

    a match {
      case ContentUriAssetForUpload(id, uri) =>
        addAssetFromUri(id, uri, None, None)

      case AudioAssetForUpload(id, entry, duration, _) =>
        addAssetFromUri(id, CacheUri(entry.data, context), Some(Mime.Audio.PCM), Some(AssetMetaData.Audio(duration)))

      case _ =>

        def fetchAssetData(mime: Mime, name: Option[String]) = (a match {
          case TranscodedVideoAsset(_, data) => CancellableFuture lift cache.move(a.id, data, Mime.Video.MP4, if (mime == Mime.Video.MP4) name else name.map(_ + ".mp4"), cacheLocation = Some(cache.cacheDir)) map { Some(_) }
          case _                             => downloader.download(AssetFromInputStream(a.id, () => a.openDataStream(context), mime, name))(streamLoader)
        }).flatMap {
          case Some(entry) => CancellableFuture.successful(entry)
          case None        =>
            errors.addAssetFileNotFoundError(a.id)
            CancellableFuture.failed(new NoSuchElementException("no cache entry after download"))
        }

        def fetchAsset(m: Mime, n: Option[String], asset: AssetData) = for {
          entry    <- fetchAssetData(m, n)
          (mimeType, nm) = entry match {
            case e: CacheEntry => (e.data.mimeType, e.data.fileName.orElse(n))
            case _ => (m, n)
          }
          meta     <- metaService.loadMetaData(mimeType, entry)
          _        =  verbose(s"meta data for ${a.id}: $meta")
          prev  <- previewService.loadPreview(a.id, mimeType, entry)
          _        =  verbose(s"preview for ${a.id}: $prev")
          updated  <- CancellableFuture lift storage.updateAsset(asset.id,
            _.copy(
              metaData = meta,
              mime = mimeType,
              name = nm,
              previewId = prev.map(_.id),
              sizeInBytes = entry.length))
        } yield updated

        for {
          m        <- a.mimeType
          size     <- a.sizeInBytes
          n        <- a.name
          asset    = AssetData(a.id, mime = m, sizeInBytes = size.getOrElse(0), name = n, convId = if (prefs.sendWithV3) None else Some(conv)) //TODO turn off v2 toggling after transition period
          _        <- storage.insert(asset)
          updated  <- Cancellable(FetchKey(a.id))(fetchAsset(m, n, asset)).future
        } yield
          updated.getOrElse(asset)
    }
  }

  def markDownloadFailed(id: AssetId) = storage.updateAsset(id, _.copy(status = DownloadFailed))

  def markDownloadDone(id: AssetId) = storage.updateAsset(id, _.copy(status = UploadDone))

  def getAssetUri(id: AssetId): CancellableFuture[Option[Uri]] =
    CancellableFuture.lift(storage.get(id)) .flatMap {
      case Some(a: AssetData) =>
        loader.getAssetData(a.loadRequest) flatMap {
          case Some(entry: CacheEntry) =>
            CancellableFuture successful Some(CacheUri(entry.data, context))
          case Some(data) =>
            CancellableFuture lift cache.addStream(a.id, data.inputStream, a.mime, a.name, Some(cache.intCacheDir))(Expiration.in(12.hours)) map { e => Some(CacheUri(e.data, context)) }
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
