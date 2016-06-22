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

import java.io.{File, FileNotFoundException}

import android.content.{BroadcastReceiver, Context, Intent, IntentFilter}
import android.media.MediaPlayer.OnSeekCompleteListener
import android.media.{AudioManager, MediaPlayer, MediaRecorder}
import android.net.Uri
import android.telephony.TelephonyManager
import com.waz.ZLog._
import com.waz.api
import com.waz.api.ErrorType
import com.waz.api.ErrorType._
import com.waz.api.impl.AudioAssetForUpload
import com.waz.cache.{CacheEntry, CacheService}
import com.waz.content.Mime
import com.waz.model.{AssetId, ErrorData, Uid}
import com.waz.service.assets.AudioLevels.peakLoudness
import com.waz.service.{ErrorsService, ZmsLifecycle}
import com.waz.threading.Threading
import com.waz.utils._
import com.waz.utils.events.{ClockSignal, EventContext, EventStream, Signal}
import org.threeten.bp
import org.threeten.bp.Instant

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class RecordAndPlayService(globalService: GlobalRecordAndPlayService, errors: ErrorsService, lifecycle: ZmsLifecycle) {
  import EventContext.Implicits.global
  import Threading.Implicits.Background

  globalService.onError { err =>
    err.tpe.foreach { tpe => errors.addErrorWhenActive(ErrorData(Uid(), tpe, responseMessage = err.message)) }
  }

  lifecycle.uiActive.onChanged.on(Background) {
    case false => globalService.AudioFocusListener.onAudioFocusChange(AudioManager.AUDIOFOCUS_LOSS)
    case true =>
  }(EventContext.Global)
}

class GlobalRecordAndPlayService(context: Context, cache: CacheService) {
  import GlobalRecordAndPlayService._
  import Threading.Implicits.Background

  private lazy val stateSource = returning(Signal[State](Idle))(_.disableAutowiring())
  private lazy val saveDir = AssetService.assetDir(context)

  lazy val state: Signal[State] = stateSource
  lazy val audioManager = context.getSystemService(Context.AUDIO_SERVICE).asInstanceOf[AudioManager]

  val onError = EventStream[Error]()

  context.registerReceiver(interruptionBroadcastReceiver, interruptionIntentFilter)

  def play(id: AssetId, contentUri: Uri): Future[State] = {
    transition {
      case Idle =>
        withAudioFocus()(playOrResumeTransition(id, Left(contentUri)))
      case Playing(player, `id`) =>
        KeepCurrent()
      case Playing(player, ongoing) =>
        Try(player.release())
        Try(withAudioFocus()(playOrResumeTransition(id, Left(contentUri)))).recover {
          case NonFatal(cause) => Next(Idle, Some(Error(s"cannot start playback $id ($contentUri) after releasing playing $ongoing", Some(cause), Some(PLAYBACK_FAILURE))))
        }.get
      case Paused(player, `id`, playhead, _) =>
        Try(withAudioFocus()(playOrResumeTransition(id, Right(player)))).recover {  // TODO if this fails, try reset, seek to playhead and then start
          case NonFatal(cause) =>
            Try(player.release())
            Next(Idle, Some(Error(s"cannot resume playback $id ($contentUri)", Some(cause), Some(PLAYBACK_FAILURE))))
        }.get
      case Paused(player, ongoing, _, _) =>
        Try(player.release())
        Try(withAudioFocus()(playOrResumeTransition(id, Left(contentUri)))).recover {
          case NonFatal(cause) => Next(Idle, Some(Error(s"cannot start playback $id ($contentUri) after releasing paused $ongoing", Some(cause), Some(PLAYBACK_FAILURE))))
        }.get
      case rec @ Recording(_, ongoing, _, _, _) =>
        Try(cancelOngoing(rec))
        Try(withAudioFocus()(playOrResumeTransition(id, Left(contentUri)))).recover {
          case NonFatal(cause) => Next(Idle, Some(Error(s"cannot start playback $id ($contentUri) after canceling recording $ongoing", Some(cause), Some(PLAYBACK_FAILURE))))
        }.get
    } (s"error while starting playback $id ($contentUri)", Some(PLAYBACK_FAILURE))
  }

  private def playOrResumeTransition(id: AssetId, uriOrPlayer: Either[Uri, MediaPlayer]): Transition = {
    val player = uriOrPlayer.fold(contentUri => createPlayer(id, contentUri), identity)
    withCleanupOnFailure {
      player.start()
      Next(Playing(player, id))
    } { cause =>
      Try(player.release())
      abandonAudioFocus()
    }
  }

  private def createPlayer(id: AssetId, content: Uri) = returning(MediaPlayer.create(context, content)) { player =>
    player.setOnCompletionListener(new CompletionHandler(id))
    player.setOnErrorListener(new PlaybackErrorHandler(id))
  }

  private class CompletionHandler(id: AssetId) extends MediaPlayer.OnCompletionListener {
    override def onCompletion(mp: MediaPlayer): Unit = transition {
      case Playing(player, `id`) =>
        Try(player.release())
        Next(Idle)
      case Paused(player, `id`, _, _) =>
        Try(player.release())
        Next(Idle)
      case other =>
        KeepCurrent(Some(Error(s"MediaPlayer signaled end of playback $id but state = $other")))
    } (s"error while ending playback $id", Some(PLAYBACK_FAILURE))
  }

  private class PlaybackErrorHandler(id: AssetId) extends MediaPlayer.OnErrorListener {
    override def onError(mp: MediaPlayer, what: Int, extra: Int): Boolean = {
      transition { any =>
        Try(mp.release())
        Next(Idle, Some(Error(s"playback $id failed; what: $what, extra: $extra, state = $any", None, Some(PLAYBACK_FAILURE))))
      } (s"error during playback $id", Some(PLAYBACK_FAILURE))
      true
    }
  }

  def pause(id: AssetId): Future[State] = transition {
    case Idle =>
      KeepCurrent()
    case Playing(player, `id`) =>
      pauseTransition(id, player, false)
    case Paused(_, `id`, _, _) =>
      KeepCurrent()
    case other =>
      throw new IllegalStateException(s"state = $other")
  } (s"error while pausing playback $id", Some(PLAYBACK_FAILURE))

  private def pauseTransition(id: AssetId, player: MediaPlayer, transient: Boolean): Transition = Try {
    player.pause()
    abandonAudioFocus()
    Next(Paused(player, id, bp.Duration.ofMillis(player.getCurrentPosition), transient))
  } getOrElse {
    player.release()
    abandonAudioFocus()
    Next(Idle)
  }

  def setPlayhead(id: AssetId, contentUri: Uri, playhead: bp.Duration): Future[State] = {
    def seek(maybePlayer: Option[MediaPlayer] = None) = {
      val player = maybePlayer.getOrElse(createPlayer(id, contentUri))
      withCleanupOnFailure {
        val promisedSeek = Promise[MediaPlayer]
        player.setOnSeekCompleteListener(new OnSeekCompleteListener {
          override def onSeekComplete(mp: MediaPlayer): Unit = {
            player.setOnSeekCompleteListener(null)
            promisedSeek.success(mp)
          }
        })
        player.seekTo(playhead.toMillis.toInt)
        promisedSeek.future
      } { cause =>
        if (maybePlayer.isEmpty) Try(player.release())
      }
    }

    def releaseOngoingAndSeek(res: Either[Recording, MediaPlayer], ongoing: AssetId) = Future {
      verbose(s"releasing $ongoing to seek $id to $playhead")
      res.fold(cancelOngoing, _.release())
    }.flatMap(_ => seek().map(p => Next(Paused(p, id, playhead)))).recover {
      case NonFatal(cause) => Next(Idle, Some(Error(s"cannot seek $id to $playhead after stopping $ongoing", Some(cause))))
    }.andThen {
      case _ => abandonAudioFocus()
    }

    transitionF {
      case Idle                                 => seek().map(p => Next(Paused(p, id, playhead)))
      case Playing(player, `id`)                => seek(Some(player)).map(_ => KeepCurrent())
      case Paused(player, `id`, _, _)           => seek(Some(player)).map(p => Next(Paused(p, id, playhead)))
      case Playing(player, ongoing)             => releaseOngoingAndSeek(Right(player), ongoing)
      case Paused(player, ongoing, _, _)        => releaseOngoingAndSeek(Right(player), ongoing)
      case rec @ Recording(_, ongoing, _, _, _) => releaseOngoingAndSeek(Left(rec), ongoing)
    }(s"error while setting playhead")
  }

  def playhead(id: AssetId): Signal[bp.Duration] = stateSource flatMap {
    case Playing(_, `id`) =>
      new ClockSignal(tickInterval).flatMap { i =>
        Signal.future(duringIdentityTransition { case Playing(player, `id`) => player.getCurrentPosition.millis.asJava })
      }
    case Paused(player, `id`, playhead, _) =>
      Signal.const(playhead)
    case other =>
      Signal.const(bp.Duration.ZERO)
  }

  def isPlaying(id: AssetId): Signal[Boolean] = stateSource.map {
    case Playing(_, `id`) => true
    case other => false
  }

  def recordingLevel(id: AssetId): EventStream[Float] =
    stateSource.flatMap {
      case Recording(_, `id`, _, _, _) =>
        new ClockSignal(tickInterval).flatMap { i =>
          Signal.future(duringIdentityTransition { case Recording(recorder, `id`, _, _, _) => (peakLoudness(recorder.getMaxAmplitude), i) })
        }
      case other => Signal.empty[(Float, Instant)]
    }.onChanged.map { case (level, _) => level }

  def record(id: AssetId, maxAllowedAssetSizeInBytes: Long): Future[(Instant, Future[RecordingResult])] = {
    def record(): Future[Next] = withAudioFocus() {
      cache.createForFile(id.str, Mime.Audio.MP4, cacheLocation = Some(saveDir)) map { entry =>
        val promisedAsset = Promise[RecordingResult]
        val recorder = new MediaRecorder()
        withCleanupOnFailure {
          prepareRecorder(recorder, entry.cacheFile, maxAllowedAssetSizeInBytes)
          val handler = new RecordingErrorHandler(id)
          recorder.setOnErrorListener(handler)
          recorder.setOnInfoListener(handler)
          recorder.start()
          val start = Instant.now
          Next(Recording(recorder, id, start, entry, promisedAsset))
        } { cause =>
          entry.delete()
          Try(recorder.release())
        }
      }
    } andThen {
      case Failure(cause) => abandonAudioFocus()
    }

    def releaseOngoingAndRecord(res: Either[Recording, MediaPlayer], ongoing: AssetId) = Future {
      verbose(s"releasing $ongoing to start recording $id")
      res.fold(cancelOngoing, _.release())
    }.flatMap(_ => record()).recover {
      case NonFatal(cause) => Next(Idle, Some(Error(s"cannot start recording $id after stopping $ongoing", Some(cause), Some(RECORDING_FAILURE))))
    }

    transitionF {
      case Idle                                        => record()
      case Playing(player, ongoing)                    => releaseOngoingAndRecord(Right(player), ongoing)
      case Paused(player, ongoing, _, _)               => releaseOngoingAndRecord(Right(player), ongoing)
      case rec @ Recording(recorder, ongoing, _, _, _) => releaseOngoingAndRecord(Left(rec), ongoing)
    }(s"error while starting audio recording $id", Some(RECORDING_FAILURE)).map {
      case Recording(_, `id`, start, _, promisedAsset) => (start, promisedAsset.future)
      case other                                       => throw new IllegalStateException(s"recording not started; state = $other")
    }
  }

  private class RecordingErrorHandler(id: AssetId) extends MediaRecorder.OnErrorListener with MediaRecorder.OnInfoListener {
    @volatile var limitReached = false

    override def onInfo(mr: MediaRecorder, what: Int, extra: Int): Unit =
      if (what == MediaRecorder.MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED) {
        verbose(s"recording $id reached the file size limit")
        limitReached = true
        stopRecording(id, true)
      }

    override def onError(mr: MediaRecorder, what: Int, extra: Int): Unit = if (! limitReached) transition {
      case Recording(recorder, `id`, _, _, promisedAsset) =>
        error(s"recording $id failed; what: $what, extra: $extra")
        promisedAsset.tryFailure(new RuntimeException(s"recording $id failed; what: $what, extra: $extra"))
        Try(mr.release())
        abandonAudioFocus()
        Next(Idle)
      case other =>
        KeepCurrent()
    } ("onError failed")
  }

  def stopRecording(id: AssetId, fileSizeLimitReached: Boolean): Future[State] = transition {
    case rec @ Recording(recorder, `id`, start, entry, promisedAsset) =>
      try Try(recorder.stop) match {
        case Success(()) =>
          if (entry.cacheFile.exists) promisedAsset.trySuccess(RecordingSuccessful(AudioAssetForUpload(id, entry, bp.Duration.between(start, Instant.now)), fileSizeLimitReached))
          else promisedAsset.tryFailure(new FileNotFoundException(s"audio file does not exist after recording: ${entry}"))
          Next(Idle)
        case Failure(exc: IllegalStateException) => throw exc
        case Failure(exc: RuntimeException) => cancelOngoing(rec); Next(Idle) // recording was stopped so soon that nothing was recorded and there probably is an invalid file
        case Failure(other) => throw other
      } finally {
        recorder.release()
        abandonAudioFocus()
      }
    case other =>
      throw new IllegalStateException(s"state = $other")
  } (s"error while stopping audio recording $id", Some(RECORDING_FAILURE))

  def cancelRecording(id: AssetId): Future[State] = transition {
    case rec @ Recording(_, `id`, _, _, _) =>
      cancelOngoing(rec)
      abandonAudioFocus()
      Next(Idle)
    case other =>
      KeepCurrent()
  } (s"error while cancelling audio recording $id", Some(RECORDING_FAILURE))

  private def prepareRecorder(recorder: MediaRecorder, destination: File, maxAllowedAssetSizeInBytes: Long) = {
    recorder.setAudioSource(recorder.AudioSource.MIC)
    recorder.setOutputFormat(recorder.OutputFormat.MPEG_4)
    recorder.setOutputFile(destination.getAbsolutePath)
    recorder.setAudioEncoder(recorder.AudioEncoder.AAC)
    recorder.setAudioChannels(1)
    recorder.setAudioSamplingRate(44100)
    recorder.setAudioEncodingBitRate(1 << 16)
    recorder.setMaxFileSize(maxAllowedAssetSizeInBytes)
    recorder.prepare() // may throw IOException
  }

  private def cancelOngoing(rec: Recording): Unit = {
    Try(rec.recorder.release())
    rec.entry.delete()
    rec.promisedAsset.trySuccess(RecordingCancelled)
  }

  def releaseAnyOngoing(ids: Set[AssetId]): Future[State] = transition {
    case Playing(player, id) if ids(id) =>
      Try(player.release())
      abandonAudioFocus()
      Next(Idle)
    case Paused(player, id, _, _) if ids(id) =>
      Try(player.release())
      Next(Idle)
    case rec @ Recording(_, id, _, _, _) if ids(id) =>
      cancelOngoing(rec)
      abandonAudioFocus()
      Next(Idle)
    case other => KeepCurrent()
  }(s"error cancelling any ongoing $ids")

  private def abandonAudioFocus(): Unit = audioManager.abandonAudioFocus(AudioFocusListener)

  private def withAudioFocus[A](transient: Boolean = false)(f: => A): A =
    audioManager.requestAudioFocus(AudioFocusListener, AudioManager.STREAM_MUSIC, if (transient) AudioManager.AUDIOFOCUS_GAIN_TRANSIENT else AudioManager.AUDIOFOCUS_GAIN) match {
      case AudioManager.AUDIOFOCUS_REQUEST_GRANTED => f
      case _ => throw new RuntimeException("audio focus request denied")
    }

  private def interruptionBroadcastReceiver = new BroadcastReceiver {
    override def onReceive(context: Context, intent: Intent): Unit = {
      val isIgnoredPhoneStateTransition =
        intent.getAction == TelephonyManager.ACTION_PHONE_STATE_CHANGED &&
        intent.getStringExtra(TelephonyManager.EXTRA_STATE) != TelephonyManager.EXTRA_STATE_RINGING

      if (! isIgnoredPhoneStateTransition) {
        verbose(s"interruption broadcast: ${intent.getAction}")
        AudioFocusListener.onAudioFocusChange(AudioManager.AUDIOFOCUS_LOSS)
      }
    }
  }

  private def interruptionIntentFilter = returning(new IntentFilter) { filter =>
    filter.addAction(AudioManager.ACTION_AUDIO_BECOMING_NOISY)
    filter.addAction(TelephonyManager.ACTION_PHONE_STATE_CHANGED)
    filter.addAction(Intent.ACTION_NEW_OUTGOING_CALL)
  }

  object AudioFocusListener extends AudioManager.OnAudioFocusChangeListener {
    override def onAudioFocusChange(focusChange: Int): Unit = focusChange match {
      case AudioManager.AUDIOFOCUS_LOSS_TRANSIENT =>
        verbose("audio focus lost (transient)")
        transition {
          case Playing(player, id)             => pauseTransition(id, player, true)
          case rec @ Recording(_, id, _, _, _) => cancelOngoing(rec); Next(Idle)
          case other                           => KeepCurrent()
        }(s"error while handling transient audio focus loss")
      case AudioManager.AUDIOFOCUS_GAIN =>
        verbose("audio focus gained")
        transition {
          case Paused(player, id, _, true) => playOrResumeTransition(id, Right(player))
          case other                       => KeepCurrent()
        }(s"error while handling audio focus gain")
      case AudioManager.AUDIOFOCUS_LOSS =>
        verbose("audio focus lost")
        abandonAudioFocus()
        transition {
          case Playing(player, id)             => pauseTransition(id, player, false)
          case rec @ Recording(_, id, _, _, _) => cancelOngoing(rec); Next(Idle)
          case other                           => KeepCurrent()
        }(s"error while handling transient audio focus loss")
      case other =>
        warn(s"unknown audio focus change: $other")
    }
    override val toString: String = s"AudioFocusListener-${Uid().str}"
  }

  private def transition(f: State => Transition)(errorMessage: String, errorType: Option[ErrorType] = None): Future[State] =
    transitionF(s => Future(f(s)))(errorMessage, errorType)

  private def transitionF(f: State => Future[Transition])(errorMessage: String, errorType: Option[ErrorType] = None): Future[State] =
    Serialized.future(GlobalRecordAndPlayService)(keepStateOnFailure(stateSource.head.flatMap(f))(errorMessage, errorType).map(applyState))

  private def duringIdentityTransition[A](pf: PartialFunction[State, A]): Future[A] = {
    val p = Promise[A]
    transition(state => returning(KeepCurrent()) { _ =>
      if (pf.isDefinedAt(state)) Try(pf(state)) match {
        case Success(a) => p.success(a)
        case Failure(cause) => p.failure(cause)
      }
    }) ("identity transition failed")
    p.future
  }

  private def applyState: Transition => State = { t =>
    t.changedState.foreach { next =>
      stateSource.mutate { current =>
        verbose(s"transition: $current -> $next")
        next
      }
    }
    t.error.foreach { err =>
      err.cause.fold(error(err.message))(c => error(err.message, c))
      onError ! err
    }
    t.changedState.orElse(stateSource.currentValue).getOrElse(Idle)
  }

  private def keepStateOnFailure(f: Future[Transition])(errorMessage: String, errorType: Option[ErrorType]): Future[Transition] = f recover {
    case NonFatal(cause) => KeepCurrent(Some(Error(errorMessage, Some(cause), errorType)))
  }
}

object GlobalRecordAndPlayService {
  private implicit val logTag: LogTag = logTagFor[GlobalRecordAndPlayService]

  sealed trait State
  case object Idle extends State
  case class Playing(player: MediaPlayer, id: AssetId) extends State
  case class Paused(player: MediaPlayer, id: AssetId, playhead: bp.Duration, transient: Boolean = false) extends State
  case class Recording(recorder: MediaRecorder, id: AssetId, start: Instant, entry: CacheEntry, promisedAsset: Promise[RecordingResult]) extends State

  sealed trait RecordingResult
  case object RecordingCancelled extends RecordingResult
  case class RecordingSuccessful(asset: api.AudioAssetForUpload, fileSizeLimitReached: Boolean) extends RecordingResult

  sealed abstract class Transition(val changedState: Option[State], val error: Option[Error])
  case class KeepCurrent(override val error: Option[Error] = None) extends Transition(None, error)
  case class Next(state: State, override val error: Option[Error] = None) extends Transition(Some(state), error)
  case class Error(message: String, cause: Option[Throwable] = None, tpe: Option[ErrorType] = None)

  val tickInterval = 50.millis
}
