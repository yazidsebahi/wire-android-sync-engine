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

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.ByteOrder.{LITTLE_ENDIAN, nativeOrder}

import android.content.Context
import android.media.MediaCodec.{BUFFER_FLAG_CODEC_CONFIG, BUFFER_FLAG_END_OF_STREAM, CONFIGURE_FLAG_ENCODE, INFO_OUTPUT_BUFFERS_CHANGED}
import android.media.MediaFormat._
import android.media.{MediaCodec, MediaCodecInfo, MediaFormat}
import android.net.Uri
import com.googlecode.mp4parser.FileDataSourceImpl
import com.googlecode.mp4parser.authoring.Movie
import com.googlecode.mp4parser.authoring.builder.DefaultMp4Builder
import com.googlecode.mp4parser.authoring.tracks.AACTrackImpl
import com.waz.api.ProgressIndicator.State
import com.waz.api.impl.ProgressIndicator.{ProgressData, ProgressReporter}
import com.waz.api.impl.{AssetForUpload, ProgressIndicator}
import com.waz.bitmap.video.MediaCodecHelper.{inputDequeueTimeoutMicros, outputDequeueTimeoutMicros}
import com.waz.service.TempFileService
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import libcore.io.SizeOf

import scala.concurrent.Promise
import scala.util.Try

class AudioTranscoder(tempFiles: TempFileService, context: Context) {
  import AudioTranscoder._
  import Threading.Implicits.Background

  //TODO remove dependency on AssetForUpload
  def apply(uri: Uri, mp4File: File, callback: ProgressIndicator.Callback): CancellableFuture[File] =
    AssetForUpload.queryContentUriInfo(context, uri).map(_._3.getOrElse(0L)).lift.flatMap { size =>
      val promisedFile = Promise[File]

      promisedFile.tryComplete(Try {
        Managed(tempFiles.newTempFile).foreach { aac =>
          val progress = if (size <= 0L) {
            callback(ProgressData.Indefinite)
            Option.empty[ProgressReporter]
          } else {
            Some(new ProgressReporter(callback, size))
          }

          transcodeToAAC(uri, aac.file, progress, promisedFile.isCompleted)
          val aacTrack = new AACTrackImpl(new FileDataSourceImpl(aac.file))
          val movie = returning(new Movie)(_.addTrack(aacTrack))
          val container = new DefaultMp4Builder().build(movie)

          if (! promisedFile.isCompleted) Managed(new FileOutputStream(mp4File)).foreach(stream => container.writeContainer(stream.getChannel))
          progress.fold2(callback.apply(ProgressData(0, -1, State.COMPLETED)), _.completed)
        }

        mp4File
      })

      new CancellableFuture(promisedFile)
    } (Threading.BlockingIO)

  import AudioLevels.MediaCodecCleanedUp

  private def transcodeToAAC(uri: Uri, aacFile: File, reporter: Option[ProgressReporter], hasBeenCancelled: => Boolean): Unit =
    for {
      in <- Managed(context.getContentResolver.openInputStream(uri))
      outStream <- Managed(new FileOutputStream(aacFile))
      out = outStream.getChannel
      encoder <- Managed(audioEncoder)
    } {
      encoder.start()

      val inputBuffers = encoder.getInputBuffers
      val outputBufferInfo = new MediaCodec.BufferInfo

      var outputBuffers = encoder.getOutputBuffers
      var samplesSoFar = 0L
      var endOfInput = false
      var endOfOutput = false

      while (! endOfOutput && ! hasBeenCancelled) {
        if (! endOfInput) {
          val inputBufferIndex = encoder.dequeueInputBuffer(inputDequeueTimeoutMicros)

          if (inputBufferIndex >= 0) {
            val inputBuffer = inputBuffers(inputBufferIndex).order(nativeOrder)
            val readBuffer = Array.ofDim[Byte](inputBuffer.remaining())
            val bytesRead = in.read(readBuffer)

            if (bytesRead < 0) {
              endOfInput = true
              encoder.queueInputBuffer(inputBufferIndex, 0, 0, 0, BUFFER_FLAG_END_OF_STREAM)
            } else {
              val shorts = ByteBuffer.wrap(readBuffer, 0, bytesRead).order(LITTLE_ENDIAN).asShortBuffer
              val presentationTimeUs = (samplesSoFar / SizeOf.SHORT) * 1000000L / PCM.sampleRate
              samplesSoFar += shorts.remaining()
              inputBuffer.position(0)
              inputBuffer.asShortBuffer.put(shorts)
              encoder.queueInputBuffer(inputBufferIndex, 0, bytesRead, presentationTimeUs, 0)
            }
          }
        }

        val outputBufferIndex = encoder.dequeueOutputBuffer(outputBufferInfo, outputDequeueTimeoutMicros)

        if (outputBufferIndex >= 0 && (outputBufferInfo.flags & BUFFER_FLAG_CODEC_CONFIG) == 0) {
          val outputBuffer = outputBuffers(outputBufferIndex)
          if (outputBufferInfo.size > 0) {
            outputBuffer.position(outputBufferInfo.offset)
            outputBuffer.limit(outputBufferInfo.offset + outputBufferInfo.size)

            out.write(adtsHeader(outputBufferInfo.size))
            out.write(outputBuffer)
          }
          encoder.releaseOutputBuffer(outputBufferIndex, false)
          if (outputBufferInfo.presentationTimeUs > 0L) reporter.foreach(_.running(samplesSoFar * SizeOf.SHORT))
          endOfOutput = (outputBufferInfo.flags & BUFFER_FLAG_END_OF_STREAM) != 0
        } else if (outputBufferIndex == INFO_OUTPUT_BUFFERS_CHANGED) {
          outputBuffers = encoder.getOutputBuffers
        }
      }

      encoder.stop()
    }

  private def adtsHeader(dataLength: Int): ByteBuffer = { // see https://wiki.multimedia.cx/index.php?title=ADTS
    val packetLength = dataLength + 7
    val profile = MediaCodecInfo.CodecProfileLevel.AACObjectLC
    val samplingFrequencyIndex = 4 //44.1KHz
    val channelConfiguration = 1

    ByteBuffer.wrap(Array[Byte](
      0xFF.toByte,
      0xF9.toByte,
      (((profile - 1) << 6) + (samplingFrequencyIndex << 2) + (channelConfiguration >> 2)).toByte,
      (((channelConfiguration & 3) << 6) + (packetLength >> 11)).toByte,
      ((packetLength & 0x7FF) >> 3).toByte,
      (((packetLength & 7) << 5) + 0x1F).toByte,
      0xFC.toByte))
  }

  private def audioEncoder: MediaCodec = returning(MediaCodec.createEncoderByType("audio/mp4a-latm")) { mc =>
    mc.configure(aacFormat, null, null, CONFIGURE_FLAG_ENCODE)
  }

  private def aacFormat = returning(new MediaFormat) { format =>
    format.setString(KEY_MIME, "audio/mp4a-latm")
    format.setInteger(KEY_BIT_RATE, bitRate)
    format.setInteger(KEY_CHANNEL_COUNT, 1)
    format.setInteger(KEY_SAMPLE_RATE, sampleRate)
    format.setInteger(KEY_AAC_PROFILE, MediaCodecInfo.CodecProfileLevel.AACObjectLC)
  }
}

object AudioTranscoder {
  val bitRate = 1 << 16
  val sampleRate = PCM.sampleRate

  def estimatedSizeBasedOnBitrate(byteCount: Long): Long =
    math.round(((byteCount / SizeOf.SHORT).toDouble / sampleRate.toDouble) * (bitRate.toDouble / 8d)).toLong
}
