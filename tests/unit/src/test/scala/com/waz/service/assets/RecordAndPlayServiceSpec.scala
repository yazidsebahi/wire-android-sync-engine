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

import java.io.File

import com.waz.api
import com.waz.api.AssetFactory
import com.waz.api.impl.AudioAssetForUpload
import com.waz.service.ZMessaging
import com.waz.service.assets.RecordAndPlayService._
import com.waz.testutils.Implicits._
import com.waz.testutils.Matchers._
import com.waz.testutils.{MockInstance, MockUiModule}
import com.waz.threading.Threading.Background
import com.waz.utils._
import org.robolectric.Robolectric
import org.scalatest._
import org.threeten.bp
import org.threeten.bp.Instant
import org.threeten.bp.Instant.now

import scala.concurrent.Promise
import scala.concurrent.duration._

class RecordAndPlayServiceSpec extends FeatureSpec with Matchers with OptionValues with BeforeAndAfterAll with RobolectricTests {

  feature("Recording an audio message") {
    scenario("Cancel recording") {
      val spy = new RecordingHandler
      val controls = AssetFactory.recordAudioAsset(spy)
      spy.start.await()
      soon(service.state.currentValue.value should beMatching { case Recording(_, _, _, _, _) => })
      controls.cancel()
      spy.cancel.await()
      soon(service.state.currentValue.value should beMatching { case Idle => })
    }

    scenario("Just record") {
      val spy = new RecordingHandler
      val beforeStart = now
      val controls = AssetFactory.recordAudioAsset(spy)
      val start = spy.start.await()
      start should be > beforeStart
      soon(service.state.currentValue.value should beMatching { case Recording(_, _, _, _, _) => })
      val Recording(recorder, _, _, _, _) = service.state.currentValue.value
      provideRecordingFile()
      val beforeStop = now
      controls.stop()
      val AudioAssetForUpload(id, entry, duration) = spy.complete.await()
      entry.cacheFile.exists shouldBe true
      duration should be > bp.Duration.between(start, beforeStop)
      soon(service.state.currentValue.value should beMatching { case Idle => })
    }

    scenario("Attempt to start recording while recording") {
      val spy, spy2 = new RecordingHandler
      val controls = AssetFactory.recordAudioAsset(spy)
      spy.start.await()
      soon(service.state.currentValue.value should beMatching { case Recording(_, _, _, _, _) => })
      val Recording(_, idOfFirstRecording, _, _, _) = service.state.currentValue.value
      val controls2 = AssetFactory.recordAudioAsset(spy2)
      spy.cancel.await()
      spy2.start.await()
      spy.complete.isCompleted shouldBe false
      soon(service.state.currentValue.value should beMatching { case Recording(_, _, _, _, _) => })
      val Recording(_, idOfSecondRecording, _, _, _) = service.state.currentValue.value
      idOfSecondRecording should not equal idOfFirstRecording
      provideRecordingFile()
      controls2.stop()
      spy2.complete.await()
      soon(service.state.currentValue.value should beMatching { case Idle => })
    }

    scenario("Race to start recording")(10.times {
      val spy, spy2 = new RecordingHandler
      val controls = AssetFactory.recordAudioAsset(spy)
      val controls2 = AssetFactory.recordAudioAsset(spy2)

      spy.start.await()
      spy.cancel.await()
      spy2.start.await()
      controls2.cancel()
      spy2.cancel.await()
    })

    scenario("File is missing after recording") {
      val spy = new RecordingHandler
      val controls = AssetFactory.recordAudioAsset(spy)
      spy.start.await()
      soon(service.state.currentValue.value should beMatching { case Recording(_, _, _, _, _) => })
      idle(100.millis)
      controls.stop()
      spy.cancel.await()
      spy.complete.isCompleted shouldBe false
      soon(service.state.currentValue.value should beMatching { case Idle => })
    }

    scenario("Record again right after stop") {
      val spy, spy2 = new RecordingHandler
      val controls = AssetFactory.recordAudioAsset(spy)
      spy.start.await()
      provideRecordingFile()
      controls.stop()

      val controls2 = spy.complete.future.map(_ => AssetFactory.recordAudioAsset(spy2))(Background).await()
      spy2.start.await()
      provideRecordingFile()
      controls2.stop()
      spy2.complete.await()

      soon(service.state.currentValue.value should beMatching { case Idle => })
    }
  }

  def provideRecordingFile(): Unit = {
    val Recording(recorder, _, _, _, _) = service.state.currentValue.value
    val shadowRecorder = Robolectric.shadowOf(recorder)
    IoUtils.copy(getClass.getResourceAsStream("/assets/audio.m4a"), new File(shadowRecorder.getOutputPath))
  }

  override def beforeAll: Unit = {
    ZMessaging.currentInstance = instance
    ZMessaging.currentGlobal = instance.global
    ZMessaging.currentUi = MockUiModule(instance)
  }

  lazy val instance = new MockInstance()
  lazy val service = instance.global.recordingAndPlayback
}

class RecordingHandler extends api.RecordingCallback {
  val start = Promise[Instant]
  val complete = Promise[api.AudioAssetForUpload]
  val cancel = Promise[Unit]

  override def onStart(timestamp: Instant): Unit = start.success(timestamp)
  override def onComplete(recording: api.AudioAssetForUpload, fileSizeLimitReached: Boolean): Unit = complete.success(recording)
  override def onCancel(): Unit = cancel.success(())
}
