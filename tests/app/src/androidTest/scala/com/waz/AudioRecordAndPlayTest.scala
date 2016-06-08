package com.waz

import android.content.Context
import android.net.Uri
import android.test._
import com.waz.api.impl.AudioAssetForUpload
import com.waz.content.Mime
import com.waz.model.AssetId
import com.waz.service.ZMessaging
import com.waz.service.assets.RecordAndPlayService._
import com.waz.testapp.EmptyTestActivity
import com.waz.threading.Threading
import org.junit.Assert._
import org.threeten.bp
import org.threeten.bp.Instant

import scala.PartialFunction.cond
import scala.concurrent.Promise
import scala.concurrent.duration._

class AudioRecordAndPlayTest extends ActivityInstrumentationTestCase2[EmptyTestActivity](classOf[EmptyTestActivity]) {

  implicit val timeout: Timeout = Timeout(5.seconds)
  val orderedInstants = Ordering.ordered[Instant]
  import orderedInstants._

  override def setUp(): Unit = {
    super.setUp()
    Threading.AssertsEnabled = false
    ZMessaging.onCreate(context)
  }

  def testAudioRecording(): Unit = {
    val ongoingRecording = new RecordingHandler
    val beforeStart = Instant.now
    val controls = api.AssetFactory.recordAudioAsset(ongoingRecording)
    assertTrue(beforeStart < ongoingRecording.start.await)
    3.seconds.idle()
    controls.stop()
    val (asset @ AudioAssetForUpload(assetId, entry, _), _) = ongoingRecording.complete.await
    assertEquals(Mime.Audio.MP4, asset.mimeType.await)
    assertTrue(asset.sizeInBytes.await.get > 0L)
  }

  def testAudioRecordingLimit(): Unit = {
    implicit val ui = ZMessaging.currentUi
    val service = ZMessaging.currentGlobal.recordingAndPlayback

    val id = AssetId()
    val (start, promisedResult) = service.record(id, 1 << 15).await
    val RecordingSuccessful(asset: AudioAssetForUpload, limitReached) = promisedResult.await(Timeout(20.seconds))

    assertTrue(limitReached)
    assertTrue(asset.sizeInBytes.await.get <= (1 << 15))
  }

  def testAudioPlayback(): Unit = {
    val service = ZMessaging.currentGlobal.recordingAndPlayback
    val uri = Uri.parse("content://com.waz.test/audio.m4a")
    val id = AssetId()
    assertTrue(cond(service.play(id, uri).await) { case Playing(_, `id`) => true })
    3.seconds.idle()
    assertTrue(cond(service.pause(id).await) { case Paused(_, `id`, _, _) => true })
    1.second.idle()
    assertTrue(cond(service.play(id, uri).await) { case Playing(_, `id`) => true })
    1.second.idle()
    assertTrue(cond(service.setPlayhead(id, uri, bp.Duration.ofSeconds(7)).await) { case Playing(_, _) => true })
    2.seconds.idle()
    assertTrue(cond(service.pause(id).await) { case Paused(_, `id`, _, _) => true })
    assertTrue(cond(service.setPlayhead(id, uri, bp.Duration.ZERO).await) { case Paused(_, `id`, bp.Duration.ZERO, _) => true })
    assertTrue(cond(service.play(id, uri).await) { case Playing(_, `id`) => true })
    2.seconds.idle()

    val id2 = AssetId()
    assertTrue(cond(service.play(id2, uri).await) { case Playing(_, `id2`) => true })
    2.seconds.idle()
    assertTrue(cond(service.pause(id2).await) { case Paused(_, `id2`, _, _) => true })
  }

  def context: Context = getActivity.getApplication
}

class RecordingHandler extends api.RecordingCallback {
  val start = Promise[Instant]
  val complete = Promise[(api.AudioAssetForUpload, Boolean)]
  val cancel = Promise[Unit]

  override def onStart(timestamp: Instant): Unit = start.success(timestamp)
  override def onComplete(recording: api.AudioAssetForUpload, fileSizeLimitReached: Boolean): Unit = complete.success((recording, fileSizeLimitReached))
  override def onCancel(): Unit = cancel.success(())
}
