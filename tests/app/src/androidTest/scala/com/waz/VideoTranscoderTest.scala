package com.waz

import java.io.File
import java.util.concurrent.CountDownLatch

import android.net.Uri
import android.test._
import com.waz.api.AssetFactory.LoadCallback
import com.waz.api.{AssetFactory, AssetForUpload}
import com.waz.bitmap.video.VideoTranscoder
import com.waz.service.ZMessaging
import com.waz.testapp.EmptyTestActivity
import com.waz.threading.Threading
import org.junit.Assert

import scala.concurrent.Await
import scala.concurrent.duration._

class VideoTranscoderTest extends ActivityInstrumentationTestCase2[EmptyTestActivity](classOf[EmptyTestActivity]) {

  def context = getActivity.getApplication

  override def setUp(): Unit = {
    super.setUp()
    Threading.AssertsEnabled = false
    ZMessaging.onCreate(getActivity.getApplication)
  }

  def test8khzAudioTranscoding() = {
    val transcoder = VideoTranscoder(context)
    val out = File.createTempFile("video", ".mp4", context.getCacheDir)

    val future = transcoder(Uri.parse("content://com.waz.test/8khz.mp4"), out, { _ => })

    Assert.assertEquals(out, Await.result(future, 15.seconds))
  }

  def testAssetLoadingWithNoAudio() = {
    val latch = new CountDownLatch(1)
    var asset = Option.empty[AssetForUpload]

    AssetFactory.videoAsset(Uri.parse("content://com.waz.test/no_audio.mp4"), new LoadCallback {
      override def onLoaded(a: AssetForUpload): Unit = {
        asset = Some(a)
        latch.countDown()

      }
      override def onFailed(): Unit = {
        println(s"transcode failed")
        latch.countDown()
      }
    })

    latch.await()
    Assert.assertTrue(asset.isDefined)
  }

}
