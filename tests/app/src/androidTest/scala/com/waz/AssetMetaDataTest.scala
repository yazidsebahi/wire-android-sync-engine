package com.waz

import android.net.Uri
import android.test._
import com.waz.model.AssetMetaData
import com.waz.service.ZMessaging
import com.waz.testapp.EmptyTestActivity
import com.waz.threading.Threading
import org.junit.Assert

class AssetMetaDataTest extends ActivityInstrumentationTestCase2[EmptyTestActivity](classOf[EmptyTestActivity]) {

  def context = getActivity.getApplication

  override def setUp(): Unit = {
    super.setUp()
    Threading.AssertsEnabled = false
    ZMessaging.onCreate(getActivity.getApplication)
  }

  def testAudioDurationLoadingFromUri() = {
    val meta = AssetMetaData.Audio(context, Uri.parse("content://com.waz.test/32kbps.m4a"))

    Assert.assertEquals(309L, meta.map(_.duration.getSeconds).getOrElse(0L))
  }
}
