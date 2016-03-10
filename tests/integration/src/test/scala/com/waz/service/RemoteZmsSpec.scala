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
package com.waz.service

import com.waz.api._
import com.waz.api.impl.{EmailCredentials, ZMessagingApi}
import com.waz.model.{EmailAddress, RConvId}
import com.waz.service.PreferenceService.Pref
import com.waz.service.ZMessaging._
import com.waz.testutils.Implicits._
import com.waz.threading.Threading
import com.waz.ui.UiModule
import org.scalatest.{BeforeAndAfterAll, RobolectricTests, Suite}

import scala.concurrent.{Future, Promise}
import scala.util.Random

class RemoteZms(ui: UiModule) extends ZMessagingApi()(ui) {
  import Threading.Implicits.Background

  onCreate(ui.context)
  onResume()

  def login(email: String, pass: String): Future[Unit] = {
    val p = Promise[Unit]()
    onInit(new InitListener {
      override def onInitialized(user: Self): Unit = {
        login(EmailCredentials(EmailAddress(email), Some(pass)), new LoginListener {
          override def onSuccess(user: Self): Unit = p.trySuccess(())

          override def onFailed(code: Int, message: String, label: String): Unit = p.tryFailure(new Exception(s"login failed: $code, $message, $label"))
        })
      }
    })
    p.future.onComplete(res => println(s"login $email, completed: $res"))
    p.future
  }

  def findConv(id: RConvId): Future[IConversation] = {
    val p = Promise[IConversation]()
    val convs = getConversations
    var listener: UpdateListener = null
    def check() =
      convs.find(_.data.remoteId == id) foreach { conv =>
        convs.removeUpdateListener(listener)
        p.trySuccess(conv)
      }
    listener = new UpdateListener {
      override def updated(): Unit = check()
    }
    convs.addUpdateListener(listener)
    check()
    p.future
  }

  def postMessage[A](conv: RConvId, msg: MessageContent[A]) = findConv(conv).map(_.sendMessage(msg))
}

trait RemoteZmsSpec extends RobolectricTests with BeforeAndAfterAll { suite: Suite with ApiSpec =>

  def zmsFactory(dataTag: String = Random.nextInt().toHexString): Factory = new ZMessaging(_, _, _, dataTag) {
    override def metadata: MetaDataService = new MetaDataService(context) {
      override val cryptoBoxDirName: String = "otr_" + dataTag
    }
  }


  override lazy val zmessagingFactory: Factory = zmsFactory()

  override protected def beforeAll(): Unit = {
    Threading.AssertsEnabled = false
    super.beforeAll()
  }

  def createRemoteZms(dataTag: String = Random.nextInt().toHexString) = new RemoteZms(new UiModule(new InstanceService(context, globalModule, zmsFactory(dataTag)) {
    override val currentUserPref: Pref[String] = globalModule.prefs.preferenceStringSignal("current_user_" + dataTag)
  }))
}
