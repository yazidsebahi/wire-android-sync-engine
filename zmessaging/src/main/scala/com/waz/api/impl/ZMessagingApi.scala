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

import android.content.Context
import com.waz.ZLog.ImplicitTag._
import com.waz.ZLog._
import com.waz.content.Uris
import com.waz.service.AccountManager
import com.waz.ui.UiModule
import com.waz.utils.events.EventContext

import scala.concurrent.Future

class ZMessagingApi(implicit val ui: UiModule) extends com.waz.api.ZMessagingApi {

  private[waz] var account: Option[AccountManager] = None
  private[waz] def zmessaging = account match {
    case Some(acc) => acc.getZMessaging
    case None => Future successful None
  }

  private var context: Context = _
  private var resumeCount = 0
  private var createCount = 0

  private val accounts = ui.accounts

  accounts.activeAccountManager.onUi { setAccount } (EventContext.Global)

  override def onCreate(context: Context): Unit = {
    verbose(s"onCreate $context, count: $createCount")
    createCount += 1
    if (this.context == null) {
      this.context = context.getApplicationContext
    }
    ui.onCreate(context)
  }

  private def setAccount(zms: Option[AccountManager]): Unit = if (account != zms) account = zms

  override def onResume(): Unit = {
    debug("onResume")
    resumeCount += 1
    ui.onResume()
  }

  override def onPause(): Unit = {
    debug("onPause")
    assert(resumeCount > 0, "onPause should be called exactly once for every onResume")
    resumeCount -= 1
    ui.onPause()
  }

  override def onDestroy(): Unit = {
    debug("onDestroy")
    assert(createCount > 0, "onDestroy should be called exactly once for every onCreate")
    assert(resumeCount == 0, s"onDestroy() was called before onPause(), this means there is some error in lifecycle callbacks")
    ui.onDestroy()
    account = None
    createCount -= 1
  }

  override def getSelf: Self = ui.users.selfUser

  override def getErrors: ErrorsList = ui.cached(Uris.ErrorsUri, new ErrorsList)

  override def getConnectionIndicator = new ConnectionIndicator()
}
