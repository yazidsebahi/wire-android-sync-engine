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
package com.waz.service.otr

import java.io.File

import android.content.Context
import com.waz.ZLog._
import com.waz.model.ZUserId
import com.waz.service.MetaDataService
import com.waz.threading.{Threading, SerialDispatchQueue}
import com.waz.utils._
import com.wire.cryptobox.CryptoBox

import scala.concurrent.Future

class CryptoBoxService(context: Context, userId: ZUserId, metadata: MetaDataService) {
  import CryptoBoxService._
  private implicit val dispatcher = new SerialDispatchQueue(Threading.IO)

  private[service] lazy val cryptoBoxDir = returning(new File(new File(context.getFilesDir, metadata.cryptoBoxDirName), userId.str))(_.mkdirs())

  private var _cryptoBox = Option.empty[CryptoBox]

  def cryptoBox = Future {
    _cryptoBox.orElse {
      returning(load) { _cryptoBox = _ }
    }
  }

  private def load = LoggedTry {
    cryptoBoxDir.mkdirs()
    CryptoBox.open(cryptoBoxDir.getAbsolutePath)
  } .toOption

  def apply[A](f: CryptoBox => Future[A]): Future[Option[A]] = cryptoBox flatMap {
    case None => Future successful None
    case Some(cb) => f(cb) map (Some(_))
  }

  def deleteCryptoBox() = Future {
    _cryptoBox.foreach(_.close())
    _cryptoBox = None
    IoUtils.deleteRecursively(cryptoBoxDir)
  }

  def close() = Future {
    _cryptoBox.foreach(_.close())
    _cryptoBox = None
  }
}

object CryptoBoxService {
  private implicit val Tag: LogTag = logTagFor[CryptoBoxService]
}
