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

import java.io._
import java.util.concurrent.CountDownLatch

import com.waz.ZLog._
import com.waz.api.ZmsVersion
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.{Deprecated, IoUtils, RichFuture}

import scala.concurrent.Future

class ReportingService() {
  import ReportingService._
  private implicit val dispatcher = new SerialDispatchQueue(name = "ReportingService")
  private var reporters = Seq.empty[Reporter]
  
  def addStateReporter(report: PrintWriter => Future[Unit])(implicit tag: LogTag): Unit = Future {
    reporters = reporters :+ Reporter(tag, report) 
  }
  
  private[service] def generateStateReport(writer: PrintWriter) =
    Future { reporters } flatMap { rs =>
      RichFuture.processSequential(rs)(_.apply(writer))
    }
}

object ReportingService {
  implicit val tag: LogTag = logTagFor[ReportingService]
  import com.waz.threading.Threading.Implicits.Background

  case class Reporter(name: String, report: PrintWriter => Future[Unit]) {
    
    def apply(writer: PrintWriter) = {
      writer.println(s"\n###### $name:")
      report(writer)
    }
  }

  lazy val metadata = ZMessaging.currentGlobal.metadata

  val VersionReporter = Reporter("Wire", { writer =>
    import android.os.Build._
    Future.successful {
      writer.println(s"package: ${ZMessaging.context.getPackageName}")
      writer.println(s"app version: ${metadata.appVersion}")
      writer.println(s"zms version: ${ZmsVersion.ZMS_VERSION}")
      writer.println(s"device: $MANUFACTURER $PRODUCT | $MODEL | $BRAND | $ID")
      writer.println(s"version: ${VERSION.RELEASE} | ${VERSION.CODENAME}")
    }
  })

  val ZUsersReporter = Reporter("ZUsers", { writer =>
    writer.println(s"current: ${ZMessaging.currentInstance.currentUser.currentValue.flatten}")
    ZMessaging.currentGlobal.users.listUsers() map { all =>
      all foreach { u =>
        writer.println(u.toString)
      }
    }
  })

  val GcmRegistrationReporter = Reporter("Gcm", { writer =>
    ZMessaging.currentGlobal.prefs.withPreferences(GcmGlobalService.GcmRegistration.apply) map { writer.println }
  })

  val LogCatReporter = Reporter("LogCat", { writer =>

    val latch = new CountDownLatch(2)

    def writeAll(input: InputStream): Unit = try {
      IoUtils.withResource(new BufferedReader(new InputStreamReader(input))) { reader =>
        Iterator.continually(reader.readLine()).takeWhile(_ != null).foreach(writer.println)
      }
    } finally {
      latch.countDown()
    }

    Future {
      import scala.sys.process._
      Process(Seq("logcat", "-d", "-v", "time")).run(new ProcessIO({in => latch.await(); in.close() }, writeAll, writeAll))
      latch.await()
    } (Threading.IO)
  })

  def zmessagingReporter(zms: ZMessaging) = Reporter(s"ZMessaging[${zms.user}]", zms.reporting.generateStateReport)

  def generateReport(): Future[File] = {
    val context = ZMessaging.context
    val name = s"se_report_${System.currentTimeMillis()}.txt"

    @SuppressWarnings(Array("deprecation"))
    lazy val writer = new PrintWriter(context.openFileOutput(name, Deprecated.MODE_WORLD_READABLE))

    RichFuture.processSequential(VersionReporter +: GcmRegistrationReporter +: ZUsersReporter +: ZMessaging.currentInstance.instanceMap.values.map(zmessagingReporter).toSeq :+ LogCatReporter) { reporter =>
      reporter.apply(writer)
    } map { _ =>
      writer.close()
      new File(context.getFilesDir, name)
    }
  }
}
