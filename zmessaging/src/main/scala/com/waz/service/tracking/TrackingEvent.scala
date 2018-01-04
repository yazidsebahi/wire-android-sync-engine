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
package com.waz.service.tracking

import java.lang.Math.max

import com.waz.ZLog.LogTag
import com.waz.api.{EphemeralExpiration, NetworkMode}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{ConversationData, Mime}
import com.waz.service.push.ReceivedPushData
import com.waz.utils.returning
import org.json
import org.json.JSONObject
import org.threeten.bp.{Duration, Instant}

import scala.util.Try

trait TrackingEvent {
  val name: String
  val props: Option[JSONObject]
}

trait OptEvent extends TrackingEvent {
  override val props = None
}

case object OptInEvent  extends OptEvent { override val name = s"settings.opted_in_tracking" }
case object OptOutEvent extends OptEvent { override val name = s"settings.opted_out_tracking" }

case class ContributionEvent(action: ContributionEvent.Action, conversationType: ConversationType, ephExp: EphemeralExpiration, withBot: Boolean) extends TrackingEvent {
  override val name = "contributed"

  override val props = Some(returning(new JSONObject()) { o =>
    o.put("action", action.name)
    o.put("conversation_type", if (conversationType == ConversationType.Group) "group" else "one_to_one")
    o.put("with_bot", withBot)
    o.put("is_ephemeral", ephExp != EphemeralExpiration.NONE) //TODO is this flag necessary?
    o.put("ephemeral_expiration", ephExp.duration().toSeconds.toString)
  })
}

object ContributionEvent {

  case class Action(name: String)

  object Action {
    lazy val Text = Action("text")
    lazy val Ping = Action("ping")
    lazy val AudioCall = Action("audio_call")
    lazy val VideoCall = Action("video_call")
    lazy val Photo = Action("photo")
    lazy val Audio = Action("audio")
    lazy val Video = Action("video")
    lazy val File = Action("file")
    lazy val Location = Action("location")
  }

  def apply(action: Action, conv: ConversationData, withOtto: Boolean): ContributionEvent =
    ContributionEvent(action, conv.convType, conv.ephemeral, withOtto)

  def fromMime(mime: Mime) = {
    import Action._
    mime match {
      case Mime.Image() => Photo
      case Mime.Audio() => Audio
      case Mime.Video() => Video
      case _ => File
    }
  }
}

// the throwable will not be serialized, but might be used to report an exception
trait ThrowableEvent extends TrackingEvent {
  def throwable: Option[Throwable]
}

//only for exceptions that actually crash the app
case class CrashEvent(crashType: String, crashDetails: String, override val throwable: Option[Throwable] = None) extends ThrowableEvent {
  override val name = "crash"
  override val props = Some(returning(new JSONObject()) { o =>
    o.put("crashType", crashType)
    o.put("crashDetails", crashDetails)
  })
}

// for all other exceptions
case class ExceptionEvent(exceptionType: String, exceptionDetails: String, description: String, override val throwable: Option[Throwable] = None)(implicit val tag: LogTag) extends ThrowableEvent {
  override val name = "debug.exception"
  override val props = Some(returning(new JSONObject()) { o =>
    o.put("exceptionType", exceptionType)
    o.put("exceptionDetails", exceptionDetails)
    o.put("description", description)
  })
}

case class MissedPushEvent(time:            Instant,
                           countMissed:     Int,
                           inBackground:    Boolean, //will help rull out false-positivie - missed pushes in foreground may be legitimate misses!
                           networkMode:     NetworkMode,
                           networkOperator: String) extends TrackingEvent {
  override val name = "debug.push_missed"
  override val props = Some(returning(new JSONObject()) { o =>
    o.put("time", time.toString)
    o.put("missed_count", countMissed)
    o.put("in_background", inBackground)
    o.put("network_mode", networkMode)
  })
}

case class ReceivedPushEvent(p: ReceivedPushData) extends TrackingEvent {
  override val name = "debug.push_received"

  def secondsAndMillis(d: Duration): Double = max(d.toMillis.toDouble / 1000, 0)

  override val props = Some(returning(new JSONObject()) { o =>
    o.put("since_sent_seconds", secondsAndMillis(p.sinceSent))
    o.put("received_at", p.receivedAt.toString)
    o.put("network_mode", p.networkMode)
    o.put("network_operator", p.networkOperator)
    o.put("is_device_idle", p.isDeviceIdle)
    p.toFetch.foreach(d => o.put("to_fetch_seconds", secondsAndMillis(d)))
  })
}

case class LoggedOutEvent(reason: String) extends TrackingEvent {
  override val name = "account.logged_out"
  override val props = Some(returning(new JSONObject()) { o =>
    o.put("reason", reason)
  })
}

object LoggedOutEvent {
  val RemovedClient = "removed_client"
  val InvalidCredentials = "invalid_credentials"
  val SelfDeleted = "self_deleted"
  val ResetPassword = "reset_password"
  val Manual = "manual"
}

case class AVSMetricsEvent(jsonStr: String) extends TrackingEvent {
  override val name = "calling.avs_metrics_ended_call"
  override val props = Try(new json.JSONObject(jsonStr)).toOption
}
