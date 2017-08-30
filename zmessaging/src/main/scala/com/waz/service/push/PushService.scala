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
package com.waz.service.push

import android.content.Context
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api.impl.ErrorResponse
import com.waz.api.impl.ErrorResponse.{ConnectionErrorCode, TimeoutCode}
import com.waz.content.UserPreferences
import com.waz.content.UserPreferences.LastStableNotification
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.ZMessaging.clock
import com.waz.service.EventPipeline
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.{PushNotification, PushNotificationsClient}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.{Duration, Instant}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import com.waz.sync.client.PushNotificationsClient.{LoadNotificationsResponse, NotificationsResponse}

trait PushService {

  def cloudPushNotificationsToProcess: SourceSignal[Set[Uid]]

  def syncHistory(): Future[Unit]

  def onHistoryLost: SourceSignal[Instant] with BgEventSource
  def pushConnected: Signal[Boolean]
  def processing: Signal[Boolean]
  def afterProcessing[T](f : => Future[T])(implicit ec: ExecutionContext): Future[T]

  /**
    * Drift to the BE time at the moment we fetch notifications
    * Used for calling (time critical) messages that can't always rely on local time, since the drift can be
    * greater than the window in which we need to respond to messages
    */
  def beDrift: Signal[Duration]

  def onPushNotification(n: PushNotification): Unit // used in tests
  def onPushNotifications(allNs: Seq[PushNotification]): Unit
  def lastNotificationId(): Future[Option[Uid]]
  def updateLastNotificationId(id: Uid): Future[Unit]
  def resetLastNotificationId(): Future[Unit] // used in tests

}

class PushServiceImpl(context: Context,
                      userPrefs: UserPreferences,
                      client: PushNotificationsClient,
                      clientId: ClientId,
                      pipeline: EventPipeline,
                      webSocket: WebSocketClientService,
                      sync: SyncServiceHandle
                     ) extends PushService { self =>
  import PushService._

  private implicit val dispatcher = new SerialDispatchQueue(name = "PushService")
  private implicit val ec = EventContext.Global

  private var connectedPushPromise = Promise[PushServiceImpl]()

  override val onHistoryLost = new SourceSignal[Instant] with BgEventSource
  override val pushConnected = Signal[Boolean]()
  override val processing = Signal(false)
  override def afterProcessing[T](f : => Future[T])(implicit ec: ExecutionContext): Future[T] = processing.filter(_ == false).head.flatMap(_ => f)
  override val cloudPushNotificationsToProcess = Signal(Set[Uid]())

  override val beDrift = Signal(Duration.ZERO).disableAutowiring()

  webSocket.connected {
    pushConnected ! _
  }

  private var subs = Seq.empty[Subscription]

  webSocket.client.on(dispatcher) {
    case None =>
      subs.foreach(_.destroy())
      subs = Nil

    case Some(ws) =>
      subs.foreach(_.destroy())
      subs = Seq(
        ws.onMessage { content =>
          verbose(s"got websocket message: $content")
          content match {
            case NotificationsResponse(notifications@_*) =>
              debug(s"got notifications from data: $content")
              onPushNotifications(notifications)
            case resp =>
              error(s"unexpected push response: $resp")
          }
        },
        ws.onError { ex =>
          warn(s"some PushNotification processing failed, will sync history", ex)
          syncHistory()
        }
      )
  }

  webSocket.connected {
    case true =>
      debug("onConnected")
      syncHistory()
      connectedPushPromise.trySuccess(self)
    case false =>
      debug("onDisconnected")
      syncHistory()
      connectedPushPromise.tryFailure(new Exception("onDisconnected"))
      connectedPushPromise = Promise()
  }

  //no need to sync history on cloud messaging if websocket is connected
  webSocket.connected.flatMap {
    case false => cloudPushNotificationsToProcess
    case _ => Signal.empty[Set[Uid]]
  }.on(dispatcher) {
    notifications =>
      if (notifications.nonEmpty){
        verbose("Sync history in response to cloud message notification")
        syncHistory()
      }
  }

  override def onPushNotification(n: PushNotification) = onPushNotifications(Seq(n)) //used in tests

  override def onPushNotifications(allNs: Seq[PushNotification]): Unit = if (allNs.nonEmpty) {
    debug(s"gotPushNotifications: ${allNs.size}")

    val ns = allNs.filter(_.hasEventForClient(clientId))

    val processing = processNotifications(ns) //careful not to inline this - needs to start executing whether transient or not!
    ns.lift(ns.lastIndexWhere(!_.transient)).foreach { n => processing.onSuccess { case _ => updateLastNotificationId(n.id) } }
  }

  protected lazy val wakeLock: WakeLock = new WakeLockImpl(context) // to be overriden in tests

  private def processNotifications(notifications: Seq[PushNotification]) = wakeLock.async {
    processing ! true
    pipeline {
      returning(notifications.flatMap(_.eventsForClient(clientId))) {
        _.foreach { ev =>
          ev.withCurrentLocalTime()
          verbose(s"event: $ev")
        }
      }
    }.map {
      _ => cloudPushNotificationsToProcess mutate (_ -- notifications.map(_.id).toSet)
    }.andThen {
      case _ => processing ! false
    }
  }

  onHistoryLost.on(dispatcher) { _ => sync.performFullSync() }

  override def syncHistory() = {
    def load(lastId: Option[Uid], attempts: Int = 0): CancellableFuture[(Vector[PushNotification], Boolean, Option[Instant])] = client.loadNotifications(lastId, clientId).flatMap {
      case Right( LoadNotificationsResponse(nots, false, time) ) => CancellableFuture.successful((nots, false, time))
      case Right( LoadNotificationsResponse(nots, true, time) ) => load(nots.lastOption.map(_.id)).flatMap {
        case (moreNots, historyLost, newTime) => CancellableFuture.successful((nots ++ moreNots, historyLost, if (newTime.isDefined) newTime else time))
      }
      case Left(ErrorResponse(TimeoutCode | ConnectionErrorCode, _, _)) =>
        if (attempts >= backoff.maxRetries) {
          warn(s"/notifications failed due to timeout or connection error. Do nothing and wait for next notification")
          CancellableFuture.successful((Vector.empty, false, None))
        } else {
          warn(s"Request from backend timed out: attempting to load last page (since id: $lastId) again")
          CancellableFuture.delay(backoff.delay(attempts)).flatMap(_ => load(lastId, attempts + 1)) //try last page again
        }
      case Left(err) =>
        warn(s"/notifications failed with unexpected error: $err, potentially history lost")
        CancellableFuture.successful((Vector.empty, true, None))
    }

    lastNotificationId() flatMap { lastId =>
      load(lastId).map {
        case (notifications, historyLost, beTime) =>
          onPushNotifications(notifications)
          beTime.foreach { time => beDrift ! Instant.now.until(time) }
          if (historyLost) {
            onHistoryLost ! clock.instant()
          }
      }
    }
  }

  private lazy val idPref = userPrefs.preference(LastStableNotification)
  override def lastNotificationId(): Future[Option[Uid]] = Future(idPref()).flatten // future and flatten ensure that there is no race with onNotification
  override def updateLastNotificationId(id: Uid): Future[Unit] = { idPref := Some(id) }
  override def resetLastNotificationId(): Future[Unit] = { idPref := None }
}

object PushService {
  val backoff = new ExponentialBackoff(3.second, 15.seconds)
}