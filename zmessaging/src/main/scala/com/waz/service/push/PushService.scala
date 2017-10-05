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
import com.waz.api.NetworkMode.{OFFLINE, UNKNOWN}
import com.waz.api.impl.ErrorResponse
import com.waz.content.UserPreferences
import com.waz.content.UserPreferences.LastStableNotification
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.ZMessaging.clock
import com.waz.service.{EventPipeline, ZMessaging}
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.{PushNotification, PushNotificationsClient}
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.{Duration, Instant}
import com.waz.utils.RichInstant
import com.waz.utils.RichThreetenBPDuration

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import com.waz.sync.client.PushNotificationsClient.{LoadNotificationsResponse, NotificationsResponse}
import com.waz.znet.Response.Status.NotFound

/** PushService handles notifications coming from FCM, WebSocket, and fetch.
  * We assume FCM notifications are unreliable, so we use them only as information that we should perform a fetch (syncHistory).
  * A notification from the web socket may trigger a fetch on error. When fetching we ask BE to send us all notifications since
  * a given lastId - it may take time and we even may find out that we lost some history (lastId not found) which triggers slow sync.
  * So, it may happen that during the fetch new notifications will arrive through the web socket. Their processing should be
  * performed only after the fetch is done. For that we use a serial dispatch queue and we process notifications in futures,
  * which are put at the end of the queue, essentially making PushService synchronous.
  * Also, we need to handle fetch failures. If a fetch fails, we want to repeat it - after some time, or on network change.
  * In such case, new web socket notifications waiting to be processed should be dismissed. The new fetch will (if successful)
  * receive them in the right order.
  */

trait PushService {

  def cloudPushNotificationsToProcess: SourceSignal[Set[Uid]]

  def onMissedCloudPushNotification: EventStream[MissedPush]
  def onReceivedPushNotification:    EventStream[ReceivedPush]

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

  override val onMissedCloudPushNotification = EventStream[MissedPush]()
  override val onReceivedPushNotification    = EventStream[ReceivedPush]()

  override val onHistoryLost = new SourceSignal[Instant] with BgEventSource
  override val pushConnected = Signal[Boolean]()
  override val processing = Signal(false)
  override def afterProcessing[T](f : => Future[T])(implicit ec: ExecutionContext): Future[T] = processing.filter(_ == false).head.flatMap(_ => f)
  override lazy val cloudPushNotificationsToProcess = Signal(Set[Uid]())

  override val beDrift = Signal(Duration.ZERO).disableAutowiring()

  private var fetchInProgress = CancellableFuture.successful({})

  private lazy val idPref = userPrefs.preference(LastStableNotification)

  webSocket.connected { pushConnected ! _ }

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
              verbose(s"got notifications from data: $content")
              fetchInProgress = if (fetchInProgress.isCompleted)
                CancellableFuture(onPushNotifications(notifications))
              else
                fetchInProgress.flatMap(_ => CancellableFuture(onPushNotifications(notifications)))
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
      verbose("onConnected")
      syncHistory()
      connectedPushPromise.trySuccess(self)
    case false =>
      verbose("onDisconnected")
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
      if (notifications.nonEmpty && fetchInProgress.isCompleted){
        verbose("Sync history in response to cloud message notification")
        syncHistory()
      }
  }

  lazy val outstandingNot = userPrefs.preference(UserPreferences.OutstandingPush)
  private def onPushNotifications(allNs: Seq[PushNotification]): Unit = if (allNs.nonEmpty) {
    verbose(s"onPushNotifications: ${allNs.size}")

    val ns = allNs.filter(_.hasEventForClient(clientId))

    val p = wakeLock.async {
      processing ! true
      pipeline {
        returning(ns.flatMap(_.eventsForClient(clientId))) {
          _.foreach { ev => ev.withCurrentLocalTime(); verbose(s"event: $ev") }
        }
      }
      .map { _ => cloudPushNotificationsToProcess mutate (_ -- ns.map(_.id).toSet) }
      .andThen { case _ => processing ! false }
    }

    ns.lift(ns.lastIndexWhere(!_.transient)).foreach { n => p.onSuccess { case _ => idPref := Some(n.id) } }
  }

  protected lazy val wakeLock: WakeLock = new WakeLockImpl(context) // to be overriden in tests

  case class Results(notifications: Vector[PushNotification], time: Option[Instant], historyLost: Boolean)

  private def futureHistoryResults(notifications: Vector[PushNotification] = Vector.empty,
                                   time: Option[Instant] = None,
                                   historyLost: Boolean = false) =
    CancellableFuture.successful(Results(notifications, time, historyLost))

  webSocket.network.networkMode {
    case OFFLINE|UNKNOWN =>
    case _ if !fetchInProgress.isCompleted => syncHistory() // it will restart the fetch
    case _ =>
  }

  override def syncHistory(): Future[Unit] = {
    def load(lastId: Option[Uid], attempts: Int = 0): CancellableFuture[Results] = client.loadNotifications(lastId, clientId).flatMap {
      case Right(LoadNotificationsResponse(nots, false, time)) => futureHistoryResults(nots, time)
      case Right(LoadNotificationsResponse(nots, true, time)) =>
        load(nots.lastOption.map(_.id)).flatMap { results =>
          futureHistoryResults(nots ++ results.notifications, if (results.time.isDefined) results.time else time, results.historyLost)
        }
      case Left(ErrorResponse(NotFound, _, _)) if lastId.isDefined =>
        warn(s"/notifications failed with 404, history lost")
        load(None).flatMap { case Results(nots, time, _) => futureHistoryResults(nots, time, historyLost = true) }
      case Left(err) =>
        warn(s"Request failed due to $err: attempting to load last page (since id: $lastId) again")
        CancellableFuture.delay(syncHistoryBackoff.delay(attempts)).flatMap { _ => load(lastId, attempts + 1) }
      // fetch failed, schedule retry and turn on retry on network change
    }

    def syncHistory(lastId: Option[Uid]): Future[Unit] =
      for {
        Results(nots, time, historyLost) <- load(lastId).future
        _ <- if (historyLost) sync.performFullSync().map(_ => onHistoryLost ! clock.instant()) else Future.successful({})
        _ <- outstandingNot.mutate {
          case Some(n) =>
            if (nots.map(_.id).contains(n.id)) onReceivedPushNotification ! n.copy(toFetch = Some(n.receivedAt.until(clock.instant)))
            //TODO what would the else here mean? We received a notification and did a fetch, but that notification was not in the result?
            None
          case None =>
            //TODO, what else do we want to track here?
            if (nots.nonEmpty) onMissedCloudPushNotification ! MissedPush(clock.instant)
            None
        }
    } yield {
      onPushNotifications(nots)
      time.foreach { time => beDrift ! Instant.now.until(time) }
    }

    returning( Future(idPref()).flatten flatMap { syncHistory } ) { f => // future and flatten ensure that there is no race with onPushNotifications
      if (!fetchInProgress.isCompleted) fetchInProgress.cancel()
      // a new fetch was ordered so we cancel the old one if it's still in progress, together with any ws notifications processing attached to it
      fetchInProgress = CancellableFuture.lift(f)
    }
  }
}

object PushService {
  var syncHistoryBackoff: Backoff = new ExponentialBackoff(3.second, 15.seconds)
}