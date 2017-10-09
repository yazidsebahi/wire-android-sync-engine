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
import com.waz.api.NetworkMode.{OFFLINE, UNKNOWN}
import com.waz.api.impl.ErrorResponse
import com.waz.content.GlobalPreferences.BackendDrift
import com.waz.content.UserPreferences.LastStableNotification
import com.waz.content.{GlobalPreferences, UserPreferences}
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.ZMessaging.clock
import com.waz.service.{EventPipeline, NetworkModeService, ZmsLifeCycle}
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.PushNotificationsClient.{LoadNotificationsResponse, NotificationsResponse}
import com.waz.sync.client.{PushNotification, PushNotificationsClient}
import com.waz.threading.CancellableFuture.lift
import com.waz.threading.{CancellableFuture, SerialDispatchQueue}
import com.waz.utils.events._
import com.waz.utils.{RichInstant, _}
import com.waz.znet.Response.Status.NotFound
import org.threeten.bp.{Duration, Instant}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

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

  def onMissedCloudPushNotifications: EventStream[MissedPushes]
  def onFetchedPushNotifications:     EventStream[Seq[ReceivedPushData]]

  def syncHistory(reason: String): Future[Unit]

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

class PushServiceImpl(context:        Context,
                      userPrefs:      UserPreferences,
                      prefs:          GlobalPreferences,
                      receivedPushes: ReceivedPushStorage,
                      client:         PushNotificationsClient,
                      clientId:       ClientId,
                      accountId:      AccountId,
                      pipeline:       EventPipeline,
                      webSocket:      WebSocketClientService,
                      network:        NetworkModeService,
                      lifeCycle:      ZmsLifeCycle,
                      sync:           SyncServiceHandle) extends PushService { self =>
  import PushService._

  implicit val logTag: LogTag = s"${logTagFor[PushService]}#${accountId.str.take(8)}"
  private implicit val dispatcher = new SerialDispatchQueue(name = "PushService")
  private implicit val ec = EventContext.Global

  override val onMissedCloudPushNotifications = EventStream[MissedPushes]()
  override val onFetchedPushNotifications    = EventStream[Seq[ReceivedPushData]]()

  override val onHistoryLost = new SourceSignal[Instant] with BgEventSource
  override val pushConnected = Signal[Boolean]()
  override val processing = Signal(false)
  override def afterProcessing[T](f : => Future[T])(implicit ec: ExecutionContext): Future[T] = processing.filter(_ == false).head.flatMap(_ => f)
  override lazy val cloudPushNotificationsToProcess = Signal(Set[Uid]())

  private val beDriftPref = prefs.preference(BackendDrift)
  override val beDrift = beDriftPref.signal.disableAutowiring()

  private var fetchInProgress = Future.successful({})

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
        ws.onMessage.on(dispatcher) { content =>
          verbose(s"got websocket message: $content")
          content match {
            case NotificationsResponse(notifications@_*) =>
              verbose(s"got notifications from data: $content")
              fetchInProgress = if (fetchInProgress.isCompleted)
                Future(onPushNotifications(notifications))
              else
                fetchInProgress.flatMap(_ => Future(onPushNotifications(notifications)))
            case resp =>
              error(s"unexpected push response: $resp")
          }
        },
        ws.onError.on(dispatcher) { ex =>
          syncHistory("websocket error")
        }
      )
  }

  EventStream.union(
    cloudPushNotificationsToProcess.onChanged.filter(_.nonEmpty).map(_ => "cloud notifications"),
    webSocket.connected.onChanged.map(_ => "web socket connection change")
  ).on(dispatcher) { reason =>
    syncHistory(reason)
  }

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

  //expose loop to tests
  protected[push] val waitingForRetry = Signal(false).disableAutowiring()

  override def syncHistory(reason: String): Future[Unit] = {
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
        waitingForRetry ! true
        warn(s"Request failed due to $err: attempting to load last page (since id: $lastId) again")
        //We want to retry the download after the backoff is elapsed and the network is available,
        //OR on a network state change (that is not offline/unknown)
        //OR on a websocket state change
        val retry = Promise[Unit]()

        //TODO it would be nice to have a method on EventStream#onChanged which can return a future for the first event
        val currentNetwork = network.networkMode.currentValue.getOrElse(UNKNOWN)
        network.networkMode.filter(!Set(UNKNOWN, OFFLINE, currentNetwork).contains(_)).head.map(_ => retry.trySuccess({}))

        val currentWSState = webSocket.connected.currentValue.getOrElse(false)
        webSocket.connected.filter(_ != currentWSState).head.map(_ => retry.trySuccess({}))

        for {
          _ <- CancellableFuture.delay(syncHistoryBackoff.delay(attempts))
          _ <- lift(network.networkMode.filter(!Set(UNKNOWN, OFFLINE).contains(_)).head)
        } yield retry.trySuccess({})

        lift(retry.future).flatMap { _ =>
          waitingForRetry ! false
          load(lastId, attempts + 1)
        }
    }

    def syncHistory(lastId: Option[Uid]): Future[Unit] =
      for {
        Results(nots, time, historyLost) <- load(lastId).future
        _ <- if (historyLost) sync.performFullSync().map(_ => onHistoryLost ! clock.instant()) else Future.successful({})
        drift  <- beDrift.head
        nw     <- network.networkMode.head
        pushes <- receivedPushes.list()
        _ <- receivedPushes.removeAll(pushes.map(_.id))
        _ <- beDriftPref.mutate(v => time.map(clock.instant.until(_)).getOrElse(v))
        inBackground <- lifeCycle.uiActive.head
    } yield {
        if (nots.map(_.id).size > pushes.map(_.id).size) //we didn't get pushes for some returned notifications
          onMissedCloudPushNotifications ! MissedPushes(clock.instant + drift, nots.size - pushes.size, inBackground, nw, network.getNetworkOperatorName)

        if (pushes.nonEmpty)
          onFetchedPushNotifications ! pushes.map(p => p.copy(toFetch = Some(p.receivedAt.until(clock.instant + drift))))

        onPushNotifications(nots)
      }

    if (fetchInProgress.isCompleted) {
      verbose(s"Sync history in response to $reason")
      fetchInProgress = idPref().flatMap(syncHistory)
    }
    fetchInProgress
  }
}

object PushService {
  var syncHistoryBackoff: Backoff = new ExponentialBackoff(3.second, 15.seconds)
}