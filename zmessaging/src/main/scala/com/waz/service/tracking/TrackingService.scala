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

import com.waz.ZLog.LogTag
import com.waz.api.EphemeralExpiration
import com.waz.content.{MembersStorage, UsersStorage}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.service.tracking.ContributionEvent.fromMime
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.{EventContext, EventStream}

import scala.annotation.tailrec
import scala.concurrent.Future.successful
import scala.concurrent.{ExecutionContext, Future}

trait TrackingService {
  def events: EventStream[(Option[ZMessaging], TrackingEvent)]

  def track(event: TrackingEvent, accountId: Option[AccountId] = None): Unit

  def loggedOut(reason: String, accountId: AccountId): Unit = track(LoggedOutEvent(reason), Some(accountId))
  def optIn(): Unit = track(OptEvent(true))
  def optOut(): Unit = track(OptEvent(false))
  def contribution(action: ContributionEvent.Action): Unit
  def assetContribution(assetId: AssetId, accountId: AccountId): Unit
  def exception(e: Throwable, description: String, accountId: Option[AccountId] = None)(implicit tag: LogTag): Unit
  def crash(e: Throwable): Unit
}

class TrackingServiceImpl(zmsProvider: TrackingService.ZmsProvider = TrackingService.defaultZmsProvider) extends TrackingService {
  import TrackingService._

  val events = EventStream[(Option[ZMessaging], TrackingEvent)]()

  override def track(event: TrackingEvent, accountId: Option[AccountId] = None): Unit = (accountId match {
    case Some(id) => zmsProvider(id)
    case _        => zmsProvider.current
  }).map { events ! _ -> event }

  override def contribution(action: ContributionEvent.Action): Unit = zmsProvider.current.map {
    case Some(z) =>
      for {
        Some(convId) <- z.convsStats.selectedConversationId.head
        Some(conv) <- z.convsStorage.get(convId)
        isBot <- TrackingService.isBot(conv, z.usersStorage)
        convType <- TrackingService.convType(conv, z.membersStorage)
      } {
        events ! Option(z) -> ContributionEvent(action, convType, conv.ephemeral, isBot)
      }
    case None =>
      events ! None -> ContributionEvent(action, ConversationData.ConversationType.Unknown, EphemeralExpiration.NONE, withBot = false)
  }

  override def exception(e: Throwable, description: String, accountId: Option[AccountId] = None)(implicit tag: LogTag): Unit = {
    val cause = rootCause(e)
    track(ExceptionEvent(cause.getClass.getSimpleName, details(cause), description, throwable = Some(e)), accountId)
  }

  override def crash(e: Throwable): Unit = {
    val cause = rootCause(e)
    track(CrashEvent(cause.getClass.getSimpleName, details(cause), throwable = Some(e)))
  }

  @tailrec
  private def rootCause(e: Throwable): Throwable = Option(e.getCause) match {
    case Some(cause) => rootCause(cause)
    case None => e
  }

  private def details(rootCause: Throwable) = {
    val stack = rootCause.getStackTrace
    if (stack != null && stack.nonEmpty) stack(0).toString else ""
  }

  override def assetContribution(assetId: AssetId, accountId: AccountId): Unit = zmsProvider(accountId).map {
    case Some(z) =>
      for {
        Some(msg)   <- z.messagesStorage.get(MessageId(assetId.str))
        Some(conv)  <- z.convsContent.convById(msg.convId)
        Some(asset) <- z.assetsStorage.get(assetId)
        convType    <- TrackingService.convType(conv, z.membersStorage)
        isBot       <- TrackingService.isBot(conv, z.usersStorage)
      } yield track(ContributionEvent(fromMime(asset.mime), convType, msg.ephemeral, isBot), Some(accountId))
    case _ => //
  }
}

object TrackingService {
  implicit val dispatcher = new SerialDispatchQueue(name = "TrackingService")
  private[waz] implicit val ec: EventContext = EventContext.Global

  trait ZmsProvider {
    def current: Future[Option[ZMessaging]]
    def apply(accountId: AccountId): Future[Option[ZMessaging]]
  }

  val defaultZmsProvider = new ZmsProvider {
    override def apply(accountId: AccountId): Future[Option[ZMessaging]] = ZMessaging.accountsService.flatMap(_.zms(accountId).head)
    override def current: Future[Option[ZMessaging]] = ZMessaging.accountsService.flatMap(_.activeZms.head)
  }

  def exception(e: Throwable, description: String, accountId: Option[AccountId] = None)(implicit tag: LogTag): Unit =
    ZMessaging.globalModule.map(_.trackingService.exception(e, description, accountId))

  trait NoReporting { self: Throwable => }

  private[waz] def isBot(conv: ConversationData, users: UsersStorage): Future[Boolean] =
    if (conv.convType == ConversationType.OneToOne) users.get(UserId(conv.id.str)).map(_.exists(_.isWireBot))
    else successful(false)

  //TODO remove workarounds for 1:1 team conversations when supported on backend
  private[waz] def convType(conv: ConversationData, membersStorage: MembersStorage)(implicit executionContext: ExecutionContext): Future[ConversationType] =
    if (conv.team.isEmpty) Future.successful(conv.convType)
    else membersStorage.getByConv(conv.id).map(_.map(_.userId).size > 2).map {
      case true => ConversationType.Group
      case _ => ConversationType.OneToOne
    }

  case class AssetTrackingData(conversationType: ConversationType, withOtto: Boolean, expiration: EphemeralExpiration, assetSize: Long, mime: Mime)
}

