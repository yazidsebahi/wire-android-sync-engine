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

import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.model.AccountId
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.utils.events.{EventContext, Signal, SourceSignal}
import com.waz.utils.returning

import scala.concurrent.Future

trait ZmsLifeCycle extends EventContext {

  //There is a logged in account
  def loggedIn: Signal[Boolean]

  //logged in accounts, but no activity and app is in the background
  def idle: Signal[Boolean]

  //At least one account has some background activity going on
  def active: Signal[Boolean]

  //One account is currently logged-in and in the foreground
  //TODO - should UI be active if there are no accounts set to active? i.e. AppEntry?
  def uiActive: Signal[Boolean]

  def accLoggedIn(accountId: AccountId): Signal[Boolean]

  //The account is active and currently being displayed in the UI
  def accInForeground(accountId: AccountId): Signal[Boolean]

  def setLoggedIn(accs: Set[AccountId]): Unit
  def setActiveAccount(acc: Option[AccountId]): Unit

  def acquireSync(source: String = ""): Unit
  def acquirePush(source: String = ""): Unit
  def acquireUi(source: String = ""): Unit

  def releaseSync(source: String = ""): Unit
  def releasePush(source: String = ""): Unit
  def releaseUi(source: String = ""): Unit

  def withSync[Result](body: => Future[Result])(implicit source: LogTag): Future[Result]
  def withPush[Result](body: => Future[Result])(implicit source: LogTag): Future[Result]
  def withUi[Result](body: => Future[Result])(implicit source: LogTag): Future[Result]
}

object ZmsLifeCycle {
  trait LifeCycleState {
    val loggedIn: Set[AccountId]
  }

  object LifeCycleState {
    def apply(active: Option[AccountId], loggedIn: Set[AccountId], uiCount: Int, pushCount: Int): LifeCycleState = {
      if (loggedIn.isEmpty && active.isEmpty) Stopped: LifeCycleState
      else if (uiCount > 0) active.map(id => UiActive(id, loggedIn)).getOrElse(Active(loggedIn))
      else if (pushCount > 0) Active(loggedIn)
      else Idle(loggedIn)
    }
  }

  //No logged in accounts
  case object Stopped extends LifeCycleState {
    override val loggedIn = Set.empty[AccountId]
  }

  //Logged in accounts, but nothing to be done
  case class  Idle(loggedIn: Set[AccountId]) extends LifeCycleState

  //Logged in accounts, but the app can be running in the background. In this state, there may be an active account,
  //but since that account is not being displayed, it should have no preference to the others.
  //Note, this state will also represent the case where the UI is active, but no one of the current accounts is being displayed
  //(e.g., when switching accounts)
  case class  Active(loggedIn: Set[AccountId]) extends LifeCycleState

  //Logged in accounts, and one of them is currently being displayed in the UI
  case class  UiActive(active: AccountId, loggedIn: Set[AccountId]) extends LifeCycleState

}

class ZmsLifeCycleImpl extends ZmsLifeCycle {
  import ZmsLifeCycle._

  private implicit val dispatcher = new SerialDispatchQueue(name = "LifeCycleDispatcher")

  private val loggedInAccounts = Signal(Set.empty[AccountId])
  private val activeAccount    = Signal(Option.empty[AccountId])

  private val deviceState = Signal((0, 0, 0)) //syncCount, pushCount, uiCount

  private val lifeCycleState =
    (for {
      (_, p, u) <- deviceState
      li        <- loggedInAccounts
      a         <- activeAccount
    } yield LifeCycleState(a, li, u, p))
      .disableAutowiring()

  lifeCycleState(state => verbose(s"lifecycle state: $state"))(this)

  override val loggedIn = lifeCycleState.map(_ != Stopped).disableAutowiring()

  override val uiActive = lifeCycleState.map {
    case UiActive(_, _) => true
    case _              => false
  }.disableAutowiring()

  override val idle = lifeCycleState.map {
    case Idle(_) => true
    case _       => false
  }.disableAutowiring()

  override val active = lifeCycleState.map {
    case UiActive(_, _) | Active(_) => true
    case _ => false
  }.disableAutowiring()

  private var accLoggedInSignals = Map.empty[AccountId, Signal[Boolean]]
  override def accLoggedIn(accountId: AccountId) =
    returning(accLoggedInSignals.getOrElse(accountId, lifeCycleState.map(_.loggedIn.contains(accountId)))) { sig =>
      accLoggedInSignals += accountId -> sig
    }

  private var accInForegroundSignals = Map.empty[AccountId, Signal[Boolean]]
  override def accInForeground(accountId: AccountId) =
    returning(accInForegroundSignals.getOrElse(accountId, lifeCycleState.map {
      case UiActive(id, _) if id == accountId => true
      case _ => false
    })) (sig => accInForegroundSignals += accountId -> sig)

  override def setLoggedIn(accs: Set[AccountId]) = loggedInAccounts.publish(accs, dispatcher)

  override def setActiveAccount(accountId: Option[AccountId]) = activeAccount.publish(accountId, dispatcher)

  def acquireSync(source: LogTag = ""): Unit = acquire('sync, source)
  def acquirePush(source: LogTag = ""): Unit = acquire('push, source)
  def acquireUi(source: LogTag = ""): Unit = acquire('ui, source)

  def releaseSync(source: LogTag = ""): Unit = release('sync, source)
  def releasePush(source: LogTag = ""): Unit = release('push, source)
  def releaseUi(source: LogTag = ""): Unit = release('ui, source)

  private def acquire(name: Symbol, source: LogTag): Unit = {
    deviceState.mutate ({
      case (s, p, u) =>
        val (sync, push, ui) = name match {
          case 'sync => (s + 1, p, u)
          case 'push => (s, p + 1, u)
          case 'ui   => (s, p, u + 1)
          case _     => (s, p, u)
        }
        debug(s"acquire${name.name.capitalize}, syncCount: $sync, pushCount: $push, uiCount: $ui, source: '$source'")
        if ((sync + push + ui) == 1) onContextStart()
        (sync, push, ui)
    }, dispatcher)
  }

  private def release(name: Symbol, source: LogTag): Unit = {
    deviceState.mutate ({
      case (s, p, u) =>
        val id = name.name.capitalize
        val (predicate, sync, push, ui) = name match {
          case 'sync => (s > 0, s - 1, p, u)
          case 'push => (p > 0, s, p - 1, u)
          case 'ui   => (u > 0, s, p, u - 1)
          case _     => (true, s, p, u)
        }
        assert(predicate, s"release$id should be called exactly once for each acquire$id")
        debug(s"release$id syncCount: $sync, pushCount: $push, uiCount: $ui, source: '$source'")
        if ((sync + push + ui) == 1) onContextStart()
        (sync, push, ui)
    }, dispatcher)
  }

  def withSync[Result](body: => Future[Result])(implicit source: LogTag = ""): Future[Result] =
    withStateCounter(acquireSync(source), releaseSync(source))(body)

  def withPush[Result](body: => Future[Result])(implicit source: LogTag = ""): Future[Result] =
    withStateCounter(acquirePush(source), releasePush(source))(body)

  def withUi[Result](body: => Future[Result])(implicit source: LogTag = ""): Future[Result] =
    withStateCounter(acquireUi(source), releaseUi(source))(body)

  private def withStateCounter[Result](acquire: => Unit, release: => Unit)(body: => Future[Result]): Future[Result] = {
    acquire
    body.andThen { case _ =>
      release
    }
  }
}
