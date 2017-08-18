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
import com.waz.content.{MembersStorage, MessagesStorage, SearchQueryCacheStorage, UsersStorage}
import com.waz.model.SearchQuery.{Recommended, RecommendedHandle}
import com.waz.model.UserData.{ConnectionStatus, UserDataDao}
import com.waz.model.{SearchQuery, _}
import com.waz.service.conversation.ConversationsUiService
import com.waz.service.teams.TeamsService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

case class SearchState(filter: String, hasSelectedUsers: Boolean, addingToConversation: Option[ConvId]){
  def shouldShowTopUsers(isTeam: Boolean) = empty && !isTeam && addingToConversation.isEmpty
  def shouldShowAbContacts(isTeam: Boolean) = addingToConversation.isEmpty && !hasSelectedUsers && !isTeam
  lazy val shouldShowGroupConversations = (if (isHandle) stripSymbol.length > 1 else !empty) && !hasSelectedUsers && addingToConversation.isEmpty
  lazy val shouldShowDirectorySearch = !empty && !hasSelectedUsers && addingToConversation.isEmpty

  lazy val empty = filter.isEmpty
  lazy val isHandle = Handle.isHandle(filter)
  lazy val stripSymbol = if (isHandle) Handle.stripSymbol(filter) else filter
  lazy val query = if (isHandle) RecommendedHandle(filter) else Recommended(filter)
}

case class SearchResults(topPeople: Option[IndexedSeq[UserData]], localResults: Option[IndexedSeq[UserData]],
                         conversations: Option[IndexedSeq[ConversationData]], directoryResults: Option[IndexedSeq[UserData]]) {
  lazy val allHandles = topPeople.getOrElse(IndexedSeq.empty).flatMap(_.handle) ++
                        localResults.getOrElse(IndexedSeq.empty).flatMap(_.handle) ++
                        directoryResults.getOrElse(IndexedSeq.empty).flatMap(_.handle)
}

class UserSearchService(selfUserId: UserId,
                        queryCache: SearchQueryCacheStorage,
                        teamId: Option[TeamId],
                        userService: UserService,
                        usersStorage: UsersStorage,
                        teamsService: TeamsService,
                        membersStorage: MembersStorage,
                        timeouts: Timeouts,
                        sync: SyncServiceHandle,
                        messages: MessagesStorage,
                        convsUi: ConversationsUiService
                       ) {

  import Threading.Implicits.Background
  import com.waz.service.UserSearchService._
  import timeouts.search._

  ClockSignal(1.day)(i => queryCache.deleteBefore(i - cacheExpiryTime))(EventContext.Global)

  def search(searchState: SearchState, excludedUsers: Set[UserId]): Signal[SearchResults] = {
    if (searchState.empty) Future.successful {
      System.gc() // TODO: [AN-5497] the user search should not create so many objects to trigger GC in-between
    }

    val topUsersSignal: Signal[IndexedSeq[UserData]] =
      if (searchState.shouldShowTopUsers(teamId.isDefined))
        topPeople.map(_.filter(u => !excludedUsers.contains(u.id))).map(SeqMap(_)(_.id, identity).values)
      else Signal.const(IndexedSeq.empty[UserData])

    val localSearchSignal = for {
      acceptedOrBlocked   <- userService.acceptedOrBlockedUsers
      members             <- searchTeamMembersForState(searchState)
      usersAlreadyInConv  <- searchConvMembersForState(searchState)
    } yield mergeUsers(acceptedOrBlocked.values, members, excludedUsers ++ usersAlreadyInConv, searchState)

    val conversationsSignal: Signal[IndexedSeq[ConversationData]] =
      if (searchState.shouldShowGroupConversations)
        Signal.future(convsUi.findGroupConversations(SearchKey(searchState.filter), Int.MaxValue, handleOnly = searchState.isHandle))
          .map(_.filter(conv => teamId.forall(conv.team.contains)).distinct.toIndexedSeq)
      else Signal.const(IndexedSeq.empty[ConversationData])

    val searchSignal: Signal[IndexedSeq[UserData]] =
      if (searchState.shouldShowDirectorySearch)
        searchUserData(searchState.query).map(_.filter(u => !excludedUsers.contains(u.id)))
      else Signal.const(IndexedSeq.empty[UserData])

    exactMatchUser ! None // reset the exact match to None on any query change

    for {
      topUsers              <- topUsersSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
      localResults          <- localSearchSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
      conversations         <- conversationsSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[ConversationData]]))
      directoryResults      <- searchSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
      exactMatchResults     <- exactMatchUser
    } yield {
      val dir = (directoryResults, exactMatchResults) match {
        case (_, None) => directoryResults
        case (None, Some(exact)) => Some(IndexedSeq(exact))
        case (Some(results), Some(exact)) => Some((results.toSet ++ Set(exact)).toIndexedSeq)
      }
      SearchResults(topUsers, localResults, conversations, dir)
    }
  }

  def updateSearchResults(query: SearchQuery, results: Seq[UserSearchEntry]) = {
    def updating(ids: Vector[UserId])(cached: SearchQueryCache) = cached.copy(query, Instant.now, if (ids.nonEmpty || cached.entries.isEmpty) Some(ids) else cached.entries)

    for {
      updated <- userService.updateUsers(results)
      _ <- userService.syncIfNeeded(updated.toSeq: _*)
      ids = results.map(_.id)(breakOut): Vector[UserId]
      _ = verbose(s"updateSearchResults($query, ${results.map(_.handle)})")
      _ <- queryCache.updateOrCreate(query, updating(ids), SearchQueryCache(query, Instant.now, Some(ids)))
    } yield ()

    query match {
      case RecommendedHandle(handle) if !results.map(_.handle).exists(_.exactMatchQuery(handle)) =>
        debug(s"exact match requested: ${handle}")
        sync.exactMatchHandle(Handle(Handle.stripSymbol(handle)))
      case _ =>
    }

    Future.successful({})
  }

  private val exactMatchUser = new SourceSignal[Option[UserData]]()

  def updateExactMatch(handle: Handle, userId: UserId) = {
    val query = RecommendedHandle(handle.withSymbol)
    def updating(id: UserId)(cached: SearchQueryCache) = cached.copy(query, Instant.now, Some(cached.entries.map(_.toSet ++ Set(userId)).getOrElse(Set(userId)).toVector))

    debug(s"update exact match: ${handle}, $userId")
    userService.getUser(userId).collect {
      case Some(user) =>
        debug(s"received exact match: ${user.handle}")
        exactMatchUser ! Some(user)
        queryCache.updateOrCreate(query, updating(userId), SearchQueryCache(query, Instant.now, Some(Vector(userId))))
    }(Threading.Background)

    Future.successful({})
  }

  private val signalMap = mutable.HashMap[SearchQuery, Signal[IndexedSeq[UserData]]]()

  def searchUserData(query: SearchQuery): Signal[IndexedSeq[UserData]] = signalMap.getOrElseUpdate(query, returning( startNewSearch(query) ) { _ =>
    CancellableFuture.delay(cacheExpiryTime).map { _ =>
      signalMap.remove(query)
      queryCache.remove(query)
    }
  })

  private def startNewSearch(query: SearchQuery): Signal[IndexedSeq[UserData]] = returning( queryCache.optSignal(query) ){ _ =>
    localSearch(query).flatMap(_ => sync.syncSearchQuery(query))
  }.flatMap {
    case None => Signal.const(IndexedSeq.empty[UserData])
    case Some(cached) => cached.entries match {
      case None => Signal.const(IndexedSeq.empty[UserData])
      case Some(ids) if ids.isEmpty => Signal.const(IndexedSeq.empty[UserData])
      case Some(ids) => usersStorage.listSignal(ids).map(_.toIndexedSeq)
    }
  }

  private def localSearch(query: SearchQuery) = (query match {
    case Recommended(prefix) =>
      usersStorage.find[UserData, Vector[UserData]](recommendedPredicate(prefix), db => UserDataDao.recommendedPeople(prefix)(db), identity)
    case RecommendedHandle(prefix) =>
      usersStorage.find[UserData, Vector[UserData]](recommendedHandlePredicate(prefix), db => UserDataDao.recommendedPeople(prefix)(db), identity)
    case _ => Future.successful(Vector.empty[UserData])
  }).flatMap { users =>
    lazy val fresh = SearchQueryCache(query, Instant.now, Some(users.map(_.id)))

    def update(q: SearchQueryCache): SearchQueryCache = if ((cacheExpiryTime elapsedSince q.timestamp) || q.entries.isEmpty) fresh else q

    queryCache.updateOrCreate(query, update, fresh)
  }

  private def topPeople = {
    def messageCount(u: UserData) = messages.countLaterThan(ConvId(u.id.str), Instant.now - topPeopleMessageInterval)

    val loadTopUsers = (for {
      conns         <- usersStorage.find[UserData, Vector[UserData]](topPeoplePredicate, db => UserDataDao.topPeople(db), identity)
      messageCounts <- Future.sequence(conns.map(messageCount))
    } yield conns.zip(messageCounts)).map { counts =>
      counts.filter(_._2 > 0).sortBy(_._2)(Ordering[Long].reverse).take(MaxTopPeople).map(_._1)
    }

    Signal.future(loadTopUsers)
  }

  private val topPeoplePredicate: UserData => Boolean = u => ! u.deleted && u.connection == ConnectionStatus.Accepted

  private def recommendedPredicate(prefix: String): UserData => Boolean = {
    val key = SearchKey(prefix)
    u => ! u.deleted && ! u.isConnected && (key.isAtTheStartOfAnyWordIn(u.searchKey) || u.email.exists(_.str == prefix) || u.handle.exists(_.startsWithQuery(prefix)))
  }

  private def recommendedHandlePredicate(prefix: String): UserData => Boolean = {
    u => ! u.deleted && ! u.isConnected && u.handle.exists(_.startsWithQuery(prefix))
  }

  private def searchTeamMembersForState(searchState: SearchState) = teamId match {
    case None => Signal.const(Set.empty[UserData])
    case Some(_) =>
      val searchKey = if (searchState.filter.isEmpty) None else Some(SearchKey(searchState.filter))
      teamsService.searchTeamMembers(searchKey, handleOnly = Handle.isHandle(searchState.filter))
  }

  private def searchConvMembersForState(searchState: SearchState) = searchState.addingToConversation match {
    case None => Signal.const(Set.empty[UserId])
    case Some(convId) => Signal.future(membersStorage.getByConv(convId)).map(_.map(_.userId).toSet)
  }

  private def mergeUsers(connected: Iterable[UserData], members: Iterable[UserData], excludedIds: Set[UserId], searchState: SearchState): IndexedSeq[UserData] = {
    val users = if (!searchState.empty) connected.filter(connectedUsersPredicate(
      searchState.filter,
      excludedIds.map(_.str),
      alsoSearchByEmail = true,
      showBlockedUsers = true,
      searchByHandleOnly = searchState.isHandle)) else connected

    val includedIds = (users.map(_.id).toSet ++ members.map(_.id).toSet).filterNot(id => id == selfUserId || excludedIds.contains(id))
    (connected ++ members).filter(u => includedIds.contains(u.id)).toIndexedSeq
  }

  private def connectedUsersPredicate(searchTerm: String,
                                      filteredIds: Set[String],
                                      alsoSearchByEmail: Boolean,
                                      showBlockedUsers: Boolean,
                                      searchByHandleOnly: Boolean): UserData => Boolean = {
    val query = SearchKey(searchTerm)
    user =>
      ((query.isAtTheStartOfAnyWordIn(user.searchKey) && !searchByHandleOnly) ||
        user.handle.exists(_.startsWithQuery(searchTerm)) ||
        (alsoSearchByEmail && user.email.exists(e => searchTerm.trim.equalsIgnoreCase(e.str)))) &&
        !filteredIds.contains(user.id.str) &&
        (showBlockedUsers || (user.connection != ConnectionStatus.Blocked))
  }

}

object UserSearchService {
  val MinCommonConnections = 4
  val MaxTopPeople = 10
}
