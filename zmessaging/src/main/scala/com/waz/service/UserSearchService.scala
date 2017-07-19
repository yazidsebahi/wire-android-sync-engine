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
import com.waz.model.SearchQuery.{Recommended, RecommendedHandle, TopPeople}
import com.waz.model.UserData.{ConnectionStatus, UserDataDao}
import com.waz.model.{SearchQuery, _}
import com.waz.service.conversation.ConversationsUiService
import com.waz.service.teams.TeamsService
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.UserSearchClient.UserSearchEntry
import com.waz.threading.Threading
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils._
import com.waz.utils.events._
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.collection.immutable.Set
import scala.concurrent.Future
import scala.concurrent.duration._


case class SearchState(filter: String, hasSelectedUsers: Boolean, addingToConversation: Option[ConvId]){
  def shouldShowTopUsers(isTeam: Boolean) = filter.isEmpty && !isTeam && addingToConversation.isEmpty
  def shouldShowAbContacts(isTeam: Boolean) = addingToConversation.isEmpty && !hasSelectedUsers && !isTeam
  lazy val shouldShowGroupConversations = filter.nonEmpty && !hasSelectedUsers && addingToConversation.isEmpty
  lazy val shouldShowDirectorySearch = filter.nonEmpty && !hasSelectedUsers && addingToConversation.isEmpty

  lazy val empty = filter.isEmpty
  lazy val isHandle = Handle.containsSymbol(filter)
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
    if (searchState.empty) Future {
      System.gc() // TODO: [AN-5497] the user search should not create so many objects to trigger GC in-between
    }(Threading.Background)

    val topUsersSignal =
      if (searchState.shouldShowTopUsers(teamId.isDefined)) searchUserData(TopPeople).map(_.values.filter(u => !excludedUsers.contains(u.id)))
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

    val searchSignal =
      if (searchState.shouldShowDirectorySearch) searchUserData(searchState.query).map(_.values.filter(u => !excludedUsers.contains(u.id)))
      else Signal.const(IndexedSeq.empty[UserData])

    (for {
      topUsers              <- topUsersSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
      localResults          <- localSearchSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
      conversations         <- conversationsSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[ConversationData]]))
      directoryResults      <- searchSignal.map(Option(_)).orElse(Signal.const(Option.empty[IndexedSeq[UserData]]))
    } yield SearchResults(topUsers, localResults, conversations, directoryResults)).map { res =>
      if (searchState.isHandle && searchState.stripSymbol.length > 1 && !res.allHandles.exists(_.exactMatchQuery(searchState.filter)))
        sync.exactMatchHandle(Handle(searchState.stripSymbol))
      res
    }

  }

  def updateSearchResults(query: SearchQuery, results: Seq[UserSearchEntry]) = {
    def updating(ids: Vector[UserId])(cached: SearchQueryCache) = cached.copy(query, Instant.now, if (ids.nonEmpty || cached.entries.isEmpty) Some(ids) else cached.entries)

    for {
      updated <- userService.updateUsers(results)
      _       <- userService.syncIfNeeded(updated.toSeq: _*)
      ids      = results.map(_.id)(breakOut): Vector[UserId]
      _        = verbose(s"updateSearchResults($query, $ids)")
      _       <- queryCache.updateOrCreate(query, updating(ids), SearchQueryCache(query, Instant.now, Some(ids)))
    } yield ()
  }

  def updateExactMatch(handle: Handle, userId: UserId) = {
    val query = RecommendedHandle(handle.withSymbol)
    val cache = SearchQueryCache(query, Instant.now, Some(Vector(userId)))
    def updating(id: UserId)(cached: SearchQueryCache) = cached.copy(query, Instant.now, Some(cached.entries.map(_.toSet ++ Set(userId)).getOrElse(Set(userId)).toVector))

    for {
      _ <- userService.getUser(userId)
      _ = verbose(s"user synced($query, $userId)")
      _ <- queryCache.updateOrCreate(query, updating(userId), cache)
    } yield ()
  }

  def searchUserData(query: SearchQuery): Signal[SeqMap[UserId, UserData]] = {
   
    queryCache.optSignal(query).flatMap {
      case _ if query == TopPeople =>
        if (teamId.isEmpty) topPeople else Signal.empty[Vector[UserData]]

      case r if r.forall(cached => (cacheExpiryTime elapsedSince cached.timestamp) || cached.entries.isEmpty) =>

        def fallbackToLocal = query match {
          case Recommended(prefix) =>
            usersStorage.find[UserData, Vector[UserData]](recommendedPredicate(prefix), db => UserDataDao.recommendedPeople(prefix)(db), identity)
          case RecommendedHandle(prefix) =>
            usersStorage.find[UserData, Vector[UserData]](recommendedHandlePredicate(prefix), db => UserDataDao.recommendedPeople(prefix)(db), identity)
          case _ => Future.successful(Vector())
        }

        fallbackToLocal.map(_.sortBy(_.name)(currentLocaleOrdering)).flatMap { users =>
          lazy val fresh = SearchQueryCache(query, Instant.now, Some(users.map(_.id)))

          def update(q: SearchQueryCache): SearchQueryCache = if ((cacheExpiryTime elapsedSince q.timestamp) || q.entries.isEmpty) fresh else q

          queryCache.updateOrCreate(query, update, fresh)
        }.flatMap(_ => sync.syncSearchQuery(query)).logFailure()

        Signal.const(Vector.empty[UserData])

      case Some(cached) =>
        if (cacheRefreshInterval elapsedSince cached.timestamp)
          queryCache.getOrCreate(query, SearchQueryCache(query, Instant.now, None)).flatMap(_ => sync.syncSearchQuery(query)).logFailure()

        cached.entries match {
          case Some(ids) => usersStorage.listSignal(ids)
          case _ => Signal.const(Vector.empty[UserData])
        }

      case _ => Signal.const(Vector.empty[UserData])
    }.map { users =>
      query match {
        case TopPeople if teamId.isEmpty => users filter topPeoplePredicate
        case Recommended(prefix) => users filter recommendedPredicate(prefix)
        case RecommendedHandle(prefix) => users filter recommendedHandlePredicate(prefix)
        case _ => users
      }
    }.map { users => SeqMap(users)(_.id, identity) }
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
    u => ! u.deleted && ! u.isConnected && (key.isAtTheStartOfAnyWordIn(u.searchKey) || u.email.exists(_.str == prefix) || u.handle.exists(_.containsQuery(prefix)))
  }

  private def recommendedHandlePredicate(prefix: String): UserData => Boolean = {
    u => ! u.deleted && ! u.isConnected && u.handle.exists(_.containsQuery(prefix))
  }

  private def searchTeamMembersForState(searchState: SearchState) = teamId match {
    case None => Signal.const(Set.empty[UserData])
    case Some(_) =>
      val searchKey = if (searchState.filter.isEmpty) None else Some(SearchKey(searchState.filter))
      teamsService.searchTeamMembers(searchKey, handleOnly = Handle.containsSymbol(searchState.filter))
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
        user.handle.exists(_.containsQuery(searchTerm)) ||
        (alsoSearchByEmail && user.email.exists(e => searchTerm.trim.equalsIgnoreCase(e.str)))) &&
        !filteredIds.contains(user.id.str) &&
        (showBlockedUsers || (user.connection != ConnectionStatus.Blocked))
  }

}

object UserSearchService {
  val MinCommonConnections = 4
  val MaxTopPeople = 10
}
