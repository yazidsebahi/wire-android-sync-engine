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
package com.waz.service.teams

import com.waz.content.{ConversationStorage, MembersStorage}
import com.waz.model.ConversationData.ConversationType.Group
import com.waz.model.{ConversationMemberData, _}
import com.waz.service.conversation._
import com.waz.service.messages.MessagesService
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle

import scala.concurrent.Future

class TeamConversationSpec extends AndroidFreeSpec {

  val account = AccountId()
  val self = UserId()
  val members      = mock[MembersStorage]
  val convsContent = mock[ConversationsContentUpdater]
  val convsStorage = mock[ConversationStorage]
  val sync         = mock[SyncServiceHandle]
  val messages     = mock[MessagesService]

  feature("Creating team conversations") {


    scenario("Creating a group conversation with a defined name and only one other member should NOT return the existing conversation with that member, but return a new group instead") {
      val oneToOneConv = ConvId("oneToOne")
      val newConv = ConvId("new")

      val otherUser = UserId("otherUser")

      val team = TeamId("team")
      val name = Some("Conv")

      (members.getByUsers _).expects(*).never()
      (members.getByConvs _).expects(*).never()
      (convsStorage.getAll _).expects(*).never()

      //should always create a new group
      (convsContent.createConversationWithMembers _).expects(newConv, *, Group, self, Seq(otherUser), name, false, Some(team)).once().returning {
        Future.successful(ConversationData(
          id            = newConv,
          name          = name,
          creator       = self,
          team          = Some(team)
        ))
      }
      (sync.postConversation _).expects(newConv, Seq(otherUser), *, Some(team)).once().returning(Future.successful(SyncId()))

      (messages.addConversationStartMessage _).expects(newConv, self, Set(otherUser), name).once().returning(Future.successful(MessageData())) //return not used

      result(initService.createGroupConversation(newConv, name, Seq(otherUser), Some(team)))
    }

    scenario("Create 1:1 conversation within a team with existing 1:1 conversation between the two members should return existing conversation, if no name is specified") {
      val otherUser = UserId()
      val team = TeamId()


      val existingConv = ConversationData(ConvId(), RConvId(), Some(""), self, Group, Some(team))

      (members.getByUsers _).expects(Set(otherUser)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (members.getByConvs _).expects(Set(existingConv.id)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(self,      existingConv.id),
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (convsStorage.getAll _).expects(Seq(existingConv.id)).once().returning(Future.successful(Seq(Some(existingConv))))

      result(initService.createGroupConversation(ConvId(), None, Seq(otherUser), Some(team))) shouldEqual existingConv
    }
  }

  def initService: ConversationsUiService = {
    new ConversationsUiServiceImpl(account, self, null, null, null, messages, null, null, members, null, convsContent, convsStorage, null, null, sync, null, null, null)
  }
}
