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
package com.waz.service.conversation

import com.waz.api.IConversation.{Access, AccessRole}
import com.waz.content.{ConversationStorage, MembersStorage}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.ConversationData.ConversationType.Group
import com.waz.model.{ConversationMemberData, _}
import com.waz.service.messages.MessagesService
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle

import scala.concurrent.Future

class TeamConversationSpec extends AndroidFreeSpec {

  val account      = AccountId()
  val self         = UserId()
  val team         = Some(TeamId("team"))
  val members      = mock[MembersStorage]
  val convsContent = mock[ConversationsContentUpdater]
  val convsStorage = mock[ConversationStorage]
  val sync         = mock[SyncServiceHandle]
  val messages     = mock[MessagesService]

  feature("Creating team conversations") {

    scenario("Create 1:1 conversation within a team with existing 1:1 conversation between the two members should return existing conversation") {
      val otherUser = UserId("otherUser")

      val existingConv = ConversationData(creator = self, convType = Group, team = team)

      (members.getByUsers _).expects(Set(otherUser)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (members.getByConvs _).expects(Set(existingConv.id)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(self,      existingConv.id),
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (convsStorage.getAll _).expects(Seq(existingConv.id)).once().returning(Future.successful(Seq(Some(existingConv))))

      result(initService.getOrCreateOneToOneConversation(otherUser)) shouldEqual existingConv
    }

    scenario("Existing 1:1 conversation between two team members with NAME should not be returned") {
      val otherUser = UserId("otherUser")
      val name = Some("Conv Name")
      val existingConv = ConversationData(creator = self, name = name, convType = Group, team = team)

      (members.getByUsers _).expects(Set(otherUser)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (members.getByConvs _).expects(Set(existingConv.id)).once().returning(Future.successful(IndexedSeq(
        ConversationMemberData(self,      existingConv.id),
        ConversationMemberData(otherUser, existingConv.id)
      )))

      (convsStorage.getAll _).expects(Seq(existingConv.id)).once().returning(Future.successful(Seq(Some(existingConv))))
      (convsContent.createConversationWithMembers _).expects(*, *, Group, self, Set(otherUser), None, false, false).once().onCall {
        (conv: ConvId, r: RConvId, tpe: ConversationType, cr: UserId, us: Set[UserId], n: Option[String], to: Boolean, hid: Boolean) =>
          val (access, role) = ConversationData.getAccessAndRole(to, team).get
          Future.successful(ConversationData(conv, r, n, cr, tpe, team, hidden = hid, access = access, accessRole = Some(role)))
      }
      (messages.addConversationStartMessage _).expects(*, self, Set(otherUser), None).once().returning(Future.successful(null))
      (sync.postConversation _).expects(*, Set(otherUser), None, team, Some((Set(Access.INVITE, Access.CODE), AccessRole.NON_VERIFIED))).once().returning(Future.successful(null))

      val conv = result(initService.getOrCreateOneToOneConversation(otherUser))
      conv shouldNot equal(existingConv)
    }
  }

  def initService: ConversationsUiService =
    new ConversationsUiServiceImpl(account, self, team, null, null, null, messages, null, null, members, null, convsContent, convsStorage, null, null, sync, null, null, null)
}
