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
package com.waz.model

import com.waz.db.{Dao, Dao2}
import com.waz.model.TeamMemberData.Permission
import com.waz.utils.wrappers.{DB, DBCursor}

case class TeamData(id:      TeamId,
                    name:    String,
                    picture: Option[AssetId])

object TeamData {
  import com.waz.db.Col._
  implicit object TeamDataDoa extends Dao[TeamData, TeamId] {
    val Id       = id[TeamId]     ('_id, "PRIMARY KEY").apply(_.id)
    val Name     = text           ('name)(_.name)
    val Picture  = opt(id[AssetId]('picture))(_.picture)

    override val idCol = Id
    override val table = Table("Teams", Id, Name, Picture)

    override def apply(implicit cursor: DBCursor): TeamData = new TeamData(Id, Name, Picture)
  }
}

case class TeamMemberData(userId:      UserId,
                          teamId:      TeamId,
                          permissions: Set[Permission])

object TeamMemberData {

  type Key = (UserId, TeamId)

  type Permission = Permission.Value
  object Permission extends Enumeration {
    val CreateConversation,
        DeleteConversation,
        AddTeamMember,
        RemoveTeamMember,
        AddConversationMember,
        RemoveConversationMember,
        GetBilling,
        SetBilling,
        SetTeamData = Value
  }

  import com.waz.db.Col._
  implicit object TeamMemberDataDoa extends Dao2[TeamMemberData, UserId, TeamId] {
    val UserId      = id[UserId]('user_id).apply(_.userId)
    val TeamId      = id[TeamId]('team_id).apply(_.teamId)
    val Permissions = set[Permission]('permissions, _.mkString(","), _.split(",").toSet.map(Permission.withName))(_.permissions)

    override val idCol = (UserId, TeamId)
    override val table = Table("TeamMembers", UserId, TeamId, Permissions)

    override def apply(implicit cursor: DBCursor): TeamMemberData = TeamMemberData(UserId, TeamId, Permissions)

    def findForUser(userId: UserId)(implicit db: DB) = iterating(find(UserId, userId))
    def findForTeam(teamId: TeamId)(implicit db: DB) = iterating(find(TeamId, teamId))

  }
}
