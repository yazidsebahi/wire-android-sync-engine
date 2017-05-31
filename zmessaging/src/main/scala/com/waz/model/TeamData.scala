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
import com.waz.service.SearchKey
import com.waz.utils.JsonDecoder
import com.waz.utils.wrappers.{DB, DBCursor}
import org.json.JSONObject

import scala.collection.mutable

case class TeamData(id:      TeamId,
                    name:    String,
                    creator: UserId,
                    icon:    Option[RAssetId] = None,
                    iconKey: Option[AESKey]  = None)

object TeamData {

  implicit lazy val Decoder: JsonDecoder[TeamData] = new JsonDecoder[TeamData] {
    override def apply(implicit js: JSONObject): TeamData = {
      import JsonDecoder._
      TeamData('id, 'name, 'creator, 'icon, decodeOptString('icon_key).map(AESKey))
    }
  }

  import com.waz.db.Col._
  implicit object TeamDataDoa extends Dao[TeamData, TeamId] {
    val Id      = id[TeamId]      ('_id, "PRIMARY KEY").apply(_.id)
    val Name    = text            ('name)(_.name)
    val Creator = id[UserId]      ('creator).apply(_.creator)
    val Icon    = opt(id[RAssetId] ('icon))(_.icon)
    val IconKey = opt(text[AESKey]('icon_key, _.str, AESKey))(_.iconKey)

    override val idCol = Id
    override val table = Table("Teams", Id, Name, Creator, Icon, IconKey)

    override def apply(implicit cursor: DBCursor): TeamData = new TeamData(Id, Name, Creator, Icon, IconKey)
  }
}

case class TeamMemberData(userId:      UserId,
                          teamId:      TeamId,
                          private val _selfPermissions: Long = 0,
                          private val _copyPermissions: Long = 0) {
  import TeamMemberData._

  lazy val selfPermissions = decodeBitmask(_selfPermissions)
  lazy val copyPermissions = decodeBitmask(_copyPermissions)
}

object TeamMemberData {

  type Key = (UserId, TeamId)

  type Permission = Permission.Value
  object Permission extends Enumeration {
    val CreateConversation,         // 0x001
        DeleteConversation,         // 0x002
        AddTeamMember,              // 0x004
        RemoveTeamMember,           // 0x008
        AddConversationMember,      // 0x010
        RemoveConversationMember,   // 0x020
        GetBilling,                 // 0x040
        SetBilling,                 // 0x080
        SetTeamData,                // 0x100
        GetMemberPermissions,       // 0x200
        GetTeamConversations,       // 0x400
        DeleteTeam,                 // 0x800
        SetMemberPermissions        // 0x1000
    = Value
  }

  def decodeBitmask(mask: Long): Set[Permission] = {
    val builder = new mutable.SetBuilder[Permission, Set[Permission]](Set.empty)
    (0 until Permission.values.size).map(math.pow(2, _).toInt).zipWithIndex.foreach {
      case (one, pos) => if ((mask & one) != 0) builder += Permission(pos)
    }
    builder.result()
  }

  def encodeBitmask(ps: Set[Permission]): Long = {
    var mask = 0L
    (0 until Permission.values.size).map(math.pow(2, _).toLong).zipWithIndex.foreach {
      case (m, i) => if (ps.contains(Permission(i))) mask = mask | m
    }
    mask
  }

  import com.waz.db.Col._
  implicit object TeamMemberDataDoa extends Dao2[TeamMemberData, UserId, TeamId] {
    val UserId          = id[UserId]('user_id).apply(_.userId)
    val TeamId          = id[TeamId]('team_id).apply(_.teamId)
    val SelfPermissions = long('self_permissions)(_._selfPermissions)
    val CopyPermissions = long('copy_permissions)(_._copyPermissions)

    override val idCol = (UserId, TeamId)
    override val table = Table("TeamMembers", UserId, TeamId, SelfPermissions, CopyPermissions)

    override def apply(implicit cursor: DBCursor): TeamMemberData = TeamMemberData(UserId, TeamId, SelfPermissions, CopyPermissions)

    def findForUsers(users: Set[UserId])(implicit db: DB) = iterating(findInSet(UserId, users))
    def findForTeams(teams: Set[TeamId])(implicit db: DB) = iterating(findInSet(TeamId, teams))

    def search(prefix: SearchKey, team: TeamId, handleOnly: Boolean)(implicit db: DB) = {
      import UserData.{UserDataDao => U}
      val select =
        s"""SELECT DISTINCT tm.*
           |  FROM ${table.name} tm, ${U.table.name} u
           | WHERE tm.${UserId.name} = u.${U.Id.name}""".stripMargin
      val handleCondition =
        if (handleOnly){
          s"""AND u.${U.Handle.name} LIKE '%${prefix.asciiRepresentation}%'""".stripMargin
        } else {
          s"""AND (   u.${U.SKey.name} LIKE '${U.SKey(prefix)}%'
             |     OR u.${U.SKey.name} LIKE '% ${U.SKey(prefix)}%'
             |     OR u.${U.Handle.name} LIKE '%${prefix.asciiRepresentation}%')""".stripMargin
        }
      list(db.rawQuery(select + " " + handleCondition, null)).toSet
    }

  }
}
