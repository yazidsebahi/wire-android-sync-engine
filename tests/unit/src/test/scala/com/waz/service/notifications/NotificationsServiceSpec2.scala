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
package com.waz.service.notifications

import com.waz.api.Message
import com.waz.content.UserPreferences.LastAccountVisibleTime
import com.waz.content._
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZmsLifeCycle
import com.waz.service.push.{NotificationService, PushService}
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.TestUserPreferences
import com.waz.utils.events.{EventStream, Signal}
import org.threeten.bp.Duration

import scala.collection.Map
import scala.concurrent.{Future, duration}
import scala.concurrent.duration._
import com.waz.utils.RichFiniteDuration
import com.waz.utils.RichThreetenBPDuration
import com.waz.utils.RichInstant



class NotificationsServiceSpec2 extends AndroidFreeSpec {


  val account   = AccountId()
  val self      = UserId()
  val messages  = mock[MessagesStorage]
  val lifeCycle = mock[ZmsLifeCycle]
  val storage   = mock[NotificationStorage]
  val users     = mock[UsersStorage]
  val convs     = mock[ConversationStorage]
  val reactions = mock[ReactionsStorage]
  val userPrefs = new TestUserPreferences
  val push      = mock[PushService]


  val inForeground = Signal(false)
  val beDrift      = Signal(Duration.ZERO)
  val convsAdded   = EventStream[Seq[ConversationData]]()
  val convsUpdated = EventStream[Seq[(ConversationData, ConversationData)]]()
  val allConvs     = Signal[IndexedSeq[ConversationData]]()

  val notifications = Signal[Map[NotId, NotificationData]]()

  val msgsAdded   = EventStream[Seq[MessageData]]()
  val msgsUpdated = EventStream[Seq[(MessageData, MessageData)]]()
  val msgsDeleted = EventStream[Seq[MessageId]]()

  val reactionsChanged = EventStream[Seq[Liking]]()
  val reactionsDeleted = EventStream[Seq[(MessageId, UserId)]]()

  val lastAccountVisiblePref = userPrefs.preference(LastAccountVisibleTime)

//  NotificationService.ClearUiThrottling = duration.Duration.Zero

  feature ("Background behaviour") {

    def beTime = clock.instant + beDrift.map(_.asScala).currentValue("").getOrElse(duration.Duration.Zero)

    def twoSimpleNotifications = {
      val user = UserId("user")
      val conv = ConversationData(ConvId("conv"), RConvId(), Some("conv"), user, ConversationType.OneToOne)
      val msg1  = MessageData(MessageId("msg1"), conv.id, Message.Type.TEXT, user)
      val msg2  = MessageData(MessageId("msg2"), conv.id, Message.Type.TEXT, user)

      val not1 = NotificationData(NotId("notf1"), user = user, conv = conv.id, referencedMessage = Some(msg1.id), time = beTime)
      val not2 = NotificationData(NotId("notf2"), user = user, conv = conv.id, referencedMessage = Some(msg2.id), time = beTime)

      notifications ! Map(
        not1.id -> not1,
        not2.id -> not2
      )

      (users.get _).expects(user).twice().returning(Future.successful(Some(UserData("testUser1").copy(id = user))))
      (messages.getMessage _).expects(*).twice.onCall { id: MessageId =>
        Future.successful(Some {
          id match {
            case msg1.id => msg1
            case msg2.id => msg2
            case _ => fail("Unexpected msg id")
          }
        })
      }
      (convs.get _).expects(conv.id).twice.returning(Future.successful(Some(conv)))
    }

    scenario("Display notifications") {
      clock + 10.seconds
      twoSimpleNotifications
      result(getService.notifications.filter(_.size == 2).head)
    }

    scenario("Bringing ui to foreground should clear notifications") {
      clock + 10.seconds
      twoSimpleNotifications
      val service = getService
      result(service.notifications.filter(_.size == 2).head)

      clock + 10.seconds
      inForeground ! true

      result(service.notifications.filter(_.isEmpty).head)
    }

    scenario("Receiving notifications after app is put to background should take BE drift into account") {
      clock + 30.seconds
      beDrift ! (-15.seconds).asJava

      inForeground ! true
      val service = getService

      inForeground ! false

      clock + 5.seconds
      twoSimpleNotifications

      result(service.notifications.filter(_.size == 2).head)
    }
  }


  def getService = {

    (lifeCycle.accInForeground _).expects(account).returning(inForeground)
    (push.beDrift _).expects().anyNumberOfTimes().returning(beDrift)

    (storage.notifications _).expects().anyNumberOfTimes().returning(notifications)

    (convs.onAdded _).expects().returning(convsAdded)
    (convs.onUpdated _).expects().returning(convsUpdated)
    (convs.getAllConvs _).expects().returning(allConvs.head)

    (messages.onAdded _).expects().returning(msgsAdded)
    (messages.onUpdated _).expects().returning(msgsUpdated)
    (messages.onDeleted _).expects().returning(msgsDeleted)

    (reactions.onChanged _).expects().returning(reactionsChanged)
    (reactions.onDeleted _).expects().returning(reactionsDeleted)

    new NotificationService(null, account, self, messages, lifeCycle, storage, users, convs, reactions, userPrefs, push)
  }

}
