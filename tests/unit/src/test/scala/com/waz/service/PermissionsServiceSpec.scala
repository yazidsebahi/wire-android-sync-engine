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

import java.util

import com.waz.RobolectricUtils
import com.waz.api.Permission.Status._
import com.waz.api.{Permission, PermissionProvider}
import com.waz.testutils.Matchers._
import com.waz.testutils.MockGlobalModule
import com.waz.threading.Threading
import org.robolectric.Robolectric
import org.scalatest._

import scala.collection.breakOut
import scala.collection.JavaConverters._
import scala.concurrent.duration._

class PermissionsServiceSpec extends FeatureSpec with Matchers with Inspectors with RobolectricTests with RobolectricUtils {

  feature("Checking permissions") {
    scenario("Initial status") {
      forAll(permissions)(p => service.isGranted(p) shouldBe false)
    }

    scenario("Granted permission") {
      Robolectric.getShadowApplication.grantPermissions(permissions.head.id)
      service.isGranted(permissions.head) shouldBe true
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe false)
    }

    scenario("Revoked permission") {
      Robolectric.getShadowApplication.denyPermissions(permissions.head.id)
      forAll(permissions)(p => service.isGranted(p) shouldBe false)
    }
  }

  feature("Requesting permissions") {
    scenario("No permissions provider") {
      service.request(permissions.toSet, false).await() shouldBe empty
      forAll(permissions)(p => service.isGranted(p) shouldBe false)

      Robolectric.getShadowApplication.grantPermissions(permissions.head.id)
      service.request(permissions.toSet, false).await() shouldEqual Set(permissions.head)
      service.isGranted(permissions.head) shouldBe true
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe false)

      Robolectric.getShadowApplication.denyPermissions(permissions.head.id)
      service.request(permissions.toSet, false).await() shouldBe empty
      forAll(permissions)(p => service.isGranted(p) shouldBe false)
    }

    scenario("With permissions provider") {
      val prov = new TailPermissionProvider
      service.setProvider(prov)

      service.request(permissions.toSet, false).await() shouldEqual permissions.tail.toSet
      service.isGranted(permissions.head) shouldBe false
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe true)

      service.clearProvider(prov)

      service.request(permissions.toSet, false).await() shouldEqual permissions.tail.toSet
      service.isGranted(permissions.head) shouldBe false
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe true)
    }

    scenario("Delayed request") {
      Robolectric.getShadowApplication.denyPermissions(permissions.map(_.id)(breakOut):_*)

      val response = service.request(permissions.toSet, true)
      awaitUi(2.seconds)
      response should not be 'completed
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe false)

      val prov = new TailPermissionProvider
      service.setProvider(prov)
      response.await() shouldEqual permissions.tail.toSet
      service.isGranted(permissions.head) shouldBe false
      forAll(permissions.tail)(p => service.isGranted(p) shouldBe true)

      service.clearProvider(prov)
    }
  }

  lazy val global = new MockGlobalModule()
  lazy val service = global.permissions
  lazy val permissions = Permission.values.toList

  class TailPermissionProvider extends PermissionProvider {
    override def requestPermissions(ps: util.Set[Permission], callback: ResponseHandler): Unit = {
      Threading.assertUiThread()
      val (denied, granted) = ps.asScala.partition(_ == permissions.head)
      Robolectric.getShadowApplication.grantPermissions(granted.map(_.id)(breakOut):_*)
      Robolectric.getShadowApplication.denyPermissions(denied.map(_.id)(breakOut):_*)
      callback.handleResponse(granted.map(_ -> GRANTED).++(denied.map(_ -> DENIED)).toMap.asJava)
    }
  }
}
