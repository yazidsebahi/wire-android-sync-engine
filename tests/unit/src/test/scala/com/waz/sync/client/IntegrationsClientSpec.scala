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
package com.waz.sync.client

import com.waz.ZLog.ImplicitTag._
import com.waz.model.{IntegrationData, ProviderData, ProviderId}
import com.waz.specs.AndroidFreeSpec
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.znet.Response.{HttpStatus, InternalError}
import com.waz.znet.{JsonObjectResponse, Request, Response, ZNetClient}
import com.waz.znet.ZNetClient.errorHandling
import org.json.JSONObject

import scala.concurrent.{ExecutionContext, Future}

class IntegrationsClientSpec extends AndroidFreeSpec {
  import IntegrationsClientSpec._

  implicit val ctx = Threading.Background

  private val nc = mock[ZNetClient]
  (nc.apply(_: Request[Unit]))
    .expects(*)
    .anyNumberOfTimes()
    .onCall { request: Request[Unit] =>

      val jsonOpt = request.resourcePath.get match {
        case path if path.contains("/services") => createIntegrationsJson(path)
        case path if path.contains("/providers") => createProviderJson(path)
        case _ => None
      }

      val response = jsonOpt match {
        case Some(json) => Response(HttpStatus(200, "HTTP/1.1 200 OK"), JsonObjectResponse(json))
        case None => Response(InternalError(s"Invalid request: $request"))
      }

      CancellableFuture { response }
    }

  (nc.withErrorHandling[Unit, Seq[IntegrationData]](_: String, _: Request[Unit])(_: PartialFunction[Response, Seq[IntegrationData]])(_: ExecutionContext))
    .expects(*, *, *, *)
    .anyNumberOfTimes()
    .onCall { (name: String, request: Request[Unit], pf: PartialFunction[Response, Seq[IntegrationData]], ec: ExecutionContext) =>
      nc.apply(request).map(pf.andThen(Right(_)).orElse(errorHandling(name)))(ec)
    }

  (nc.withErrorHandling[Unit, ProviderData](_: String, _: Request[Unit])(_: PartialFunction[Response, ProviderData])(_: ExecutionContext))
    .expects(*, *, *, *)
    .anyNumberOfTimes()
    .onCall { (name: String, request: Request[Unit], pf: PartialFunction[Response, ProviderData], ec: ExecutionContext) =>
      nc.apply(request).map(pf.andThen(Right(_)).orElse(errorHandling(name)))(ec)
    }

  feature("integrations") {

    val nc = mock[ZNetClient]
    (nc.apply(_: Request[Unit]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { request: Request[Unit] =>

        val jsonOpt = request.resourcePath.get match {
          case path if path.contains("/services") => createIntegrationsJson(path)
          case _ => None
        }

        val response = jsonOpt match {
          case Some(json) => Response(HttpStatus(200, "HTTP/1.1 200 OK"), JsonObjectResponse(json))
          case None => Response(InternalError(s"Invalid request: $request"))
        }

        CancellableFuture { response }
      }

    (nc.withErrorHandling[Unit, Seq[IntegrationData]](_: String, _: Request[Unit])(_: PartialFunction[Response, Seq[IntegrationData]])(_: ExecutionContext))
      .expects(*, *, *, *)
      .anyNumberOfTimes()
      .onCall { (name: String, request: Request[Unit], pf: PartialFunction[Response, Seq[IntegrationData]], ec: ExecutionContext) =>
        nc.apply(request).map(pf.andThen(Right(_)).orElse(errorHandling(name)))(ec)
      }


    def integrationsFuture(startWith: String) = new IntegrationsClientImpl(nc).getIntegrations(startWith).future.flatMap {
      case Right(ints) => Future.successful(ints)
      case Left(error) => Future.failed(new Exception(error.message))
    }

    scenario("get all integrations") {
      result(integrationsFuture("")).size shouldEqual 3
    }

    scenario("get collectionsbot") {
      val res = result(integrationsFuture("collectionsbot"))

      res.size shouldEqual 1
      res.head.name shouldEqual "collectionsbot"
    }

    scenario("get Echo bots") {
      val res = result(integrationsFuture("Echo"))

      res.size shouldEqual 2
      res.forall(_.name.startsWith("Echo")) shouldEqual true
    }

    scenario("get Echo bots - letter case important") {
      result(integrationsFuture("echo")).isEmpty shouldEqual true
    }

  }

  feature("providers") {

    val nc = mock[ZNetClient]
    (nc.apply(_: Request[Unit]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { request: Request[Unit] =>

        val jsonOpt = request.resourcePath.get match {
          case path if path.contains("/providers") => createProviderJson(path)
          case _ => None
        }

        val response = jsonOpt match {
          case Some(json) => Response(HttpStatus(200, "HTTP/1.1 200 OK"), JsonObjectResponse(json))
          case None => Response(InternalError(s"Invalid request: $request"))
        }

        CancellableFuture { response }
      }

    (nc.withErrorHandling[Unit, ProviderData](_: String, _: Request[Unit])(_: PartialFunction[Response, ProviderData])(_: ExecutionContext))
      .expects(*, *, *, *)
      .anyNumberOfTimes()
      .onCall { (name: String, request: Request[Unit], pf: PartialFunction[Response, ProviderData], ec: ExecutionContext) =>
        nc.apply(request).map(pf.andThen(Right(_)).orElse(errorHandling(name)))(ec)
      }

    def providerFuture(id: ProviderId) = new IntegrationsClientImpl(nc).getProvider(id).future.flatMap {
      case Right(provider) => Future.successful(provider)
      case Left(error) => Future.failed(new Exception(error.message))
    }

    scenario("get provider") {
      result(providerFuture(ProviderId(providerId0))).name equals "Wire Swiss GmbH"
    }
  }

}

object IntegrationsClientSpec {

  val providerId0 = "19c092cb-27cf-42f9-812e-a10de4f2dcae"
  val providerResponse0 =
    s"""
       |{
       |  "id": "$providerId0",
       |  "name": "Wire Swiss GmbH",
       |  "email": "support@wire.com",
       |  "url": "https://wire.com/",
       |  "description": "The Wire Team"
       | }
    """.stripMargin

  val providerId1 = "5671b6be-f958-4b85-aecc-a5fcffa856fa"
  val providerResponse1 =
    s"""
       |{
       |  "id": "$providerId1",
       |  "name": "Collections Corp",
       |  "email": "support@coco.com",
       |  "url": "https://coco.com/",
       |  "description": "The Collections Corp providing collectionsbot"
       | }
      """.stripMargin

  val providerId2 = "6ca9ae2b-b6ac-4e9b-a868-d648d51787a3"
  val providerResponse2 =
    s"""
       |{
       |  "id": "$providerId2",
       |  "name": "Echo Company",
       |  "email": "support@echo.com",
       |  "url": "https://echo.com/",
       |  "description": "Echo Company providing high-quality echo"
       | }
      """.stripMargin

  val providerId3 = "fd6ffe9f-d943-43d7-a691-9bb9e4d1b964"
  val providerResponse3 =
    s"""
       |{
       |  "id": "$providerId3",
       |  "name": "blah blah",
       |  "email": "support@blah.com",
       |  "url": "https://blah.com/",
       |  "description": "blah blah blah"
       | }
      """.stripMargin

  val providers = Map(
    providerId0 -> providerResponse0,
    providerId1 -> providerResponse1,
    providerId2 -> providerResponse2,
    providerId3 -> providerResponse3
  )

  val integrationId1 = "07653181-7c72-4a3a-8e76-39fcbf27fd17"
  val integrationName1 = "collectionsbot"
  val integrationResponse1 =
    s"""
       |    {
       |      "assets" : [
       |
      |      ],
       |      "provider" : "$providerId1",
       |      "enabled" : true,
       |      "id" : "$integrationId1",
       |      "description" : "Helps you fill your library",
       |      "name" : "$integrationName1",
       |      "tags" : [
       |        "tutorial"
       |      ]
       |    }
    """.stripMargin

  val integrationId2 = "748bda63-7783-42d4-80c2-030e3daef5c7"
  val integrationName2 = "Echo"
  val integrationResponse2 =
    s"""
       |{
       |      "assets" : [
       |
      |      ],
       |      "provider" : "$providerId2",
       |      "enabled" : true,
       |      "id" : "$integrationId2",
       |      "description" : "Echo",
       |      "name" : "$integrationName2",
       |      "tags" : [
       |        "tutorial"
       |      ]
       |    }
    """.stripMargin

  val integrationId3 = "f21ef724-64cc-45e3-b78d-d1b18a7c02a5"
  val integrationName3 = "Echo_stage"
  val integrationResponse3 =
    s"""
       |{
       |      "assets" : [
       |
      |      ],
       |      "provider" : "$providerId3",
       |      "enabled" : true,
       |      "id" : "$integrationId3",
       |      "description" : "blah",
       |      "name" : "$integrationName3",
       |      "tags" : [
       |        "tutorial"
       |      ]
       |    }
    """.stripMargin

  val integrationResponses = Map(
    integrationName1 -> integrationResponse1,
    integrationName2 -> integrationResponse2,
    integrationName3 -> integrationResponse3
  )

  def integrationResponseJson(startWith: String): String = {
    val resps = integrationResponses.flatMap {
      case (name, jsonStr) if name.startsWith(startWith) => Some(jsonStr)
      case _ => None
    }.mkString(",")

    s"""
       |{
       |  "has_more" : false,
       |  "services" : [
       |    $resps
       |  ]
       |}
     """.stripMargin
  }

  private val nameRegex = """.*name=([A-Za-z0-9_]+)""".r

  def createIntegrationsJson(path: String): Option[JSONObject] =
    (path match {
      case nameRegex(n) => Some(n)
      case _ => Some("")
    }).map(n => new JSONObject(integrationResponseJson(n)))

  private val idRegex = """.*/([A-Za-z0-9\\-]+)""".r

  def createProviderJson(path: String): Option[JSONObject] =
    (path match {
      case idRegex(id) => Some(id)
      case _ => None
    }).flatMap(pId => providers.get(pId)).map(new JSONObject(_))

}
