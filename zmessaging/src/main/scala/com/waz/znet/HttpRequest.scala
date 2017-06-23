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
package com.waz.znet

import com.koushikdutta.async.http._
import com.waz.utils.wrappers.{AndroidURI, URI}
import com.waz.znet.ContentEncoder.{EmptyRequestContent, RequestContent}

import scala.concurrent.duration._
import scala.collection.JavaConverters._

trait HttpRequest {
  def absoluteUri: Option[URI]
  def httpMethod: String
  def getBody: RequestContent
  def headers: Map[String, String]
  def followRedirect: Boolean
  def timeout: FiniteDuration
}

class HttpRequestImpl(val req: AsyncHttpRequest) extends HttpRequest {
  override val absoluteUri: Option[URI] = Some(new AndroidURI(req.getUri()))
  override val httpMethod: String = req.getMethod()
  override val getBody: RequestContent = EmptyRequestContent // TODO
  override val headers: Map[String, String] = HttpRequest.getHeaders(req)
  override val followRedirect: Boolean = req.getFollowRedirect()
  override val timeout: FiniteDuration = req.getTimeout().millis
}

object HttpRequest {
  import scala.language.implicitConversions

  def apply(req: AsyncHttpRequest): HttpRequest = new HttpRequestImpl(req)

  implicit def wrap(req: AsyncHttpRequest): HttpRequest = apply(req)
  implicit def unwrap(req: HttpRequest): AsyncHttpRequest = req match {
    case wrapper: HttpRequestImpl => wrapper.req
    case _ => throw new IllegalArgumentException(s"Expected AsyncHttpRequest, but tried to unwrap: $req")
  }

  def getHeaders(req: AsyncHttpRequest) = {
    val m = req.getHeaders().getMultiMap()
    m.keySet().asScala.toSet[String].map(k => (k -> m.getString(k))).toMap
  }
}

trait RequestWorker {
  def processRequest(request: HttpRequest, additionalHeaders: (String, String)*): HttpRequest
}

class HttpRequestImplWorker extends RequestWorker {
  override def processRequest(request: HttpRequest, additionalHeaders: (String, String)*): HttpRequest = {
    val r = new AsyncHttpRequest(URI.unwrap(request.absoluteUri.getOrElse(throw new IllegalArgumentException("URI not provided")).normalizeScheme), request.httpMethod)
    r.setTimeout(request.timeout.toMillis.toInt)
    r.setFollowRedirect(request.followRedirect)

    // if 'r' is created with some headers, they have the highest priority, then headers from 'request', and only then 'additionalHeaders'
    val h1 = HttpRequest.getHeaders(r).filter(_._2 != "")
    val h2 = request.headers.filterKeys(key => !h1.contains(key))
    val h3 = additionalHeaders.toMap.filterKeys(key => !h1.contains(key) && !h2.contains(key))
    (h1 ++ h2 ++ h3).foreach {
      case (key, value) => r.getHeaders.set(key, value)
      case _ =>
    }

    new HttpRequestImpl(request.getBody.apply(r))
  }
}