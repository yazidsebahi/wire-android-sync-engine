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
package com.waz.utils

import java.io._
import java.security.MessageDigest
import java.util.zip.GZIPOutputStream

import com.waz.ZLog

import scala.util.control.NonFatal

object IoUtils {
  private val buffer = new ThreadLocal[Array[Byte]] {
    override def initialValue(): Array[Byte] = new Array[Byte](8096)
  }

  def copy(in: InputStream, out: OutputStream): Unit = {
    try {
      val buff = buffer.get()
      Iterator continually (in read buff) takeWhile (_ != -1) foreach (size => out.write(buff, 0, size))
      out match {
        case out: FileOutputStream =>
          out.flush()
          out.getFD.sync()
        case _ => // nothing to do
      }
    } finally {
      // make sure both streams are closed
      var rethrow = None: Option[Throwable]
      try { out.close() } catch { case NonFatal(ex) => rethrow = Some(ex) }
      try { in.close() } catch { case NonFatal(ex) => rethrow = Some(ex) }
      rethrow foreach(throw _)
    }
  }

  def copy(in: InputStream, out: File): Unit = {
    out.getParentFile.mkdirs()
    copy(in, new FileOutputStream(out))
  }

  def copy(in: File, out: File): Unit = {
    out.getParentFile.mkdirs()
    copy(new FileInputStream(in), new FileOutputStream(out))
  }

  def toByteArray(in: InputStream) = {
    val out = new ByteArrayOutputStream()
    copy(in, out)
    out.toByteArray
  }

  def gzip(data: Array[Byte]) = {
    val bos = new ByteArrayOutputStream()
    withResource(new GZIPOutputStream(bos)) { os =>
      os.write(data)
      os.finish()
    }
    bos.toByteArray
  }

  def asString(in: InputStream) = new String(toByteArray(in), "utf8")

  def withResource[I <: Closeable, O](in: I)(op: I => O): O = try op(in) finally in.close()

  def md5(file: File): Array[Byte] = hash(file, "MD5")
  def md5(stream: => InputStream): Array[Byte] = hash(stream, "MD5")
  def sha256(file: File): Array[Byte] = hash(file, "SHA-256")
  def sha256(is: InputStream): Array[Byte] = hash(is, "SHA-256")

  def hash(file: File, hashAlgorithm: String): Array[Byte] = hash(new FileInputStream(file), hashAlgorithm)

  def hash(stream: => InputStream, hashAlgorithm: String): Array[Byte] = withResource(stream) { in =>
    val digest = MessageDigest.getInstance(hashAlgorithm)
    val buff = buffer.get()
    Iterator continually (in read buff) takeWhile (_ != -1) foreach (size => digest.update(buff, 0, size))
    digest.digest()
  }

  def deleteRecursively(file: File): Unit =  {
    if (file.isDirectory)
      Option(file.listFiles()).foreach(_ foreach deleteRecursively)
    file.delete()
  }
}
