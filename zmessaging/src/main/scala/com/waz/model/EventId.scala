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

import scala.util.Random

case class EventId(sequence: Long, hex: String) extends Ordered[EventId] {
  require(sequence >= 0L, s"illegal sequence: $sequence")

  lazy val str = s"${sequence.toHexString}.$hex"

  override def toString: String = str

  override def compare(o: EventId): Int = {
    val res = sequence.compareTo(o.sequence)
    if (res == 0) hex.compareTo(o.hex)
    else res
  }

  def floor = EventId(sequence, "0")

  def ceil = if (sequence == Long.MaxValue) EventId.MaxValue else EventId(sequence + 1, "0")
  def max(o: EventId) = if (this >= o) this else o
  def min(o: EventId) = if (this <= o) this else o

  def isLocal = EventId.isLocal(this)
  def nonLocal = !isLocal
}

object EventId {

  val LocalMarker = '!'
  val Zero = new EventId(0, hexString(0))
  val One = new EventId(1, hexString(0))
  val MaxValue = new EventId(Long.MaxValue, "FFFFFFFFFFFFFFFF")

  def apply(seq: Long): EventId = EventId(seq, hexString(Random.nextLong()))
  def apply(str: String): EventId = {
    require(str != null)
    val dot = str.indexOf('.')
    if (dot == 16 && str(0) >= '8') EventId.Zero // backwards compatibility hack: negative numbers will be set to zero
    else EventId(java.lang.Long.parseLong(str.substring(0, dot), 16), str.substring(dot + 1))
  }

  def isLocal(id: EventId) = id.hex(0) == LocalMarker

  def local(seq: Long) = EventId(seq, localHexString(0))

  def next(ev: EventId): EventId =  EventId(ev.sequence + 1, hexString(0))

  def nextLocal(lastEvent: EventId) = {
    if (isLocal(lastEvent)) EventId(lastEvent.sequence, localHexString(hexNumber(lastEvent.hex) + 1))
    else EventId(lastEvent.sequence + 1, localHexString(0))
  }

  /**
   * Returns a hex string from given number, string is padded with zeros so that lexical ordering is working as expected in db.
   */
  def hexString(v: Long) = padHexWithZeros(v.toHexString)

  def hexNumber(hex: String) = java.lang.Long.parseLong(hex.substring(1), 16)

  def localHexString(v: Long) = LocalMarker + padHexWithZeros(v.toHexString, 15)

  def padHexWithZeros(str: String, len: Int = 16) = {
    val res = new StringBuilder
    for(_ <- 1 to len - str.length) res.append('0')
    res.append(str).toString()
  }

  implicit object Id extends Id[EventId] {
    override def random() = EventId {
      val rnd = math.abs(Random.nextLong())
      if (rnd == Long.MinValue) Long.MaxValue else rnd
    }
    override def decode(str: String) = EventId(str)
    override def compare(x: EventId, y: EventId): Int = x.compare(y)
  }
}
