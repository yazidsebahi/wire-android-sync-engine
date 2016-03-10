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
package com.waz.bitmap.gif

import android.content.Context
import android.graphics.{Bitmap, Color}
import android.support.v8.renderscript.{Allocation, Element, Int4, Short4}
import com.waz.bitmap.RenderScriptContext
import com.waz.bitmap.RenderScriptContext.RSNotAvailableException
import com.waz.bitmap.gif.AnimGifDecoder.ImageDecoder
import com.waz.zms.R

class RsLzwDecoder(context: Context, gif: Gif) extends ImageDecoder {
  if (RenderScriptContext.rs == null) throw RSNotAvailableException

  val imageWidth = gif.width
  val imageHeight = gif.height
  val maxImageDataSize = gif.frames.map(_.imageDataSize).max
  val globalColorTable = gif.gct
  val bgColor = if (globalColorTable.isEmpty) Color.TRANSPARENT else globalColorTable(gif.bgIndex)
  val _rs = RenderScriptContext.rs

  val dummy = Allocation.createSized(_rs, Element.U32(_rs), 1, Allocation.USAGE_SCRIPT)
  val inputData = new Array[Byte](maxImageDataSize)
  val inAllocation = Allocation.createSized(_rs, Element.U8(_rs), maxImageDataSize, Allocation.USAGE_SCRIPT)
  val colorAllocation = Allocation.createSized(_rs, Element.RGBA_8888(_rs), 256, Allocation.USAGE_SCRIPT)
  val pixelsAllocation = Allocation.createFromBitmap(_rs, Bitmap.createBitmap(gif.width, gif.height, Bitmap.Config.ARGB_8888))
  val decodeScript = new ScriptC_decode(_rs, context.getResources, R.raw.decode)
  val currentImage = Bitmap.createBitmap(gif.width, gif.height, Bitmap.Config.ARGB_8888)

  val usesDisposePrevious = gif.frames.exists(_.dispose == Gif.Disposal.Previous)

  val linesAllocation = {
    val alloc = Allocation.createSized(_rs, Element.U32(_rs), imageHeight, Allocation.USAGE_SCRIPT)
    alloc.copyFrom(Array.tabulate(imageHeight)(x => x))
    alloc
  }

  var swapCreated = false
  // temp, used only when frame disposal is set to PREVIOUS
  lazy val swapAllocation = {
    swapCreated = true
    Allocation.createFromBitmap(_rs, currentImage.copy(currentImage.getConfig, true))
  }

  val bgColorV = new Short4(Color.blue(bgColor).toShort, Color.green(bgColor).toShort, Color.red(bgColor).toShort, Color.alpha(bgColor).toShort)

  decodeScript.set_image(inAllocation)
  decodeScript.set_pixels(pixelsAllocation)
  decodeScript.set_colors(colorAllocation)
  decodeScript.set_width(imageWidth)
  decodeScript.set_height(imageHeight)
  decodeScript.set_bgColor(bgColorV)
  decodeScript.set_transIndex(0)

  val currentRect = new Int4(0, 0, imageWidth, imageHeight)
  decodeScript.set_fx(0)
  decodeScript.set_fy(0)
  decodeScript.set_fw(imageWidth)
  decodeScript.set_fh(imageHeight)

  override def clear(): Unit = {
    updateFrameBounds(0, 0, imageWidth, imageHeight)
    decodeScript.set_bgColor(new Short4(0, 0, 0, 0))
    decodeScript.forEach_clear(linesAllocation)
  }

  def decode(frame: Gif.Frame) = {
    gif.data(frame).readFully(inputData, 0, frame.imageDataSize)
    inAllocation.copyFromUnchecked(inputData)

    updateColorTable(frame.lct)
    updateFrameBounds(frame.bounds.x, frame.bounds.y, frame.bounds.w, frame.bounds.h)
    decodeScript.set_inputSize(frame.imageDataSize)
    decodeScript.set_transIndex(if (frame.transparency) frame.transIndex.toShort else 0)
    decodeScript.set_interlace(if (frame.interlace) 1 else 0)
    decodeScript.set_transparency(if (frame.transparency) 1 else 0)

    decodeScript.forEach_decode(dummy)
    _rs.finish()
  }

  def updateImage(frame: Gif.Frame) = {
    pixelsAllocation.copyTo(currentImage)

    frame.dispose match {
      case Gif.Disposal.Background =>
        decodeScript.set_bgColor(if (frame.transparency) new Short4(0, 0, 0, 0) else bgColorV)
        decodeScript.forEach_clear(linesAllocation)
      case Gif.Disposal.Previous =>
        pixelsAllocation.copyFrom(swapAllocation)
      case Gif.Disposal.None =>
        if (usesDisposePrevious) swapAllocation.copyFrom(pixelsAllocation)
      case _ =>
    }

    currentImage
  }

  def updateColorTable(local: Array[Int]) = {
    if (local.isEmpty) colorAllocation.copyFromUnchecked(globalColorTable)
    else colorAllocation.copyFromUnchecked(local)
  }

  def updateFrameBounds(x: Int, y: Int, width: Int, height: Int) = {
    if (x != currentRect.x) {
      currentRect.x = x
      decodeScript.set_fx(x)
    }
    if (y != currentRect.y) {
      currentRect.y = y
      decodeScript.set_fy(y)
    }
    if (width != currentRect.x) {
      currentRect.z = width
      decodeScript.set_fw(width)
    }
    if (height != currentRect.w) {
      currentRect.w = height
      decodeScript.set_fh(height)
    }
  }

  override def destroy() = {
    inAllocation.destroy()
    pixelsAllocation.destroy()
    linesAllocation.destroy()
    decodeScript.destroy()
    if (swapCreated) swapAllocation.destroy()
  }
}
