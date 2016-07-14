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
import android.os.Build
import com.waz.ZLog._
import com.waz.bitmap.gif.AnimGifDecoder.JavaImageDecoder
import com.waz.bitmap.gif.Gif.{Disposal, Frame}
import com.waz.bitmap.gif.ImageDecoderHelper.PixelConsumer

import scala.concurrent.duration.Duration

/**
 * Decodes gif animation frames.
 *
 * TODO: consider keeping pixels data in native buffer (especially for swap pixels)
 * TODO: maybe image decoder could save pixels directly to current image (line bye line), would not need pixels buffer
 * @param gif
 */
class AnimGifDecoder(context: Context, gif: Gif) {

  private implicit val logTag: LogTag = logTagFor[AnimGifDecoder]

  val framesCount = gif.frames.length
  var frameIndex = -1
  var loopCounter = 0
  var frameDirty = false

  var currentImage: Bitmap = _

  val decoder = if (AnimGifDecoder.useRS) {
    try {
      new RsLzwDecoder(context, gif)
    } catch {
      case e: Exception =>
        error("Could not create RSLzwDecoder, falling back to java implementation", e)
        new JavaImageDecoder(gif)
    }
  } else
    new JavaImageDecoder(gif)

  /**
   * Returns a delay to wait before displaying next frame.
   * @return finite duration if there is next frame to show (or looping) or Duration.Inf if this is the last frame and loopCount is finished
   */
  def getFrameDelay: Duration = {
    if (frameIndex < 0) Duration.Zero
    else if (gif.loop.shouldAnimate(loopCounter)) gif.frames(frameIndex).delay
    else Duration.Inf
  }


  private def advance(): Boolean = {
    if (frameIndex == framesCount - 1) loopCounter += 1
    if (gif.loop.shouldAnimate(loopCounter)) {
      frameIndex = if (frameIndex == framesCount - 1) 0 else frameIndex + 1
      true
    } else false
  }

  /**
   * Advances animation.
   * Will decode next frame pixels, but not modify current frame image yet.
   */
  def advanceNextFrame(): Unit = {
    if (frameDirty) warn(s"should call getCurrentFrame before advancing to next frame")
    if (!frameDirty && advance()) {
      val frame = gif.frames(frameIndex)
      if (frameIndex == 0) decoder.clear()
      decoder.decode(frame)
      frameDirty = true
    }
  }

  /**
   * Returns current frame image.
   * @return Bitmap representation of frame
   */
  def getCurrentFrame: Bitmap = {
    if (frameDirty) {
      currentImage = decoder.updateImage(gif.frames(frameIndex))
      frameDirty = false
    }
    currentImage
  }

  def destroy() = decoder.destroy()
}

object AnimGifDecoder {

  val SamsungS7 = """SM-G93[05].*""".r

  lazy val useRS = Build.MODEL.toUpperCase match {
    case SamsungS7() =>
      // disable Renderscript on Exynos based S7 and S7 edge,
      // there are issues with GPU on those devices, RS blocks gpu rendering for long time
      false
    case _ => true
  }

  trait ImageDecoder {
    def clear(): Unit

    /**
     * Decode next frame pixels, but does not modify current frame image yet.
     */
    def decode(frame: Gif.Frame): Unit

    /**
     * Updates frame image from current pixel data (and clears pixel buffer according to disposition codes).
     */
    def updateImage(frame: Gif.Frame): Bitmap

    def destroy(): Unit = {}
  }

  class JavaImageDecoder(gif: Gif) extends ImageDecoder {
    private implicit val tag: LogTag = logTagFor[JavaImageDecoder]

    var act = gif.gct
    var bgIndex = gif.bgIndex
    var bgColor = if (act.isEmpty) Color.TRANSPARENT else act(bgIndex)
    val currentImage = Bitmap.createBitmap(gif.width, gif.height, Bitmap.Config.ARGB_8888)
    val pixels = Array.fill[Int](gif.width * gif.height)(bgColor)
    lazy val pixelsSwap = new Array[Int](gif.width * gif.height)

    val imageDecoder = new ImageDecoderHelper()
    val frameBuffer = new Array[Byte](gif.frames.view.map(f => f.bounds.w * f.bounds.h).max)

    val consumer = new PixelConsumer(pixels, gif.gct, gif.width, gif.height)

    override def clear(): Unit = {
      ImageDecoderHelper.clear(pixels, Color.TRANSPARENT)
    }

    override def decode(frame: Frame): Unit = {
      act = if (frame.lct.isEmpty) gif.gct else frame.lct

      if (frame.dispose == Disposal.Previous) {
        System.arraycopy(pixels, 0, pixelsSwap, 0, pixels.length)
      }

      consumer.reset(frame)
      imageDecoder.decodeBitmapData(gif.data(frame), frame.bounds.w, frame.bounds.h, consumer)
    }

    override def updateImage(frame: Frame): Bitmap = {
      val b = frame.bounds
      currentImage.setPixels(pixels, b.y * gif.width + b.x, gif.width, b.x, b.y, b.w, b.h)

      frame.dispose match {
        case Disposal.Previous => System.arraycopy(pixelsSwap, 0, pixels, 0, pixels.length)
        case Disposal.Background =>
          val b = frame.bounds
          ImageDecoderHelper.clearRect(pixels, gif.width, b.x, b.y, b.w, b.h, if (frame.transparency) Color.TRANSPARENT else bgColor)
        case _ => // ignore
      }
      currentImage
    }
  }

}
