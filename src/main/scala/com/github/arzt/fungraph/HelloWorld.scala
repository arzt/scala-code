package com.github.arzt.fungraph

import com.github.arzt.fungraph.awt.MouseLocationRelative
import com.github.arzt.fungraph.function.{
  MouseLocationAbsolute,
  circle,
  waves,
  colors
}

import java.awt.Frame
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.lang.System.currentTimeMillis
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import scala.sys.exit

object HelloWorld:

  def drawFunction(
      t: Double,
      f: (Double, Double, Double, Int) => Double,
      img: BufferedImage,
      width: Int,
      height: Int
  ): Unit =
    var i = 0
    while i < width * height do
      val x = i % width
      val y = i / width
      val yF = height - y - 1
      val alpha = (f(t, x, yF, 0) * 255).toInt % 256
      val red = (f(t, x, yF, 1) * 255).toInt % 256
      val green = (f(t, x, yF, 2) * 255).toInt % 256
      val blue = (f(t, x, yF, 3) * 255).toInt % 256
      val argb = (alpha << 24) + (red << 16) + (green << 8) + blue
      img.setRGB(x, y, argb)
      i += 1

  def main(args: Array[String]): Unit =
    val width = 2000
    val height = 2000
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val wind = new Frame("Hello")
    wind.setLocation(100, 100)
    wind.setSize(600, 400)
    val canvas = new ImgCanvas(image)
    wind.add(canvas)
    wind.setVisible(true)
    val executor = new ScheduledThreadPoolExecutor(2)

    wind.addWindowListener(
      new WindowAdapter:
        override def windowClosing(e: WindowEvent): Unit =
          executor.shutdown()
          wind.dispose()
          exit(0)
    )

    val uiFunctions =
      new MouseLocationAbsolute()
      new MouseLocationRelative(canvas)
    var count = 0
    var last = currentTimeMillis()

    // def fun = circle(centerX = uiFunctions.getX, centerY = uiFunctions.getY)
    def fun = colors

    val update: Runnable =
      () => {
        val t = currentTimeMillis()
        val fps = 1000.0 / (t - last)
        last = t
        drawFunction(
          t / 1000.0,
          fun,
          image,
          canvas.getWidth,
          canvas.getHeight
        )
        // canvas.repaint()
        count += 1
        Thread.sleep(0, 10)
        println(fps)
      }
    val repaint: Runnable = () =>
      canvas.repaint()
      println("repaint!")
    executor.scheduleWithFixedDelay(update, 0, 1, TimeUnit.NANOSECONDS)
    executor.scheduleWithFixedDelay(repaint, 0, 1, TimeUnit.MILLISECONDS)
    // while true do
    // runner.run()
