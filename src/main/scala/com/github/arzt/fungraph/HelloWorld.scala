package com.github.arzt.fungraph

import com.github.arzt.fungraph.function.waves

import java.awt.Frame
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.util.concurrent.ScheduledThreadPoolExecutor

object HelloWorld:

  def drawFunction(
      t: Double,
      f: (Double, Double, Double, Int) => Double,
      img: BufferedImage
  ): Unit =
    val width = img.getWidth()
    val height = img.getHeight()
    var i = 0
    while i < width * height do
      val x = i % width
      val y = i / width
      val alpha = (f(t, x, y, 0) * 255).toInt % 256
      val red = (f(t, x, y, 1) * 255).toInt % 256
      val green = (f(t, x, y, 2) * 255).toInt % 256
      val blue = (f(t, x, y, 3) * 255).toInt % 256
      val argb = (alpha << 24) + (red << 16) + (green << 8) + blue
      img.setRGB(x, height - y - 1, argb)
      i += 1

  def main(args: Array[String]): Unit =
    val width = 200
    val height = 200
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val wind = new Frame("Hello")
    wind.setLocation(100, 100)
    wind.setSize(image.getWidth(), image.getHeight())
    val canvas = new ImgCanvas(image)
    wind.add(canvas)
    wind.setVisible(true)
    val executor = new ScheduledThreadPoolExecutor(2)
    val runner: Runnable = () => {
      val t = System.currentTimeMillis() / 1000.0
      drawFunction(t, waves, image)
    }

    wind.addWindowListener(
      new WindowAdapter:
        override def windowClosing(e: WindowEvent): Unit =
          executor.shutdown()
          wind.dispose()
          System.exit(0)
    )

    var count = 0
    var last = System.currentTimeMillis()
    while true do
      val t = System.currentTimeMillis()
      val fps = 1000.0 / (t - last)
      last = t
      runner.run()
      canvas.setImage(image)
      count += 1
      Thread.sleep(0, 10)
      println(fps)
    end while
