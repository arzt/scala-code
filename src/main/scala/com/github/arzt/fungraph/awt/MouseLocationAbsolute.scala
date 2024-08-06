package com.github.arzt.fungraph.function

import java.awt.{MouseInfo, Point}
import java.util.concurrent.{
  ScheduledExecutorService,
  ScheduledThreadPoolExecutor,
  TimeUnit
}

class MouseLocationAbsolute(
    val executor: ScheduledExecutorService = new ScheduledThreadPoolExecutor(1)
):

  private var location: Point = null
  private var lastTime: Double = 1.0
  private var time: Double = 0.0

  private val command: Runnable =
    () =>
      val old = location
      val next = MouseInfo.getPointerInfo.getLocation
      if (next != location && lastTime != time)
        location = next
        lastTime = time
        println(s"Mouse: ${location}")
  executor.scheduleWithFixedDelay(command, 0, 5, TimeUnit.MILLISECONDS)

  def locationX(t: Double): Double =
    time = t
    location.getX

  def locationY(t: Double): Double =
    time = t
    location.getY

object MouseLocationAbsolute:

  def main(args: Array[String]): Unit =
    val test = new MouseLocationAbsolute()
    Thread.sleep(10000)
