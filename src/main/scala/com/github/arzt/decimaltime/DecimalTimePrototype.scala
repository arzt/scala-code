package com.github.arzt.decimaltime

import java.time.Instant
import java.time.Instant.now
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}

object DecimalTimePrototype:

  def main(args: Array[String]): Unit =
    val totalMillisDay = 1000*60*60*24
    def fractionDay(): Double =
      1.0 * (now().toEpochMilli % totalMillisDay) / totalMillisDay

    val exec = new ScheduledThreadPoolExecutor(1)

    val printTimes: Runnable = () =>
      val frac = fractionDay()
      val millisDay = now().toEpochMilli % totalMillisDay
      val hours = millisDay/1000/60/60
      val hoursDec = (frac*10).toInt
      val min = (millisDay/1000/60) % 60
      val minDec = (frac*1000 % 100).toInt
      val sec = (millisDay/1000) % 60
      val secDec = (frac*100000 % 100).toInt
      val millis = (millisDay % 1000).toInt
      val millisDec = ((frac*100000*1000) % 1000).toInt
      println(f"$hoursDec%02d:$minDec%02d:$secDec%02d:$millisDec%03d $hours:$min%02d:$sec%02d:$millis%03d")
    val instantNow = Instant.now()
    val hui = 1000 - (System.currentTimeMillis() % 1000)
    val hui2 = System.currentTimeMillis()

    val s2 = 24*60*60*1000/100000
    exec.scheduleAtFixedRate(printTimes, hui, s2, TimeUnit.MILLISECONDS)
    exec.scheduleAtFixedRate(printTimes, hui, 1000, TimeUnit.MILLISECONDS)
