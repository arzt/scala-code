package com.github.arzt.audio

import java.io.File
import java.util.concurrent.{Executors, TimeUnit}
import javax.sound.sampled.{AudioSystem, DataLine, SourceDataLine}
import scala.collection.immutable.ArraySeq
import scala.runtime.Arrays

class HearTheDifference(paths: Array[String], var index: Int = 0) extends Runnable:

  def setIndex(i: Int): Unit =
    println(f"Setting intex ${i}: ${paths(i)}")
    index = i

  def run(): Unit =
    val bufferSize = 128
    val streams = paths.map(new File(_)).map(AudioSystem.getAudioInputStream)
    val buf = new Array[Byte](bufferSize)
    val format = streams(0).getFormat()
    val line = AudioSystem.getSourceDataLine(format)
    line.open(format)
    line.start()
    val bytesBuffer = new Array[Byte](bufferSize)
    var bytesRead = 0
    while (bytesRead != -1)
      val reads = streams
        .indices
        .map(
          i => {
            if i == index then
              streams(i).read(buf)
            else
              streams(i).skip(bufferSize)
          }
        )
      line.write(buf, 0, bufferSize)
    line.drain()
    line.close()


object HearTheDifference:
  def main(args: Array[String]): Unit =
    val numFiles = args.length
    val htd = new HearTheDifference(args)
    val executor = Executors.newScheduledThreadPool(1);
    val future = executor.schedule(htd, 0, TimeUnit.SECONDS)
    while(true) {
      Thread.sleep(500)
      val index = (math.random() * numFiles).toInt
      htd.setIndex(index)
      println(f"switch: ${htd.index}")
    }
    future.wait()
    println("done!")




