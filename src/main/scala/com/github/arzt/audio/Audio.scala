package com.github.arzt.audio

import java.io.File
import java.io.FileInputStream

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioFormat.Encoding.PCM_SIGNED
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem

object Audio {


  def main(args: Array[String]): Unit = {
    val system = AudioSystem.getMixerInfo
    val museMono = "/home/sebastian/Desktop/audio/Muse - 01 - Sunburn-mono.wav"
    val museStereo = "/home/sebastian/Desktop/audio/Muse - 01 - Sunburn-stereo.wav"

    val museMonoFormat = AudioSystem.getAudioFileFormat(new File(museMono)).getFormat
    val museStereoFormat = AudioSystem.getAudioFileFormat(new File(museStereo)).getFormat

    val dataLine = AudioSystem.getSourceDataLine(museMonoFormat)

    val stream = new FileInputStream(new File(museMono))
    val stream2 = new FileInputStream(new File(museMono))
    val in = stream //new AudioInputStream(stream, format, 1000000)
    val buf = new Array[Byte](100000)
    val buf2 = new Array[Byte](1000)
    inputStreamToByteIterator(stream2, buf2)
      .foreach { byte =>
        println(byte)
      }
    dataLine.open()
    dataLine.start()
    val isOpen = dataLine.isOpen
    val active = dataLine.isActive
    Iterator
      .continually(in.read(buf))
      .takeWhile(_ >= 0)
      .map { len =>
        dataLine.write(buf, 0, len)
        len
      }
      .zipWithIndex
      .foreach(println)
    dataLine.drain()
    dataLine.start()
    println("hello world")
  }

  def bytesToShorts(in: Iterator[Byte]): Iterator[Int] = {
    Iterator
      .continually(in.hasNext)
      .takeWhile(x => x)
      .map { _ =>
        val a = in.next()
        val b = in.next()
        a << 8 | b & 255
      }
  }

  def shortsToDouble: Iterator[Short] => Iterator[Double] =
    _.map(_ / Short.MaxValue)

  def doubleToShorts: Iterator[Double] => Iterator[Short] =
    _.map(x => (math.max(math.min(x, 1.0), -1.0)*Short.MaxValue).toShort)



  def shortsToBytes: Iterator[Short] => Iterator[Byte] = {
    x => {
      new Iterator[Byte] {
        var s: Short = 0
        var isSecond = false

        var hasNext: Boolean = x.hasNext

        override def next(): Byte = {
          if (isSecond) {
            hasNext = x.hasNext
            val out = (s & 255).toByte
            isSecond = false
            out
          } else {
            s = x.next()
            isSecond = true
            (s >> 8).toByte
          }
        }
      }
    }
  }

  def inputStreamToByteIterator(in: FileInputStream, buf: Array[Byte]): Iterator[Byte] = {
    Iterator
      .continually(in.read(buf, 0, buf.length))
      .takeWhile(_ >= 0)
      .flatMap { _ =>
        buf.iterator
      }
  }
}
