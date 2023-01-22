package com.github.arzt.audio

import java.io.File
import java.io.InputStream

import com.github.arzt.audio.Const._
import com.github.arzt.audio.MusicImplicits.FunctionOps
import com.github.arzt.audio.MusicImplicits.const
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem

object Audio {

  def main(args: Array[String]): Unit = {
    val path = "/home/sebastian/Desktop/audio/Muse - 01 - Sunburn-stereo.wav"

    val in = AudioSystem.getAudioInputStream(new File(path))
    val origBytes = streamToBytes()(in).toArray
    val format = in.getFormat
    /*

    val shortInput = new LittleEndianSeq(MappedByteSeq(new File(path)))
    val data = readDoublesLittleEndian(in)
    val head1 = data.take(1000000)
    val audio = toMono(data, format)
    val (left, right) = toStereo(data, format)
    val doubles = Iterator
      .iterate(0)(_ + 1)
      .map(_.toDouble)
      .map(
        (id / 44100d) andThen audio
      )
    val samples = stereoToSamples(left, right, format)
    val head2 = doubles.take(1000000).toArray
    val shortOutput = doubleToShorts(head1.iterator)
    val byteOutput = shortsToBytes(shortOutput)
     * */
    playBackBytes(origBytes.iterator, format)

  }

  def monoToSamples(
      wave: Double => Double,
      format: AudioFormat
  ): Iterator[Double] = {
    Iterator
      .iterate(0L)(_ + 1)
      .map(_.toDouble)
      .map(id / format.getSampleRate.toDouble andThen wave)
  }

  def stereoToSamples(
      left: Double => Double,
      right: Double => Double,
      format: AudioFormat
  ): Iterator[Double] = {
    val leftIt = monoToSamples(left, format)
    val rightIt = monoToSamples(right, format)
    Iterators.interleave(leftIt, rightIt)
  }

  def playBackMono(wave: Double => Double, format: AudioFormat): Unit =
    playBack(monoToSamples(wave, format), format)

  def playBackStereo(
      left: Double => Double,
      right: Double => Double,
      format: AudioFormat
  ): Unit = {}

  def readDoublesLittleEndian(in: AudioInputStream): Array[Double] = {
    //streamToBytes andThen bytesToShorts andThen shortsToDouble
    val c = bytesToShorts(streamToBytes()(in)).map(_ / Short.MaxValue.toDouble)
    val length = in.getFrameLength.toInt * in.getFormat.getChannels
    val out = new Array[Double](length)
    var i = 0
    while (i < length) {
      out(i) = c.next()
      i += 1
    }
    out
  }

  def playBackBytes(
      output: Iterator[Byte],
      format: AudioFormat,
      size: Int = 1024
  ): Unit = {
    val outBuf: Array[Byte] = new Array[Byte](size)
    val data = AudioSystem.getSourceDataLine(format)
    var pos = 0
    data.open()
    data.start()
    output
      .map { b =>
        outBuf(pos) = b
        if (pos + 1 == outBuf.length) {
          pos = 0
        } else {
          pos += 1
        }
        pos == 0
      }
      .foreach { write =>
        if (write) data.write(outBuf, 0, outBuf.length)
      }
    data.drain()
    data.stop()
  }

  def playBack(
      data: Iterator[Double],
      format: AudioFormat,
      size: Int = 1024
  ): Unit = {
    val shortOutput = doubleToShorts(data)
    val byteOutput = shortsToBytes(shortOutput)
    playBackBytes(byteOutput, format, size)
  }

  val bytesToShorts: Iterator[Byte] => Iterator[Short] =
    in => {
      Iterator
        .continually(in.hasNext)
        .takeWhile(x => x)
        .map { _ =>
          val b = in.next()
          val a = in.next()
          (a << 8 | b & 255).toShort
        }
    }

  def shortsToDouble: Iterator[Int] => Iterator[Double] =
    _.map(x => 1.0 * x / Short.MaxValue)

  def doubleToShorts: Iterator[Double] => Iterator[Short] =
    _.map(x => {
      val r = (math.max(math.min(x, 1.0), -1.0) * Short.MaxValue).toShort
      r
    })

  val shortsToBytes: Iterator[Short] => Iterator[Byte] = { x =>
    {
      new Iterator[Byte] {
        var s: Short = 0
        var dingDong = true

        var hasNext: Boolean = x.hasNext

        override def next(): Byte = {
          val byte =
            if (dingDong) {
              s = x.next()
              (s & 255).toByte
            } else {
              hasNext = x.hasNext
              (s >> 8).toByte
            }
          dingDong = !dingDong
          byte
        }
      }
    }
  }

  def streamToBytes(size: Int = 1024): InputStream => Iterator[Byte] =
    in => {
      val buf = new Array[Byte](size)
      Iterator
        .continually(in.read(buf, 0, buf.length))
        .takeWhile(_ > -1)
        .flatMap(buf.iterator.take)
    }
}
