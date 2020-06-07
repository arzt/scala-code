package com.github.arzt.audio

import org.scalatest.FreeSpec
import org.scalatest.Matchers

import scala.util.Random

class AudioTest extends FreeSpec with Matchers {

  import Audio.bytesToShorts
  import Audio.shortsToBytes

  def randomShort(r: Random): Short = {
    r.nextInt(Short.MaxValue).toShort
    (r.nextInt()%Short.MaxValue).toShort
  }

  "Audio test" - {
    "bytes to shorts" in {
      def res(a: Byte, b: Byte): Int = Audio.bytesToShorts(Array[Byte](b, a).iterator).toSeq.head

      res(0, 0) shouldBe 0
      res(0, 1) shouldBe 1
      res(0, 127) shouldBe 127
      res(0, -128) shouldBe 128
      res(0, -127) shouldBe 129
      res(0, -1) shouldBe 255
      res(1, 0) shouldBe 256
      res(1, 127) shouldBe 256 + 127
      res(1, -1) shouldBe 256 + 255
      res(1, -1) shouldBe 256 + 255
      res(127, 0) shouldBe 256 * 127
      res(127, -1) shouldBe 256 * 127 + 255
      res(-128, 0) shouldBe -256 * 128
      res(-1, -1) shouldBe -1
    }
    "shorts to bytes" in {
      def res(short: Short) = {
        val Seq(a, b) = Audio.shortsToBytes(Iterator(short)).toArray.toSeq
        (a, b)
      }

      res(0) shouldBe (0, 0)
      res(1) shouldBe (1, 0)
    }
    "shorts to bytes 2" in {
      val r = new Random()
      val in = Array.fill[Short](10)(randomShort(r)).toSeq
      val id = shortsToBytes andThen bytesToShorts
      val seq = id(in.iterator).toArray.toSeq
      seq shouldBe in
    }
    "any short to short" in {
      val r = new Random()
      val in = Array.fill[Short](100)(r.nextInt().toShort)

    }
  }


}
