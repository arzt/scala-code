package com.github.arzt.audio

import java.io.File

class LittleEndianSeq(in: Seq[Byte]) extends Seq[Int] {
  override val length: Int = in.length / 2

  override def apply(idx: Int): Int = {
    val b = in(2 * idx)
    val a = in(2 * idx + 1)
    a << 8 | b & 255
  }

  def toDoubleSeq: Seq[Double] = this.map(_ / Short.MaxValue.toDouble)

  override def iterator: Iterator[Int] = Iterator.range(0, length).map(this)
}
