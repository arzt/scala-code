package com.github.arzt.audio

import java.io.File
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel.MapMode

class MappedByteSeq(buf: MappedByteBuffer) extends Seq[Byte] {
  override def length: Int = buf.capacity()

  override def apply(idx: Int): Byte = buf.get(idx)

  override def iterator: Iterator[Byte] =
    Iterator
      .range(0, length)
      .map(this)

}

object MappedByteSeq {
  def apply(file: File): MappedByteSeq = {
    val channel = new RandomAccessFile(file, "r").getChannel
    new MappedByteSeq(channel.map(MapMode.READ_ONLY, 0, channel.size()))
  }
}