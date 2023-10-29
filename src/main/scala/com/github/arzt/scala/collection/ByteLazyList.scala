package com.github.arzt.scala.collection

object ByteLazyList:

  def intFromBytes(a: Byte, b: Byte, c: Byte, d: Byte): Int =
    val a1 = (a & 0xff) << 24
    val a2 = (b & 0xff) << 16
    val a3 = (c & 0xff) << 8
    val a4 = (d & 0xff) << 0
    a1 | a2 | a3 | a4

  extension (integer: Int)
    def getByte(i: Int): Byte = (integer >> (i*8)).toByte


  extension (l: LazyList[Byte])
    def prependInt(i: Int): LazyList[Byte] =
      val d = i & 15.toByte
      val c = (i >> 8) & 15.toByte
      val b = (i >> 16) & 15.toByte
      val a = (i >> 24) & 15.toByte
      a.toByte +: b.toByte +: c.toByte +: d.toByte +: l

