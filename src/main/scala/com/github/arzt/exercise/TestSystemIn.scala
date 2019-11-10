package com.github.arzt.exercise

import java.io.InputStreamReader
import java.nio.CharBuffer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object TestSystemIn {
  def main(args: Array[String]): Unit = {
    val buf = new Array[Char](12)
    val in = System.in
    val reader = new InputStreamReader(in)
    while (true) {
      val avail = {
        //reader.ready()
        in.available()
      }
      if (avail > 0) {
        val n = reader.read(buf)
        println(n)
        println(new String(buf, 0, n))
      }
    }
  }
}
