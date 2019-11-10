package com.github.arzt.exercise

import java.io.BufferedInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.ServerSocket
import java.nio.channels.ServerSocketChannel

import scala.io.Source

object Exercise {
  def main(args: Array[String]): Unit = {
    val s = new ServerSocket(9000, 1)
    ServerSocketChannel.open()

   Iterator
      .continually{
        s.getChannel
        Option(s.getChannel)
      }
      .flatten
      .map {
        channel =>
          channel.accept()
      }
      .foreach(println)
  }
}
