package com.github.arzt.newexcercise

import java.io.PrintStream
import java.net.Socket

import scala.concurrent.ExecutionContext.global
import scala.io.Source

object DataClient {
  def main(args: Array[String]): Unit = {
    implicit val contex = global
    val socket = new Socket("localhost", 9000)
    handleConnection2(socket)
  }

  def handleConnection2(socket: Socket): Unit = {

    val ps = new PrintStream(socket.getOutputStream)
    Source
      .fromInputStream(socket.getInputStream)
      .getLines()
      .map { line =>
        println(s"forward $line")
        line
      }
      .foreach(ps.println)
  }

}
