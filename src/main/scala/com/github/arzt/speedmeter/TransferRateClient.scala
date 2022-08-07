package com.github.arzt.speedmeter

import java.net.Socket
import scala.util.Random

object TransferRateClient {
  def main(args: Array[String]): Unit = {
    val Array(host, port) = args
    val socket = new Socket(host, port.toInt)
    println("Client connected to server...")
    val os = socket.getOutputStream()
    val buf = new Array[Byte](1024 * 100)
    val a = Iterator
      .continually {
        Random.nextBytes(buf)
        println("printting!")
        os.write(buf)
        (buf.length, System.currentTimeMillis())
      }
    TransferRateServer
      .toBytesPerMs(a)
      .map { x => s"${x * 1000} MB/s" }
      .foreach(println)
    println("Client stopped")
  }
}
