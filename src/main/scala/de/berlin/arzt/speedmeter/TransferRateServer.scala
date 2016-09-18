package de.berlin.arzt.speedmeter

import java.net.ServerSocket

import scala.annotation.migration

object TransferRateServer {
  def main(args: Array[String]) {
    val Array(portStr) = args
    val port = portStr.toInt
    val server = new ServerSocket(port);
    println(s"Server started on port '$port'")
    val socket = server.accept()
    println("Server successfully connected to client...")
    val stream = socket.getInputStream()
    val buf = new Array[Byte](1024 * 1024)
    val a = Iterator
      .continually {
        stream.read(buf)
      }
      .takeWhile(_ > -1)
      .map {
        x =>
          (x, System.currentTimeMillis())
      }
    toBytesPerMs(a)
      .map { x => s"${x * 1000 / 1024 / 1024} MB/s" }
      .foreach(println)
    println("Server stopped")
  }

  def toBytesPerMs(in: Iterator[(Int, Long)]) = in
    .scanLeft(0, 0L) {
      case ((sumBytes, _), (bytes, time)) if (time % 1000 < time) =>
        (bytes, time % 1000)
      case ((sumBytes, _), (bytes, time)) =>
        (sumBytes + bytes, time % 1000)
    }
    .sliding(2, 1)
    .collect {
      case (bytes, milliseconds) :: (_, nextTime) :: Nil if milliseconds > nextTime =>
        1d * bytes / milliseconds
    }

}