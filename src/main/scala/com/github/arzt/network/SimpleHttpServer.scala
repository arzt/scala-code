package com.github.arzt.network
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.io.FileInputStream
import java.net.InetSocketAddress
import java.util.concurrent.ThreadPoolExecutor
import javax.imageio.ImageIO

object SimpleHttpServer:

  val root: HttpHandler =
    exchange =>
      println(exchange)
      val response = "<html><h1>Hello World</h1></html>"
      exchange.sendResponseHeaders(200, response.length)
      val stream = exchange.getResponseBody
      val headers = exchange.getResponseHeaders
      stream.write(response.getBytes)
      stream.flush()
      stream.close()
      exchange.close()

  val img: HttpHandler =
    exchange =>
      println("img")
      val fis = new FileInputStream("/home/sebastian/Desktop/pic-waiter-carrying-drinks.jpg")
      val bytes = fis.readAllBytes()
      val stream = exchange.getResponseBody
      val length = bytes.length
      exchange.sendResponseHeaders(200, length)
      stream.write(bytes)
      stream.flush()
      stream.close()
      exchange.close()

  val flac: HttpHandler =
    exchange =>
      println("flac")
      val fis = new FileInputStream("/home/sebastian/Downloads/other/music/The Cure - Assemblage - 1991 (12CD FLAC)/The Cure - Assemblage - 1991 (12CD FLAC)/02 - Boys Don't Cry (1980)/01 - Boys Don't Cry.flac")
      val bytes = fis.readAllBytes()
      val stream = exchange.getResponseBody
      val length = bytes.length
      exchange.sendResponseHeaders(200, length)
      stream.write(bytes)
      stream.flush()
      stream.close()
      exchange.close()


  val flac2: HttpHandler =
    exchange =>
      println("flac2")
      val fis = new FileInputStream("/home/sebastian/Downloads/other/music/The Cure - Assemblage - 1991 (12CD FLAC)/The Cure - Assemblage - 1991 (12CD FLAC)/02 - Boys Don't Cry (1980)/01 - Boys Don't Cry.flac")
      val length = fis.available()
      val buffer = new Array[Byte](1024*100)
      val stream = exchange.getResponseBody
      exchange.sendResponseHeaders(200, length)
      var count = 0
      var read = fis.read(buffer)
      while (read > -1)
        stream.write(buffer, 0, read)
        read = fis.read(buffer)
        Thread.sleep(1000)
        println(f"hui ${count} read: ${read}")
        count += 1
      println("done")
      stream.flush()
      stream.close()
      exchange.close()


  def main(args: Array[String]): Unit =
    import java.util.concurrent.Executors

    val executor = Executors.newFixedThreadPool(10)
    val server = HttpServer.create(new InetSocketAddress("192.168.178.73", 8001), 0)

    server.createContext("/", root)
    server.createContext("/img", img)
    server.createContext("/flac", flac)
    server.createContext("/flac2", flac2)
    server.setExecutor(executor)
    server.start()
    print(server)
    println("first")
    println("test")
