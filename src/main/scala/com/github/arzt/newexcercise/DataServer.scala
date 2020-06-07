package com.github.arzt.newexcercise

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.net.ServerSocket
import java.net.Socket
import java.util.UUID.randomUUID
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future

object DataServer {

  def main(args: Array[String]): Unit = {
    val nUsers = 1000
    val ids = getRandomUsersIds(nUsers)
    val serverSocket = new ServerSocket(9000)

    val randomUsers = getRandomUsers(ids) //.take(4).toArray.toSeq
    println("serving")
    implicit val context: ExecutionContextExecutor = {
      //global
      ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
    }
    Iterator
      .iterate(0)(_ + 1)
      .foreach { id =>
        val socket = serverSocket.accept()
        Future(handleInput(id, socket))
        Future(handleOutput(id, socket))
      }
  }

  def handleOutput(id: Int, socket: Socket): Unit = {
    println(s"New output handler ${Thread.currentThread().getId}")
    val nUsers = 1000
    val ids = getRandomUsersIds(nUsers)
    val ps = new PrintStream(socket.getOutputStream)
    val randomUsers = getRandomUsers(ids) //.take(4).toArray.toSeq

    randomUsers
      .map(_.toString)
      .map { line =>
        //Thread.sleep(1000)
        line
      }
      .foreach(ps.println)
    ps.flush()
    ps.close()
    println("done sending")
    socket.close()
    println(s"done output handler ${Thread.currentThread().getId}")
  }

  def handleInput(id: Int, socket: Socket)(implicit
      ec: ExecutionContext
  ): Unit = {
    println(s"New input handler ${Thread.currentThread().getId}")
    val is = socket.getInputStream
    val reader = new BufferedReader(new InputStreamReader(is))
    Iterator
      .continually(reader.readLine())
      .takeWhile(_ != null)
      .map(x => s"Response client $id: $x")
      .foreach(println)
    println(s"done input handler ${Thread.currentThread().getId}")

  }

  def getRandomUsersIds(n: Int): Seq[String] =
    Stream.continually(randomUUID()).map(_.toString).take(n).toArray.toSeq

  def getRandomUsers(ids: Seq[String]): Seq[User] =
    Stream.continually(ids).map(User.getRandom)
}
