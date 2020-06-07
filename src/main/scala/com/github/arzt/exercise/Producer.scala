package com.github.arzt.exercise

import java.io.OutputStreamWriter
import java.net.Socket
import java.util.Base64
import java.util.UUID
import java.util.UUID.randomUUID

import scala.collection.Iterator.continually
import scala.util.Failure
import scala.util.Random
import scala.util.Try

object Producer {

  val Random = new Random(12345L)

  def main(args: Array[String]): Unit = {
    val nUser = 1000
    val users =
      continually(randomUUID())
        .map(_.toString)
        .take(nUser)
        .toArray
        .toSeq
    val s = new Socket("localhost", 9000)
    s.getChannel
    val writer = new OutputStreamWriter(s.getOutputStream)

    def write(s: String): Unit = {
      Try {
        writer.write(s)
        writer.flush()
      }.recoverWith {
        case t =>
          println(s)
          Failure(t)
      }.get
    }

    val printer = qpsPrinter[String]

    printer(producerIterator(users))
      .foreach(write)
  }

  def randomString(rnd: Random, length: Int): String = {
    val a = 'a'.toInt
    val z = 'z'.toInt
    def hui = rnd.nextInt(z - a)
    val chars = continually('a' + hui)
      .map(_.toChar)
      .take(length)
      .toArray
    new String(chars)
  }

  def producerIterator(users: Seq[String]): Iterator[String] = {
    val nUser = users.length
    continually {
      val iUser = Random.nextInt(nUser)
      val user = users(iUser).toString
      val string = randomString(Random, 10)
      val stringBase64 = new String(Base64.getEncoder.encode(string.getBytes()))
      val fpValue = Random.nextFloat().toString
      val integerOne = Random.nextInt().abs
      val integerTwo = Random.nextInt().abs
      s"$user,$stringBase64,$fpValue,$integerOne,$integerTwo\n"
    }
  }

  def qpsPrinter[T]: Iterator[T] => Iterator[T] = {
    var count = 0L
    val start = System.currentTimeMillis()
    x => {
      x.map { el =>
          count += 1
          if (count % 100000 == 0) {
            val diff = (System.currentTimeMillis() - start)
            val seconds = (System.currentTimeMillis() - start) / 1000.0
            val qps = count / seconds
            println(s"qps: $qps count $count seconds $seconds")
          }
          el
        }
    }
  }
}
