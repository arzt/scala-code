package com.github.arzt.newexcercise

import java.util.Base64

import scala.collection.Iterator.continually
import scala.util.Random
case class User(id: String, secret: String, price: Float, amount: Int, data: Int) {

  override def toString: String = s"$id,$secret,$price,$amount,$data"
}

object User {

  def fromString(s: String): User = {
    val Seq(id,secret, priceStr, amountStr, dataStr, _*) = s.split(",").toSeq
    User(
      id,
      secret,
      priceStr.toFloat,
      amountStr.toInt,
      dataStr.toInt
    )
  }

  def getRandom(ids: Seq[String]): User = {
    val i = Random.nextInt(ids.length)
    val user = ids(i)
    val string = randomString(Random, 10)
    val stringBase64 = new String(Base64.getEncoder.encode(string.getBytes()))
    val fpValue = Random.nextFloat()
    val integerOne = Random.nextInt().abs
    val integerTwo = Random.nextInt().abs
    User(user, stringBase64, fpValue, integerOne, integerTwo)
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
}