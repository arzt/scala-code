package com.github.arzt.math

trait Dim extends Seq[Int] {

  def n: Int

  def sub: Seq[Int]

  def length: Int

  def apply(i: Int): Int

  override def iterator: Iterator[Int] = Iterator.range(0, length).map(x => apply(x))
}

case class UnitDim(n: Int, sub: Seq[Int]) extends Dim {

  val length: Int = sub.length

  def apply(i: Int): Int = sub.map(_ % n).apply(i)

}

case class Comp(a: Int, sub: Seq[Int], child: Dim) extends Dim {

  val n = a * child.n

  val length: Int = child.length * sub.length

  def apply(i: Int): Int = {
    sub(i / child.length) * child.n + child(i % child.length)
  }

}