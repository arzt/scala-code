package com.github.arzt.math
import scala.annotation.tailrec
import com.github.arzt.scala.collection.IteratorExtension

import scala.collection.Iterator.range

object Sudoku {


  def index(width: Int): Int => Int => Int =
    x => {
      y => {
        y * width + x
      }
    }

  def main(args: Array[String]): Unit = {
    val b = Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0): Seq[Int]
    Iterator
      .iterate(b)(increment(90))
      .everyK(10000000)
      //.sample(0.0000001)
      .foreach(x => println(x.mkString(", ")))
    println("tst")
  }

  def incrementArray(max: Int): Array[Int]  => Array[Int] =
    a => {
      incrementArray(max, a.length - 1, a)
    }

  def increment(max: Int): Seq[Int] => Seq[Int] =
    a => {

      increment(max, a.length - 1, a)

    }

  @tailrec
  def increment(max: Int, i: Int, a: Seq[Int]): Seq[Int] =
      if (a(i) < max) {
        a.updated(i, a(i) + 1)
      } else {
        increment(max, i - 1, a.updated(i, 0))
      }

  @tailrec
  def incrementArray(max: Int, i: Int, a: Array[Int]): Array[Int] =
      if (a(i) < max) {
        a(i) = a(i) + 1
        a
      } else {
        a(i) =  0
        incrementArray(max, i - 1, a)
      }

  def hasNoDuplicate(values: Iterable[Int]): Boolean = {
    val init = (0, Set.empty[Int])
    values
      .scanLeft(init) {
        case ((_, set), a) => {
          (a, set + a)
        }
      }
      .map {
        case (v, set) => !set.contains(v)
      }
      .forall(identity)
  }

  def isValidSudoku(m: Int, n: Int, values: Seq[Int]): Boolean =
    isValidSudoku(m, n, values, m * n * m * n)

  def isValidRow(m: Int, n: Int, values: Seq[Int], i: Int, rowi: Int): Boolean = {
    val start = (m * n) * rowi
    start
      .until(start + (m * n))
      .view
      .map(values.apply)
      .iterator
      .hasNoDuplicate
  }

  def isValidCol(m: Int, n: Int, values: Seq[Int], i: Int, coli: Int): Boolean = {
    coli
      .until(coli * (m * n))
      .by(m * n)
      .view
      .map(values.apply)
      .iterator
      .hasNoDuplicate
  }

  def isValidBlock(m: Int, n: Int, values: Seq[Int], i: Int, blockI: Int, blockJ: Int): Boolean = {
    val rowTmp = 0.until(n)
    val yy = 0
      .until(m)
      .iterator
      .flatMap{ j =>
        rowTmp
          .map(_ + (m*n)*j)
      }
      .toArray
      .toSeq

    yy
      .iterator
      .hasNoDuplicate
  }

  def hasValidRows(m: Int, n: Int, values: Seq[Int], i: Int): Boolean = {
    range(0, m*n)
      .map{ row =>
        isValidRow(m, n, values, i, row)
      }
      .forall(identity)
  }

  def hasValidCols(m: Int, n: Int, values: Seq[Int], i: Int): Boolean = {
    range(0, m * n)
      .map { col =>
        isValidCol(m, n, values, i, col)
      }
      .forall(identity)
  }

  def hasValidBlocks(m: Int, n: Int, values: Seq[Int], i: Int): Boolean = {
    isValidBlock(m, n, values, i, 0, 0)
  }

  def isValidSudoku(m: Int, n: Int, values: Seq[Int], i: Int): Boolean = {
    hasValidRows(m, n, values, i) &&
    hasValidCols(m, n, values, i) &&
    hasValidBlocks(m, n, values, i)
  }


}
