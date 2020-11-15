package com.github.arzt.math


import com.github.arzt.math.Sudoku2.{Constraint, combine}
import com.github.arzt.scala.collection.IteratorExtension

import scala.annotation.tailrec
import scala.collection.Iterator.range
import scala.collection.IndexedSeq


class Sudoku2(w: Int, h: Int) {

  def valueCount: Int = w * h

  def cellCount: Int = valueCount * valueCount

  def values: Seq[Int] = 0.until(valueCount)

  def initArray(): Array[Int] = {
    new Array[Int](cellCount + 1)
  }

  def initArray(vals: Int*): Array[Int] = {
    val array = initArray()
    vals.copyToArray(array)
    array
  }

  def setLast(x: Array[Int], value: Int): Array[Int] = {
    x(x.length - 1) = value
    x
  }

  private def toIndex(x: Int, y: Int): Int = y * valueCount + x

  @tailrec
  final def dropLast(x: Array[Int], value: Int): Array[Int] = {
    if (x(x.last) == value) {
      x(x.length - 1) = x.last - 1
      dropLast(x, value)
    } else {
      x
    }
  }

  def nextBiggerCandidate(x: Array[Int], c: Constraint): Array[Int] = {
    if (c(x)(x.last) && x.last < cellCount) {
      setLast(x, x.last + 1)
      x(x.last) = 1
      x
    } else if (x(x.last) < valueCount) {
      x(x.last) = x(x.last) + 1
      x
    } else {
      dropLast(x, valueCount)
      x(x.last) = x(x.last) + 1
      x
    }
  }

  def nextSmallerCandidate(x: Array[Int], c: Constraint): Array[Int] = {
    if (c(x)(x.last) && x.last < cellCount) {
      setLast(x, x.last + 1)
      x(x.last) = 9
      x
    } else if (x(x.last) > 1) {
      x(x.last) = x(x.last) - 1
      x
    } else {
      dropLast(x, 1)
      x(x.last) = x(x.last) - 1
      x
    }
  }

  def printSudoku(x: Seq[Int]): Unit = {
    for (i <- range(0, valueCount)) {
      for (j <- range(0, valueCount)) {
        print(x(i*valueCount + j))
        print(" ")
        if (j % w == (w - 1)) print("  ")
      }
      println()
      if (i % h == (h - 1)) println()

    }
  }

  def applySafe(x: IndexedSeq[Int])(i: Int): Int = math.abs(x.applyOrElse(i, (y: Int) => 0))

  def hasValidRow(i: Int): Constraint =
    x => j => {
      val start = i * valueCount
      if (start > j) {
        true
      } else {
        val end = math.min(start + valueCount, j)
        val values = range(start, end).map(x.apply).toIndexedSeq
        val isValid = values.iterator.hasNoDuplicate
        isValid
      }
    }

  def hasValidRows: Constraint = combine(values.map(hasValidRow): _*)

  def hasValidCol(i: Int): Constraint =
    x => j => {
      val start = i
      if (start >= j) {
        true
      } else {
        val unsaveEnd = start + (valueCount - 1) * valueCount + 1
        val end = math.min(unsaveEnd, j)
        val indices = Range(start, end, valueCount)
        val values = indices.map(x.apply)
        val result = values.iterator.hasNoDuplicate
        result
      }
    }

  def hasValidColumns: Constraint = combine(values.map(hasValidCol): _*)

  def hasValidBox(i: Int): Constraint =
    x => j => {
      val offset = i/h * h * valueCount + i%h * w
      val unsafeIndexes = for {
        a <- Range(0, h);
        b <- Range(0, w)
      } yield valueCount * a + b + offset
      val indexes = unsafeIndexes.filter(_ < j)
      val value = indexes.map(x.apply)
      val result = value.iterator.hasNoDuplicate
      result
    }

  def hasValidBoxes: Constraint = {
    val boxes = 0.until(valueCount)
    val res = combine(boxes.map(hasValidBox): _*)
    res
  }

  def hasValue(i: Int, value: Int): Constraint =
    x => j => {
      val res = i >= j || x(i) == value
      res
    }

  def hasValue(x: Int, y: Int, value: Int): Constraint = hasValue(toIndex(x, y), value)

  //def isValidSudoku: Constraint = combine(hasValidRows, hasValidBoxes, has)

}

object Sudoku2 {
  type Constraint = IndexedSeq[Int] => Int => Boolean

  implicit class ConstraintOps(c: Constraint) {
    def &&(other: Constraint): Constraint = x => i => c(x)(i) && other(x)(i)
  }

  def combine(cs: Constraint*): Constraint = cs.reduce(_ && _)

}