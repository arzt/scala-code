package com.github.arzt.math

import com.github.arzt.math.Sudoku.{
  Constraint,
  ConstraintOps,
  ConstraintStr,
  ConstraintStrOps
}
import com.github.arzt.scala.collection.IteratorExtension

import scala.collection.IndexedSeq
import scala.collection.Iterator.range

class Sudoku(w: Int, h: Int) {

  def satisfiesTemplate(s: String): Constraint =
    s.indices.map(i => hasValueAt(i, s(i) - '0')).reduce(_ && _)

  def valueCount: Int = w * h

  def cellCount: Int = valueCount * valueCount

  def values: Seq[Int] = 0.until(valueCount)

  def initArray(): Array[Int] = Array.fill[Int](cellCount)(1)

  def initArray(vals: Int*): Array[Int] = {
    val array = initArray()
    vals.copyToArray(array)
    array
  }

  def initArray(vals: String): Array[Int] = initArray(vals.map(_ - '0'): _*)

  private def toIndex(x: Int, y: Int): Int = y * valueCount + x

  private def toCol(i: Int): Int = i % valueCount

  private def toRow(i: Int): Int = i / valueCount

  def nextCandidate(x: Array[Int], i: Int, c: Constraint): Int = {
    if (c(x)(i) && i < cellCount) {
      x(i) = 1
      i + 1
    } else {
      val j = x.lastIndexWhere(_ < valueCount, i - 1)
      if (j >= 0) {
        x(j) = x(j) + 1
      }
      j + 1
    }
  }

  def nextCandidate(c: ConstraintStr, x: String): String = {
      val biggest = (valueCount + '0').toChar
      if (c(x) && x.length < cellCount) {
        x + "1"
      } else {
        val j = x.lastIndexWhere(_ < biggest)
        if (j > -1) {
          val ca = (x(j) + 1).toChar
          x.substring(0, j) + ca
        } else {
          x
        }
      }
    }

  def printSudoku(x: collection.Seq[Int]): Unit = {
    for (i <- range(0, valueCount)) {
      for (j <- range(0, valueCount)) {
        print(x(i * valueCount + j))
        print(" ")
        if (j % w == (w - 1)) print("  ")
      }
      println()
      if (i % h == (h - 1)) println()

    }
  }

  def getRow(i: Int): collection.Seq[Int] = {
    val col = toCol(i)
    val row = toRow(i)
    val start = row * valueCount
    start.until(start + col)
  }

  def getCol(i: Int): collection.Seq[Int] = {
    val col = toCol(i)
    col.until(i).by(valueCount)
  }

  def getOffset(i: Int): Int = {
    val row = toRow(i) / h * h
    val col = toCol(i) / w * w
    val offset = toIndex(col, row)
    offset
  }

  def getBox(i: Int): Iterable[Int] = {
    val offset = getOffset(i)
    val filtered = box.view.map(_ + offset).filter(_ < i)
    filtered
  }

  def hasValidRow: ConstraintStr =
    x => {
      val row = getRow(x.length - 1)
      val sliced = row.view.map(x.apply)
      val contains = sliced.contains(x.last)
      !contains
    }

  def hasValidCol: ConstraintStr =
    x => {
      val col = getCol(x.length - 1)
      val sliced = col.view.map(x.apply)
      val contains = sliced.contains(x.last)
      !contains
    }

  def hasValidBox: ConstraintStr =
    x => {
      val box = getBox(x.length - 1)
      val sliced = box.map(x.apply)
      val contains = sliced.exists(_ == x.last)
      !contains
    }

  def isValid: ConstraintStr = hasValidBox && hasValidCol && hasValidRow

  def hasValidRow(i: Int): Constraint =
    x =>
      j => {
        val start = i * valueCount
        val end = math.min(start + valueCount, j)
        val values = range(start, end).map(x.apply).toIndexedSeq
        val isValid = values.iterator.hasNoDuplicate
        isValid
      }

  def hasValidCol(i: Int): Constraint =
    x =>
      j => {
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

  val box: collection.Seq[Int] = (for {
    a <- Range(0, h);
    b <- Range(0, w)
  } yield valueCount * a + b).toArray

  def hasValidBox(i: Int): Constraint =
    x =>
      j => {
        val offset = i / h * h * valueCount + i % h * w
        val unsafeIndexes = box.view.map(_ + offset)
        val indexes = unsafeIndexes.filter(_ < j)
        val value = indexes.map(x.apply)
        val result = value.iterator.hasNoDuplicate
        result
      }

  def hasValidRows: Constraint = values.map(hasValidRow).reduce(_ && _)

  def hasValidColumns: Constraint = values.map(hasValidCol).reduce(_ && _)

  def hasValidBoxes: Constraint = values.map(hasValidBox).reduce(_ && _)

  def hasValueAt(i: Int, value: Int): Constraint =
    x => j => i >= j || x(i) == value

  def hasValue(x: Int, y: Int, value: Int): Constraint =
    hasValueAt(toIndex(x, y), value)

  def isValidSudoku: Constraint =
    hasValidRows && hasValidBoxes && hasValidColumns

}

object Sudoku {
  type Constraint = IndexedSeq[Int] => Int => Boolean

  type ConstraintStr = String => Boolean

  implicit class StringOpsSudoku(v: String) {
    def toInts: Array[Int] = v.map(_ - '0').toArray
  }

  implicit class ConstraintOps(a: Constraint) {
    def &&(b: Constraint): Constraint = x => i => a(x)(i) && b(x)(i)
  }

  implicit class ConstraintStrOps(a: ConstraintStr) {
    def &&(b: ConstraintStr): ConstraintStr = x => a(x) && b(x)
  }

}
