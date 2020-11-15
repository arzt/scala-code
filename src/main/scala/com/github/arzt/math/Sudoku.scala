package com.github.arzt.math
import com.github.arzt.math.Sudoku.{hasValidRows, isValidSudoku}

import scala.annotation.tailrec
import com.github.arzt.scala.collection.IteratorExtension

import scala.collection.Iterator.range

class Sudoku(w: Int, h: Int) {

  def hasValidValues(xs: Seq[Int]): Boolean = xs.forall(x => x > 0 && x <= size)

  def size: Int = w * h

  @tailrec
  final def dropLast(x: Seq[Int], value: Int): Seq[Int] = {
    if (x.last == value) {
      dropLast(x.dropRight(1), value)
    } else {
      x
    }
  }

  def nextCandidate(x: Seq[Int]): Seq[Int] = {
    if (isValidSudoku(x) && x.length < size*size) {
      //x.appended(1)
      x :+ 1
    } else if (x.last < size) {
      val last = x.last + 1
      val next = x.updated(x.length - 1, last)
      next
    } else {
      val dropped = dropLast(x, size)
      val last = dropped.last + 1
      val next = dropped.updated(dropped.length - 1, last)
      next
    }
  }

  def nextCandidate2(x: Seq[Int]): Seq[Int] = {
    if (isValidSudoku(x) && x.length < size*size) {
      x :+ 9
    } else if (x.last > 1) {
      val last = x.last - 1
      val next = x.updated(x.length - 1, last)
      next
    } else {
      val dropped = dropLast(x, 1)
      val last = dropped.last - 1
      val next = dropped.updated(dropped.length - 1, last)
      next
    }
  }

  def toStringSudoku(x: Seq[Int]): String = {
    x.sliding(size, size).map(_.mkString(" ")).mkString("\n")
  }

  def printSudoku(x: Seq[Int]): Unit = {
    for (i <- range(0, size)) {
      for (j <- range(0, size)) {
        print(x(i*size + j))
        print(" ")
        if (j % w == (w - 1)) print("  ")
      }
      println()
      if (i % h == (h - 1)) println()
      //if (i % h == 0) println()

    }
    val str = toStringSudoku(x)
    //println(str)
  }

  def hasValidBoxes(s: Seq[Int]): Boolean = {
    val boxes = (for (
      i <- range(0, size, h);
      j <- range(0, size, w)
    ) yield {
      i*size + j
    })
      .toIndexedSeq
    val validBoxes = boxes.map(x => hasValidBox(s, x))
    validBoxes.forall(identity)
  }

  def isValidSudoku(x: Seq[Int]): Boolean = {
    hasValidRows(x) && hasValidCols(x) && hasValidBoxes(x)
  }

  def hasValidCols(x: Seq[Int]): Boolean = hasValidCols(true, x, 0)

  def applySafe(x: Seq[Int])(i: Int): Int = math.abs(x.applyOrElse(i, (y: Int) => 0))

  def hasValidRow(x: Seq[Int])(i: Int): Boolean = {
    val start = i
    val end = start + size
    val values = range(start, end).map(applySafe(x)).withFilter(_ > 0).toIndexedSeq
    val isValid = values.iterator.hasNoDuplicate
    isValid
  }

  def hasValidCol(x: Seq[Int])(i: Int): Boolean = {
    val start = i
    val end = start * (size - 1) * size + 1
    val indices = range(start, end, size).toIndexedSeq
    val values = indices.map(applySafe(x))
    val result = values.iterator.hasNoDuplicate
    result
  }

  def hasValidRows(x: Seq[Int]): Boolean = {
    val starts = range(0, size * size, size).toIndexedSeq
    val validRows = starts.map(hasValidRow(x))
    val result = validRows.reduce(_ && _)
    result
  }

  @tailrec
  final def hasValidCols(acc: Boolean, x: Seq[Int], i: Int): Boolean = {
    val start = i
    if (!acc || start > size || start >= x.length) {
      acc
    } else {
      val end = math.min(start + (size-1)*size + 1, x.length)
      val isValid = range(start, end, size).map(x.apply).hasNoDuplicate
      hasValidCols(isValid, x, i + 1)
    }
  }

  final def hasValidBox(x: Seq[Int], i: Int): Boolean = {
    val indexes = (for (a <- range(0, h);
                        b <- range(0, w)) yield size * a + b + i).toSeq
    val value = indexes.withFilter(_ < x.length).map(x.apply)
    val result = value.iterator.hasNoDuplicate
    result
  }

}

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

  def incrementArray(max: Int): Array[Int] => Array[Int] =
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
      a(i) = 0
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

  def isValidRow(
      m: Int,
      n: Int,
      values: Seq[Int],
      i: Int,
      rowi: Int
  ): Boolean = {
    val start = (m * n) * rowi
    start
      .until(start + (m * n))
      .view
      .map(values.apply)
      .iterator
      .hasNoDuplicate
  }

  def isValidCol(
      m: Int,
      n: Int,
      values: Seq[Int],
      i: Int,
      coli: Int
  ): Boolean = {
    coli
      .until(coli * (m * n))
      .by(m * n)
      .view
      .map(values.apply)
      .iterator
      .hasNoDuplicate
  }

  def isValidBlock(
      m: Int,
      n: Int,
      values: Seq[Int],
      i: Int,
      blockI: Int,
      blockJ: Int
  ): Boolean = {
    val rowTmp = 0.until(n)
    val yy = 0
      .until(m)
      .iterator
      .flatMap { j =>
        rowTmp
          .map(_ + (m * n) * j)
      }
      .toArray
      .toSeq

    yy.iterator.hasNoDuplicate
  }

  def hasValidRows(m: Int, n: Int, values: Seq[Int], i: Int): Boolean = {
    range(0, m * n)
      .map { row =>
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
