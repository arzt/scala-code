package com.github.arzt.math

import com.github.arzt.math.Sudoku.{Constraint, LazyConstraint}

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.WrappedString
import scala.util.Random
import Sudoku.&&

class Sudoku(w: Int, h: Int):

  def randomTemplate(): String =
    val output = Array.fill(cellCount)('_')
    for (j <- 0 until math.min(w, h)) {
      val rn = randomNumbers()
      val boxOffset = toIndex(w*j, h*j)
      for (i <- 0 until valueCount) {
        val ii = inverseBoxIndex(i)
        output(boxOffset + ii) = rn(i)
      }
    }
    output.mkString

  def randomFilledSudoku(n: Int = 1): String = {
    val str = randomTemplate()
    val solved = solveList(str)
    solved(Random.nextInt(n))
  }

  val biggest: Char = (valueCount + '0').toChar

  def matchesCell(x: String, temp: String): Boolean = {
    (x.length > temp.length) || {
      val template = temp(x.length - 1)
      template > biggest ||
        template < '1' ||
        template == x.last
    }
  }

  def matchesRow(x: String, temp: String): Boolean = {
    var i = x.length

    while (i % valueCount > 0 && i < temp.length && temp(i) != x.last)
      i += 1

    i == temp.length || i % valueCount == 0
  }

  def matchesCol(s: String, temp: String): Boolean = {
    var i = s.length - 1
    val value = s(i)
    i += valueCount

    while (i < temp.length && value != temp(i))
      i += valueCount

    i >= temp.length
  }

  def matchesBox(s: String, temp: String): Boolean = {
    temp.length < s.length || {
      val last = s.length - 1
      val value = s(last)
      val offset = boxOffset(last)
      var iBox = boxIndex(last)
      iBox += 1
      val i1 = inverseBoxIndex(iBox)
      var i = offset + i1
      while (iBox < valueCount && i < temp.length && temp(i) != value) {
        iBox += 1
        val i2 = inverseBoxIndex(iBox)
        i = offset + i2
      }
      iBox == valueCount || i == temp.length || (i <= temp.length && temp(i) != value)
    }
  }

  def matchesTemplate(x: String, temp: String): Boolean =
    matchesCell(x, temp) &&
      matchesRow(x, temp) &&
      matchesCol(x, temp) &&
      matchesBox(x, temp)

  def valueCount: Int = w * h

  def maxValue: Char = ('0' + valueCount).toChar

  def cellCount: Int = valueCount * valueCount

  def toIndex(x: Int, y: Int): Int = y * valueCount + x

  def toCol(i: Int): Int = i % valueCount

  def toRow(i: Int): Int = i / valueCount

  def nextCandidate(c: Constraint, x: String): String = {
    val biggest = (valueCount + '0').toChar
    if (c(x) && x.length < cellCount) {
      x + "1"
    } else {
      var j = x.length - 1
      while (j > 0 && x(j) == biggest) {
        j -= 1
      }
      val charJ = x(j)
      if (j > -1 && charJ < biggest) {
        val ca = (charJ + 1).toChar
        val r1 = x.substring(0, j) + ca
        r1
      } else {
        ""
      }
    }
  }

  def nextCandidate2(c: LazyConstraint)(x: String): String = nextCandidate2(c, x, '1')

  @tailrec
  final def nextCandidate2(c: LazyConstraint, x: String, test: Char): String = {
    if (test <= maxValue)
      if (c(x)(test))
        val result = x + test
        result
      else
        nextCandidate2(c, x, (test + 1).toChar)
    else
      if (test >= maxValue)
        val last = x.last
        nextCandidate2(c, x.take(x.length - 1), (last + 1).toChar)
      else
        nextCandidate2(c, x, (test + 1).toChar)
  }

  def nextCandidateCurr(c: Constraint): String => String =
  x => {
    nextCandidate(c, x)
  }

  def boxOffset(i: Int): Int = {
    val row = toRow(i)
    val col = toCol(i)
    val boxCol = col / w * w
    val boxRow = row / h * h
    val offset = toIndex(boxCol, boxRow)
    offset
  }

  def inverseBoxIndex(i: Int): Int = {
    val colBox = i % w
    val rowBox = i / w
    val index = toIndex(colBox, rowBox)
    index
  }

  def boxIndex(i: Int): Int = {
    val col = i % valueCount
    val row = i / valueCount
    val boxCol = col % w
    val boxRow = row % h
    val result = boxRow * w + boxCol
    result
  }

  val hasValidRow2: LazyConstraint = x => h => {
    val end = x.length
    val start = end - toCol(end)
    !x.view.slice(start, end).contains(h)
  }

  val hasValidCol2: LazyConstraint = x => h => {
    val end = x.length
    val start = toCol(end)
    !Range(start, end, valueCount).view.map(x.apply).contains(h)
  }

  val hasValidBox2: LazyConstraint = x => h => {
    val offset = boxOffset(x.length)
    var j = 0
    while (j < box.length && box(j) + offset < x.length && x(box(j) + offset) != h) {
      j += 1
    }
    val k = box(j) + offset
    k == x.length
  }

  def hasValidRow(x: String): Boolean = hasValidRow2(x.take(x.length - 1))(x.last)

  def hasValidCol(x: String): Boolean = hasValidCol2(x.take(x.length - 1))(x.last)

  def hasValidBox(x: String): Boolean = hasValidBox2(x.take(x.length - 1))(x.last)

  def isValid(x: String): Boolean = hasValidRow(x) && hasValidCol(x) && hasValidBox(x)

  def isValidLazy: LazyConstraint = hasValidBox2 && hasValidCol2 && hasValidRow2

  def isValidTemplate(template: String)(x: String): Boolean = isValid(x) && matchesTemplate(x, template)

  def allSudocus(start: String): LazyList[String] = LazyList.iterate(start)(nextCandidate2(isValidLazy))

  def allSudokus: LazyList[String] = LazyList.iterate("1")(nextCandidate2(isValidLazy))

  def iterateCandidates(sudoku: String): Iterator[String] = {
    val start = "1"
    var i = 0
    Iterator
      .iterate(start)(nextCandidateCurr(isValidTemplate(sudoku)))
      .map(
        x => {
          i += 1
          //if (i % 10000 == 0)
            //println(f"Steps: ${i}")
          x
        }
      )
      .takeWhile(_.nonEmpty)
  }

  def solve(sudoku: String): Iterator[String] = iterateCandidates(sudoku).filter(_.length == cellCount).filter(isValid)

  def solveList(sudoku: String): LazyList[String] = solve(sudoku).to(LazyList)

  private val box: Array[Int] = (for {
    a <- Range(0, h);
    b <- Range(0, w)
  } yield valueCount * a + b).toArray

  def rowToString(row: WrappedString): String = row.sliding(w, w).mkString("", " ", "\n")

  def rowsToString(rows: WrappedString): String = rows.sliding(valueCount, valueCount).map(rowToString).mkString("")

  def toString(x: String) = x.toIterable.sliding(valueCount*h, valueCount*h).map(rowsToString).mkString("", "\n", "\n")

  def randomNumbers(): String = Random.shuffle(('1' to biggest).toVector).mkString

  def transpose(x: String): String = {
    (for (i <- 0 until cellCount) yield {
      val col = toCol(i)
      val row = toRow(i)
      val o = toIndex(row, col)
      x(o)
    }).mkString
  }

  def flipHorizonal(x: String): String = {
    (for (i <- 0 until cellCount) yield {
      val col = toCol(i)
      val row = valueCount - toRow(i) - 1
      val o = toIndex(col, row)
      x(o)
    }).mkString
  }

  def flipVertical(x: String): String = {
    (for (i <- 0 until cellCount) yield {
      val col = valueCount - toCol(i) - 1
      val row = toRow(i)
      val o = toIndex(col, row)
      x(o)
    }).mkString
  }

  def isUnique(s: String): Boolean = solveList(s).tail.isEmpty

end Sudoku

object Sudoku:

  type Constraint = String => Boolean

  type LazyConstraint = String => Char => Boolean

  implicit class StringOpsSudoku(v: String) {
    def toInts: Array[Int] = v.map(_ - '0').toArray
  }

  implicit class ConstraintStrOps(a: Constraint) {
    def &&(b: Constraint): Constraint = x => a(x) && b(x)
  }

  extension (a: LazyConstraint)
    def &&(b: LazyConstraint): LazyConstraint = s => c => a(s)(c) && b(s)(c)

  def concat(x: String*): String = x.reduce(_ + _)

end Sudoku

