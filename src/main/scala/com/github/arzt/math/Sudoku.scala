package com.github.arzt.math

import com.github.arzt.math.Sudoku.ConstraintStr

import scala.collection.Iterator.range

class Sudoku(w: Int, h: Int) {

  def valueCount: Int = w * h

  def cellCount: Int = valueCount * valueCount

  private def toIndex(x: Int, y: Int): Int = y * valueCount + x

  private def toCol(i: Int): Int = i % valueCount

  private def toRow(i: Int): Int = i / valueCount

  def nextCandidate(c: ConstraintStr, x: String): String = {
    val biggest = (valueCount + '0').toChar
    if (c(x) && x.length < cellCount) {
      x + "1"
    } else {
      var j = x.length - 1
      while (x.charAt(j) == biggest) {
        j -= 1
      }
      if (j > -1) {
        val ca = (x.charAt(j) + 1).toChar
        x.substring(0, j) + ca
      } else {
        x
      }
    }
  }

  def getOffset(i: Int): Int = {
    val row = toRow(i) / h * h
    val col = toCol(i) / w * w
    val offset = toIndex(col, row)
    offset
  }

  def getBox(i: Int): collection.Seq[Int] = {
    val offset = getOffset(i)
    val filtered = box.map(_ + offset).filter(_ < i)
    filtered
  }

  def hasValidRow(x: String): Boolean = {
    val last = x.length - 1
    val col = toCol(last)
    val end = last
    val start = end - col
    var i = start
    while (i < end && x.charAt(i) != x.charAt(last)) {
      i = i + 1
    }
    i == end
  }

  def hasValidCol(x: String): Boolean = {
    val last = x.length - 1
    var i = last - valueCount
    while (i > -1 && x.charAt(i) != x.charAt(last)) {
      i -= valueCount
    }
    val valid = i < 0
    valid
  }

  def hasValidBox(x: String): Boolean = {
    val offset = getOffset(x.length - 1)
    var j = 0
    while (j < box.length && x.charAt(box(j) + offset) != x.last) {
      j += 1
    }
    val k = box(j) + offset
    val contains = k < x.length - 1 && x.charAt(k) == x.last
    !contains
  }

  def isValid(x: String): Boolean = hasValidRow(x) && hasValidCol(x) && hasValidBox(x)

  private val box: Array[Int] = (for {
    a <- Range(0, h);
    b <- Range(0, w)
  } yield valueCount * a + b).toArray

}

object Sudoku {

  type ConstraintStr = String => Boolean

  implicit class StringOpsSudoku(v: String) {
    def toInts: Array[Int] = v.map(_ - '0').toArray
  }

  implicit class ConstraintStrOps(a: ConstraintStr) {
    def &&(b: ConstraintStr): ConstraintStr = x => a(x) && b(x)
  }

}
