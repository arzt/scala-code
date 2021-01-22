package com.github.arzt.math

import com.github.arzt.math.Sudoku.ConstraintStr

class Sudoku(w: Int, h: Int) {

  val biggest = (valueCount + '0').toChar

  def matchesCell(x: String, temp: String): Boolean = {
    (x.length > temp.length) || {
      val template = temp.charAt(x.length - 1)
      template > biggest ||
        template < '1' ||
        template == x.charAt(x.length - 1)
    }
  }

  def matchesRow(x: String, temp: String): Boolean = {
    val value = x(x.length - 1)
    var i = x.length

    while (i % valueCount > 0 && i < temp.length && temp.charAt(i) != value)
      i += 1

    i == temp.length || i % valueCount == 0
  }

  def matchesCol(s: String, temp: String): Boolean = {
    var i = s.length - 1
    val value = s(i)
    i = i + valueCount

    while (i < temp.length && value != temp.charAt(i))
      i += valueCount

    i >= temp.length
  }

  def matchesBox(s: String, temp: String): Boolean = {
    temp.length < s.length || {
      val value = s.charAt(s.length - 1)
      val offset = boxOffset(s.length - 1)
      var iBox = boxIndex(s.length - 1) + 1
      var i = offset + inverseBoxIndex(iBox)
      while (iBox < valueCount && i < temp.length && temp.charAt(i) != value) {
        iBox += 1
        i = offset + inverseBoxIndex(iBox)
      }
      iBox == valueCount || i == temp.length || (i <= temp.length && temp.charAt(i) != value)
    }
  }

  def matchesTemplate(x: String, temp: String): Boolean =
    matchesCell(x, temp) &&
      matchesRow(x, temp) &&
      matchesCol(x, temp) &&
      matchesBox(x, temp)

  def valueCount: Int = w * h

  def cellCount: Int = valueCount * valueCount

  def toIndex(x: Int, y: Int): Int = y * valueCount + x

  def toCol(i: Int): Int = i % valueCount

  def toRow(i: Int): Int = i / valueCount

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
    val rowBox = i / h
    val index = toIndex(colBox, rowBox)
    index
  }

  def boxIndex(i: Int): Int = {
    val col = i % valueCount
    val row = i / valueCount
    val boxCol = col % w
    val boxRow = row % h
    val result = boxRow * h + boxCol
    result
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
    val offset = boxOffset(x.length - 1)
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
