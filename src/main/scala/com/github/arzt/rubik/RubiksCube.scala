package com.github.arzt.rubik
import Array.fill
import scala.collection.SeqView
import scala.collection.immutable.ArraySeq

  case class RubiksCube(n: Int, front: String, back: String, left: String, right: String, top: String, bottom: String):
    val toStringRow: PartialFunction[Int, String] =
      (r: Int) => r match
        case r if (r >= 0 && r < n) =>
          "_" * n + top.slice(r * n, r * n + n) + "_" * 2 * n
        case r if (r >= n && r < 2 * n) =>
          val row = r - n
          val leftRow = left.slice(row * n, row * n + n)
          val frontRow = front.slice(row * n, row * n + n)
          val rightRow = right.slice(row * n, row * n + n)
          val backRow = back.slice(row * n, row * n + n)
          leftRow + frontRow + rightRow + backRow
        case r if (r >= 2 * n && r < 3 * n) =>
          val row = r - 2 * n
          "_" * n + bottom.slice(row * n, row * n + n) + "_" * 2 * n

    override def toString: String = (0 until 3*n).map(toStringRow).mkString("\n")

    def getRow(i: Int): Int = i / n

    def getCol(i: Int): Int = i % n

    def rotateRow(i: Int): Int = n - getCol(i) - 1

    def rotateCol(i: Int): Int = getRow(i)

    def rotateIndex(i: Int): Int = n * rotateRow(i) + rotateCol(i)

    def rotateIndexTimes(times: Int): Int => Int =
      print(times)
      if (times == 1)
        rotateIndex
      else
        rotateIndexTimes(times - 1).andThen(rotateIndex)

    def rotateFront(nSlices: Int): RubiksCube =
      val rotated = rotateCubeFront()
      val newFront = ""
      val newBack = ""
      val newLeft = ""
      val newRight = ""
      val newTop = ""
      val newBottem = ""
      RubiksCube(n, newFront, newBack, newLeft, newRight, newTop, newBottem)

    def rotateCubeFront(): RubiksCube =
      val RubiksCube(n, front, back, left, right, top, bottom) = this
      val newFront = rotate(front.view).mkString
      val newBack = rotate(back.view, 3).mkString
      val newLeft = rotate(bottom.view).mkString
      val newRight = rotate(top.view).mkString
      val newTop =  rotate(left.view).mkString
      val newBottom = rotate(right.view).mkString
      RubiksCube(n, newFront, newBack, newLeft, newRight, newTop, newBottom)

    def rotate(face: SeqView[Char], times: Int = 1): SeqView[Char] = {
      val fullRotation = rotateIndexTimes(times)
      face.indices.view.map(fullRotation).map(face.apply)
    }


object RubiksCube:

  def replace(target: String, source: String, targetIndices: Seq[Int], soruceIndices: Seq[Int]): String =
    val output = target.toCharArray
    targetIndices.indices
      .foreach(
        i => {
          val aii = targetIndices(i)
          val bii = soruceIndices(i)
          output(aii) = source(bii)
        }
      )
    new String(output)


  def apply(n: Int): RubiksCube =
    if (n > 0)
      val nn = n * n
      val front = "o" * nn
      val back = "r" * nn
      val left = "g" * nn
      val right = "b" * nn
      val top = "y" * nn
      val bottom = "w" * nn
      RubiksCube(n, front, back, left, right, top, bottom)
    else
      throw new RuntimeException("test")
