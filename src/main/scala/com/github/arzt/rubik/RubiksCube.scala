package com.github.arzt.rubik
import Array.fill

  case class RubiksCube(n: Int, front: String, back: String, left: String, right: String, top: String, bottom: String):
    def toStringRow(r: Int): String =
      if (r >= 0 && r < n)
        "_" * n + top.slice(r * n, r * n + n) + "_" * 2 * n
      else if (r >= n && r < 2 * n)
        val row = r - n
        val leftRow = left.slice(row * n, row * n + n)
        val frontRow = front.slice(row * n, row * n + n)
        val rightRow = right.slice(row * n, row * n + n)
        val backRow = back.slice(row * n, row * n + n)
        leftRow + frontRow + rightRow + backRow
      else if (r >= 2 * n && r < 3 * n)
        val row = r - 2 * n
        "_" * n + bottom.slice(row * n, row * n + n) + "_" * 2 * n
      else
        throw new IllegalArgumentException()

    override def toString: String =
      (0 until 3*n).map(toStringRow).mkString("\n")

    """
      |____yyyy________
      |____yyyy________
      |____yyyy________
      |____yyyy________
      |bbbbrrrrggggoooo
      |bbbbrrrrggggoooo
      |bbbbrrrrggggoooo
      |bbbbrrrrggggoooo
      |____wwww________
      |____wwww________
      |____wwww________
      |____wwww________
      |""".stripMargin

object RubiksCube:

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
