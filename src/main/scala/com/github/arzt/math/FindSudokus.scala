package com.github.arzt.math

import scala.util.Random


object FindSudokus {

  val diabolical01 =
    "_7____83_" +
    "9_2___5__" +
    "_____1___" +
    "____8 ___" +
    "2_______8" +
    "___13____" +
    "___6_____" +
    "__7___6_4" +
    "_14__5_92"

  val minimized =
    "_2_1_____" +
    "___3_71__" +
    "___6_9__2" +
    "_9_4__2__" +
    "4___1_6_9" +
    "__8_____5" +
    "7___3____" +
    "_547_6__3" +
    "2_____5_7"

  val solved =
    "8___5_39_" +
    "___32_14_" +
    "3__6___5_" +
    "59__68___" +
    "__2_1____" +
    "___9__4__" +
    "786______" +
    "154______" +
    "_________"
  def main(args: Array[String]): Unit = {
    println("test")
    val s = new Sudoku(3, 3)
    print(s.toString(s.randomFilledSudoku()))
    println(s.toString(s.randomFilledSudoku()))
    println(s.toString(s.randomFilledSudoku()))
    println(s.toString(s.randomFilledSudoku()))
    println(s.toString(s.randomFilledSudoku()))
  }
}
