package com.github.arzt.math

import java.lang.System.currentTimeMillis

object FindSudokus {

  val diabolical01 =
    "57_4__83_" +
    "9_2___5__" +
    "_____1___" +
    "____82___" +
    "2_______8" +
    "___13____" +
    "___6_____" +
    "__7___6_4" +
    "_14__5_92"

  def main(args: Array[String]): Unit = {
    println("test")
    val start = currentTimeMillis()
    val s = new Sudoku(3, 3)
    while (true) {
      val result = s.solve(diabolical01).toVector
      println(f"found ${result.length}")
    }

  }
}
