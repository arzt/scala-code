package com.github.arzt.math

import java.lang.System.currentTimeMillis

object FindSudokus {
  def main(args: Array[String]): Unit = {
    println("test")
    val start = currentTimeMillis()
    val s = new Sudoku(3, 3)
    var countAll = 0L
    var countSudoku = 0L
    var su = "123456789"
    while (su.startsWith("123456789")) {
      //s.printSudoku(a)
      while (su.length < s.cellCount || !s.isValid(su)) {
        su = s.nextCandidate(s.isValid, su)
        countAll += 1
      }
      countSudoku += 1
      if (countSudoku % 10000 == 0) {
        val diff = currentTimeMillis() - start
        val ratio = 1.0 * countSudoku / countAll
        println(f"$ratio ($countSudoku/$countAll) sudoku/ms: ${countSudoku*1.0/diff}")
        println(ratio)
        println(su)
      }
      su = s.nextCandidate(s.isValid, su)
    }
    println(f"Sudokus: $countSudoku")
  }
}
