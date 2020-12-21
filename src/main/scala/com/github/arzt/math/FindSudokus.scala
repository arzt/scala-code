package com.github.arzt.math

object FindSudokus {
  def main(args: Array[String]): Unit = {
    println("test")
    val s = new Sudoku(3, 3)
    val a = s.initArray()
    var i = 1
    var countAll = 0L
    var countSudoku = 0L
    while (i > 0) {
      //s.printSudoku(a)
      i = s.nextCandidate(a, i, s.isValidSudoku)
      if (s.isValidSudoku(a)(s.cellCount)) {
        countSudoku += 1
        //println(countSudoku)
        if (countSudoku % 10000 == 0) {
          val ratio = 1.0*countSudoku/countAll
          println(f"$ratio ($countSudoku/$countAll) rate: ${}")
          println(a.mkString(""))
        }
      }
      countAll += 1
    }
  }
}
