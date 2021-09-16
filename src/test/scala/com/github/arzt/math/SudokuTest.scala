package com.github.arzt.math

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class SudokuTest extends AnyFreeSpec with Matchers {

  "A valid sudoku" - {
    "should be valid" in {
      val values = Vector(
        0, 1, 2, 3,
        2, 3, 0, 1,
        1, 2, 3, 0,
        3, 0, 1, 2
      )
      assert(Sudoku.isValidSudoku(2, 2, values))
    }
  }

}
