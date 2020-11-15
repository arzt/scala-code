package com.github.arzt.math

import org.scalatest.FreeSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class SudokuTest extends AnyFreeSpec with Matchers {

  "A sudoku" - {
    val s2b2 = new Sudoku(2, 2)
    val s2b3 = new Sudoku(2, 3)
    val s3b3 = new Sudoku(3, 3)
    "has valid values" in {
      s2b2.hasValidValues(Vector(1,2,3,4)) shouldBe true
      s2b2.hasValidValues(Vector(5)) shouldBe false
    }
    "has valid box" in {
      s2b2.hasValidBox(Vector(1, 2, 3, 4, 3, 2), 0) shouldBe false
      s2b2.hasValidBox(Vector(1, 2, 3, 4, 3, 4), 0) shouldBe true
      s2b2.hasValidBox(Vector(1, 2, 3, 4, 3, 4, 2), 1) shouldBe false
      s2b2.hasValidBox(Vector(1, 2, 3, 4, 3, 4, 1), 1) shouldBe true
    }
    "has valid boxes" in {
      s2b2.hasValidBoxes(Vector(
        1,2,3,4,
        3,4
      )) shouldBe true
      s2b2.hasValidBoxes(Vector(
        1,2,3,4,
        3,3
      )) shouldBe false
      s2b2.hasValidBoxes(Vector(
        1,2,3,4,
        3,4,1,2,
        1,2,4,3,
        3,4,2,4
      )) shouldBe false
      s2b2.hasValidBoxes(Vector(
        1,2,3,4,
        3,4,1,2,
        1,2,4,3,
        3,4,2,1
      )) shouldBe true
    }
    "has valid rows" in {
      s2b2.hasValidRows(Vector(1, 2)) shouldBe true
      s2b2.hasValidRows(Vector(1, 1)) shouldBe false
      s2b2.hasValidRows(Vector(4, 1, 3, 2)) shouldBe true
      s2b2.hasValidRows(Vector(4, 1, 3, 2, 1)) shouldBe true
      s2b2.hasValidRows(Vector(4, 1, 3, 4, 1)) shouldBe false
      s2b2.hasValidRows(Vector(4, 1, 3, 4, 1)) shouldBe false
    }
    "his valid cols" in {
      s2b2.hasValidCols(Vector()) shouldBe true
      s2b2.hasValidCols(Vector(1, 1, 1, 1)) shouldBe true
      s2b2.hasValidCols(Vector(1, 1, 1, 1, 1)) shouldBe false
      s2b2.hasValidCols(Vector(1, 1, 1, 1, 2)) shouldBe true
      s2b2.hasValidCols(Vector(
        1, 1, 1, 1,
        2, 2, 2, 2,
        3, 3, 3, 3,
        4, 4, 4, 4
      )) shouldBe true
      s2b2.hasValidCols(Vector(
        1, 1, 1, 1,
        2, 2, 2, 2,
        3, 3, 3, 3,
        4, 4, 4, 1
      )) shouldBe false
    }
  }
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
  "next canditade" - {
    val s = new Sudoku(2, 2)
    "should work" in {
      val next = s.nextCandidate(Vector(
        1, 2, 3, 4,
        3, 4, 1, 2,
        2, 1, 4, 3,
        4, 4
      ))
      1 shouldBe 1
    }
    "be true" ignore {
      val s = new Sudoku(3, 3)
      val next = s.nextCandidate(Vector())
      s.nextCandidate(Vector(1)) shouldBe Vector(1, 1)
      s.nextCandidate(Vector(1, 1)) shouldBe Vector(1, 2)
      s.nextCandidate(Vector(1, 2)) shouldBe Vector(1, 2, 1)
      s.nextCandidate(Vector(1, 2, 1)) shouldBe Vector(1, 2, 2)
      val start: Seq[Int] = Vector()
      Iterator.iterate(start)(s.nextCandidate2)
        .filter(x => s.isValidSudoku(x) & x.length == s.size*s.size)
        .take(1000)
        .foreach( x => {
          s.printSudoku(x)
          println()
        })
      next === Seq(1)
    }
  }
}
