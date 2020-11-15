package com.github.arzt.math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Sudoku2Test extends AnyFlatSpec with Matchers {
  "A sudoku" should "have valid rows" in {
    val sudoku = new Sudoku2(2, 2)
    val c0 = sudoku.hasValidRow(0)
    c0(Array(1, 2, 3, 3))(3) mustBe true
    c0(Array(1, 2, 3, 3))(4) mustBe false
    c0(Array(1, 2, 3, 4))(4) mustBe true
    val c1 = sudoku.hasValidRow(1)
    c1(Array(1))(1) mustBe true
    sudoku.hasValidRows(Array(1, 2, 3, 4, 4, 3, 2, 1))(8) mustBe true
    sudoku.hasValidRows(Array(1, 2, 3, 4, 4, 3, 2, 3))(8) mustBe false
  }
  it should "have valid cols" in {
    val s = new Sudoku2(2, 2)
    val c0 = s.hasValidCol(0)
    c0(Array[Int]())(0) mustBe true
    c0(Array(1, 2, 3, 4))(4) mustBe true
    c0(Array(1, 2, 3, 4, 1))(5) mustBe false
    c0(Array(1, 2, 3, 4, 2))(5) mustBe true
    c0(Array(1, 2, 3, 4, 2, 1, 4, 3))(8) mustBe true
  }
  it should "have valid box" in {
    val s = new Sudoku2(2, 2)
    s.hasValidBox(1)(Array(
      0,0,3,4,
      0,0,3,4
    ))(8) mustBe false
    s.hasValidBox(1)(Array(
      1,2,3,4,
      1,2,1,2
    ))(8) mustBe true
    s.hasValidBox(2)(Array(
      0,0,0,0,
      0,0,0,0,
      1,3,0,0,
      2,4,0,0,
    ))(8) mustBe true
    val c = s.hasValidBox(0)
    c(Array(1, 2, 3, 4))(2) mustBe true
    c(Array(2, 2, 3, 4))(2) mustBe false
    c(Array(2, 1, 3, 4, 4, 3))(6) mustBe true
    c(Array(2, 1, 3, 4, 4, 1))(6) mustBe false
    c(Array(2, 1, 3, 4, 4, 1))(5) mustBe true
    val s3 = new Sudoku2(3, 3)
    s3.hasValidBox(0)(Array(1, 2, 3, 4, 5, 6, 7, 8, 9))(9) mustBe true
    new Sudoku2(3,2).hasValidBox(1)(Array(
      0,0,0,4,5,6,
      0,0,0,7,8,4
    ))(12) mustBe false
    new Sudoku2(3,2).hasValidBox(1)(Array(
      0,0,0,4,5,6,
      0,0,0,7,8,9
    ))(12) mustBe true
  }
  it should "have valid boxes" in {
    val data = Array(
      1, 2, 3, 4,
      3, 4, 2, 1,
      2, 3, 1, 2,
      1, 4
    )
    new Sudoku2(2, 2).hasValidBoxes(data)(14) mustBe true
  }
  it should "hold value constraints" in {
    new Sudoku2(2, 2).hasValue(2, 5)(Array(1,5,6))(3) mustBe false
    new Sudoku2(2, 2).hasValue(2, 5)(Array(1,5,5))(3) mustBe true
    new Sudoku2(2, 2).hasValue(1, 1, 8)(Array(1, 5, 5, 1, 2, 8))(3) mustBe true
    new Sudoku2(2, 2).hasValue(3, 5)(Array(1, 2, 3))(3) mustBe true
  }
  "An empty 2x2 Sudoku" should "should be initialized" in {
    val sudoku = new Sudoku2(2, 2)
    val s = sudoku.initArray()
    s.toSeq mustBe new Array[Int](4 * 4 + 1).toSeq
  }
  "next candidate" should "yield next candidate" in {
    val s = new Sudoku2(2 , 2)
    val a = s.initArray()
    1 mustBe 1
  }
}
