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
    
  "A sudoku" should "have valid rows" in {
    val sudoku = new Sudoku(2, 2)
    val c0 = sudoku.hasValidRow(0)
    c0("1233".toInts)(3) mustBe true
    c0("1233".toInts)(4) mustBe false
    c0("1234".toInts)(4) mustBe true
    val c1 = sudoku.hasValidRow(1)
    c1(Array(1))(1) mustBe true
    sudoku.hasValidRows("12344321".toInts)(8) mustBe true
    sudoku.hasValidRows("12344323".toInts)(8) mustBe false
  }
  it should "init array by string" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray("4321")
    a.length == s.cellCount
    a.slice(0, 4) mustBe Seq(4, 3, 2, 1)
    a.slice(4, a.length) mustBe Seq.fill[Int](a.length - 4)(1)
  }
  it should "have valid cols" in {
    val s = new Sudoku(2, 2)
    val c0 = s.hasValidCol(0)
    c0(Array[Int]())(0) mustBe true
    c0("1234".toInts)(4) mustBe true
    c0("12341".toInts)(5) mustBe false
    c0("12342".toInts)(5) mustBe true
    c0("12342143".toInts)(8) mustBe true
  }
  it should "have valid box" in {
    val s = new Sudoku(2, 2)
    s.hasValidBox(1)("00340034".toInts)(8) mustBe false
    s.hasValidBox(1)("12341212".toInts)(8) mustBe true
    s.hasValidBox(2)("0000000013002400".toInts)(8) mustBe true
    val c = s.hasValidBox(0)
    c("1234".toInts)(2) mustBe true
    c(Array(2, 2, 3, 4))(2) mustBe false
    c(Array(2, 1, 3, 4, 4, 3))(6) mustBe true
    c(Array(2, 1, 3, 4, 4, 1))(6) mustBe false
    c(Array(2, 1, 3, 4, 4, 1))(5) mustBe true
    val s3 = new Sudoku(3, 3)
    s3.hasValidBox(0)("123456789".toInts)(9) mustBe true
    new Sudoku(3, 2).hasValidBox(1)("000456000784".toInts)(12) mustBe false
    new Sudoku(3, 2).hasValidBox(1)("000456000789".toInts)(12) mustBe true
  }
  it should "have valid boxes" in {
    new Sudoku(2, 2).hasValidBoxes("12343421231214".toInts)(14) mustBe true
  }
  it should "hold value constraints" in {
    new Sudoku(2, 2).hasValueAt(2, 5)(Array(1, 5, 6))(3) mustBe false
    new Sudoku(2, 2).hasValueAt(2, 5)(Array(1, 5, 5))(3) mustBe true
    new Sudoku(2, 2).hasValue(1, 1, 8)(Array(1, 5, 5, 1, 2, 8))(3) mustBe true
    new Sudoku(2, 2).hasValueAt(3, 5)(Array(1, 2, 3))(3) mustBe true
  }
  it should "hold value constrain from string" in {
    new Sudoku(2, 2).satisfiesTemplate("1")(Array(1))(1) mustBe true
    new Sudoku(2, 2).satisfiesTemplate("1")(Array(2))(1) mustBe false
    new Sudoku(2, 2).satisfiesTemplate("2")(Array(2))(1) mustBe true
    new Sudoku(2, 2).satisfiesTemplate("2134")(Array(2, 1, 3, 3))(3) mustBe true
    new Sudoku(2, 2).satisfiesTemplate("2134")(Array(2, 1, 3, 3))(4) mustBe false
  }
  "An empty 2x2 Sudoku" should "should be initialized" in {
    val sudoku = new Sudoku(2, 2)
    val s = sudoku.initArray()
    s.toSeq mustBe Array.fill[Int](4 * 4)(1).toSeq
  }
  "next bigger candidate" should "yield next candidate" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray(1, 2, 3)
    s.nextCandidate(a, 3, s.hasValidRows) mustBe 4
  }
  "next candidate" should "yield next candidate when valid candidate" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray()
    val i = s.nextCandidate(a, 0, s.isValidSudoku)
    a(0) mustBe 1
    i mustBe 1
  }
  it should "yield next candidate when valid candidate 2" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray()
    a(0) = 1
    val i = s.nextCandidate(a, 1, s.isValidSudoku)
    a(0) mustBe 1
    a(1) mustBe 1
    i mustBe 2
  }
  it should "yield next candidate when valid candidate 3" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray()
    a(0) = 1
    a(1) = 1
    val i = s.nextCandidate(a, 2, s.isValidSudoku)
    a(0) mustBe 1
    a(1) mustBe 2
    i mustBe 2
  }
  it should "yield next candidate when valid candidate 4" in {
    val s = new Sudoku(2, 2)
    val a = s.initArray()
    a(0) = 1
    a(1) = 1
    val i = s.nextCandidate(a, 2, s.isValidSudoku)
    a(0) mustBe 1
    a(1) mustBe 2
    i mustBe 2
  }
  it should "yield next candidate when valid candidate 5" in {
    val s = new Sudoku(2, 2)
    val a = "1234341221434324".toInts
    var i = 16
    s.printSudoku(a)
    i = s.nextCandidate(a, i, s.isValidSudoku)
    s.printSudoku(a)
    a(14) mustBe 3
    i mustBe 15
  }
  it should "yield next candidate when valid candidate 6" in {
    val s = new Sudoku(2, 2)
    val a = "1234341221434340".toInts
    var i = 15
    s.printSudoku(a)
    i = s.nextCandidate(a, i, s.isValidSudoku)
    s.printSudoku(a)
    a(13) mustBe 4
    i mustBe 14
  }
  it should "yield next candidate when valid candidate 7" in {
    val s = new Sudoku(2, 2)
    val a = "1234341221434400".toInts
    var i = 14
    s.printSudoku(a)
    i = s.nextCandidate(a, i, s.isValidSudoku)
    s.printSudoku(a)
    a(11) mustBe 4
    i mustBe 12
  }
  "prefix version" should "have valid row" in {
    new Sudoku(2, 2).hasValidRow("1234121") mustBe false
    new Sudoku(2, 2).hasValidRow("1232") mustBe false
    new Sudoku(2, 2).hasValidRow("1234") mustBe true
    new Sudoku(2, 2).hasValidRow("12") mustBe true
    new Sudoku(2, 2).hasValidRow("11") mustBe false
    new Sudoku(2, 2).hasValidRow("123412") mustBe true
  }
  it should "have valid col" in {
    new Sudoku(2, 2).hasValidCol("12341") mustBe false
    new Sudoku(2, 2).hasValidCol("1234") mustBe true
    new Sudoku(2, 2).hasValidCol("12342") mustBe true
    new Sudoku(2, 2).hasValidCol("12342341") mustBe true
    new Sudoku(2, 2).hasValidCol("12342344") mustBe false
    new Sudoku(2, 2).hasValidCol("123423412") mustBe false
    new Sudoku(2, 2).hasValidCol("123423414") mustBe true
  }
  it should "have valid box" in {
    new Sudoku(2, 2).hasValidBox("12341") mustBe false
    new Sudoku(2, 2).hasValidBox("12343") mustBe true
    new Sudoku(2, 2).hasValidBox("123434") mustBe true
    new Sudoku(2, 2).hasValidBox("123432") mustBe false
    new Sudoku(2, 2).hasValidBox("12343212") mustBe true
    new Sudoku(2, 2).hasValidBox("123432122") mustBe true
    new Sudoku(2, 2).hasValidBox("1234321222") mustBe false
    new Sudoku(2, 2).hasValidBox("1234321221") mustBe true
  }
  "next candidate string" should "yield next candidate" in {
    val s = new Sudoku(2, 2)
    val nextCandidate = (x: String) => s.nextCandidate(s.isValid, x)
    nextCandidate("121") mustBe "122"
    nextCandidate("1") mustBe "11"
    nextCandidate("11") mustBe "12"
    nextCandidate("12") mustBe "121"
    nextCandidate("122") mustBe "123"
    nextCandidate("123") mustBe "1231"
  }
}
