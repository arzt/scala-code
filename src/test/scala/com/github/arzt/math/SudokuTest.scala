package com.github.arzt.math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SudokuTest extends AnyFlatSpec with Matchers {
  "prefix version" should "have valid row" in {
    new Sudoku(2, 2).hasValidRow("1234121") shouldBe false
    new Sudoku(2, 2).hasValidRow("1232") shouldBe false
    new Sudoku(2, 2).hasValidRow("1234") shouldBe true
    new Sudoku(2, 2).hasValidRow("12") shouldBe true
    new Sudoku(2, 2).hasValidRow("11") shouldBe false
    new Sudoku(2, 2).hasValidRow("123412") shouldBe true
  }

  it should "have valid col" in {
    new Sudoku(2, 2).hasValidCol("12341") shouldBe false
    new Sudoku(2, 2).hasValidCol("1234") shouldBe true
    new Sudoku(2, 2).hasValidCol("12342") shouldBe true
    new Sudoku(2, 2).hasValidCol("12342341") shouldBe true
    new Sudoku(2, 2).hasValidCol("12342344") shouldBe false
    new Sudoku(2, 2).hasValidCol("123423412") shouldBe false
    new Sudoku(2, 2).hasValidCol("123423414") shouldBe true
    new Sudoku(3, 3).hasValidCol(Sudoku.concat(
      "123", "426", "789",
      "234", "537", "891",
      "234", "547", "891",
      "234", "557", "891",
      "234", "56"
    )) shouldBe true
  }
  it should "have valid box" in {
    val s3 = new Sudoku(3, 3)
    s3.hasValidBox(Sudoku.concat(
      "123", "456", "789",
      "456", "789", "123",
      "789"
    )) shouldBe true
    s3.hasValidBox(Sudoku.concat(
      "123", "456", "789",
      "456", "789", "123",
      "781"
    )) shouldBe false
    new Sudoku(2, 2).hasValidBox("12341") shouldBe false
    new Sudoku(2, 2).hasValidBox("12343") shouldBe true
    new Sudoku(2, 2).hasValidBox("123434") shouldBe true
    new Sudoku(2, 2).hasValidBox("123432") shouldBe false
    new Sudoku(2, 2).hasValidBox("12343212") shouldBe true
    new Sudoku(2, 2).hasValidBox("123432122") shouldBe true
    new Sudoku(2, 2).hasValidBox("1234321222") shouldBe false
    new Sudoku(2, 2).hasValidBox("1234321221") shouldBe true
  }
  "next candidate string" should "yield next candidate" in {
    val s = new Sudoku(2, 2)
    val nextCandidate = (x: String) => s.nextCandidate(s.isValid, x)
    nextCandidate("121") shouldBe "122"
    nextCandidate("1") shouldBe "11"
    nextCandidate("11") shouldBe "12"
    nextCandidate("12") shouldBe "121"
    nextCandidate("122") shouldBe "123"
    nextCandidate("123") shouldBe "1231"
  }
  "matches cell" should "respect cell constraint" in {
    val s = new Sudoku(2, 2)
    s.matchesCell("1", "____1") shouldBe true
    s.matchesCell("12", "+r2") shouldBe true
    s.matchesCell("123", "123") shouldBe true
    s.matchesCell("123", "13") shouldBe true
    s.matchesCell("123", "1 3") shouldBe true
    s.matchesCell("123", "122") shouldBe false
    s.matchesCell("122", "122455") shouldBe true
    s.matchesCell("1224", "122") shouldBe true
  }
  "matches row" should "respect row constrains" in {
    val s = new Sudoku(2, 2)
    s.matchesRow("12341", "______2_____") shouldBe true
    s.matchesRow("12341", "______2") shouldBe true
    s.matchesRow("12341", "_______1") shouldBe false
    s.matchesRow("21", "__3_") shouldBe true
    s.matchesRow("1", "21") shouldBe false
    s.matchesRow("1", "2341") shouldBe false
    s.matchesRow("1", "__1_") shouldBe false
  }
  "matches col" should "respect col constrains" in {
    val s = new Sudoku(2, 2)
    s.matchesCol("1", "____2") shouldBe true
    s.matchesCol("12", "_________2") shouldBe false
    s.matchesCol("1", "____1") shouldBe false
  }
  "get box offset 2x2" should "convert absolute index to box offset" in {
    val s = new Sudoku(2, 2)
    s.boxOffset(0) shouldBe 0
    s.boxOffset(4) shouldBe 0
    s.boxOffset(2) shouldBe 2
    s.boxOffset(13) shouldBe 8
    s.boxOffset(11) shouldBe 10
    s.boxOffset(15) shouldBe 10
  }
  "get box offset 3x3" should "convert absolute index to box offset" in {
    val s = new Sudoku(3, 3)
    s.boxOffset(0) shouldBe 0
    s.boxOffset(3) shouldBe 3
    s.boxOffset(6) shouldBe 6
    s.boxOffset(9) shouldBe 0
    s.boxOffset(12) shouldBe 3
    s.boxOffset(15) shouldBe 6
    s.boxOffset(18) shouldBe 0
    s.boxOffset(80) shouldBe 6 * 9 + 2 * 3
  }
  "get internal box offset" should "compute internal box offset" in {
    val s = new Sudoku(2, 2)
    s.inverseBoxIndex(0) shouldBe 0
    s.inverseBoxIndex(1) shouldBe 1
    s.inverseBoxIndex(2) shouldBe 4
    s.inverseBoxIndex(3) shouldBe 5
  }
  "get internal box offset 3x3" should "compute internal box offset" in {
    val s = new Sudoku(3, 3)
    s.inverseBoxIndex(0) shouldBe 0
    s.inverseBoxIndex(1) shouldBe 1
    s.inverseBoxIndex(2) shouldBe 2
    s.inverseBoxIndex(3) shouldBe 3 + 6
    s.inverseBoxIndex(4) shouldBe 4 + 6
    s.inverseBoxIndex(5) shouldBe 5 + 6
    s.inverseBoxIndex(6) shouldBe 6 + 6 + 6
    s.inverseBoxIndex(7) shouldBe 7 + 6 + 6
    s.inverseBoxIndex(8) shouldBe 8 + 6 + 6
  }
  "get internal box offset 3x2" should "compute internal box offset" in {
    val s = new Sudoku(3, 2)
    s.inverseBoxIndex(0) shouldBe 0
    s.inverseBoxIndex(1) shouldBe 1
    s.inverseBoxIndex(2) shouldBe 2
    s.inverseBoxIndex(3) shouldBe 3 + 3
    s.inverseBoxIndex(4) shouldBe 4 + 3
    s.inverseBoxIndex(5) shouldBe 5 + 3
  }
  "within box offset" should "work" in {
    val s = new Sudoku(2, 2)
    s.boxIndex(0) shouldBe 0
    s.boxIndex(1) shouldBe 1
    s.boxIndex(2) shouldBe 0
    s.boxIndex(3) shouldBe 1
    s.boxIndex(4) shouldBe 2
    s.boxIndex(5) shouldBe 3
    s.boxIndex(6) shouldBe 2
    s.boxIndex(7) shouldBe 3
  }
  "exhaustive box conversion test" should "work" in {
    val s = new Sudoku(3, 3)
    for (x <- 0 to s.cellCount) {
      val result = s.inverseBoxIndex(s.boxIndex(x)) + s.boxOffset(x)
      result shouldBe x
    }
  }
  "matches box" should "respect box constrains" in {
    val s = new Sudoku(2, 2)
    s.matchesBox("21", "____1___________") shouldBe false
    s.matchesBox("1", "_____1") shouldBe false
    s.matchesBox("12343", "____________3______") shouldBe true
    s.matchesBox("12343", "_____4") shouldBe true
    s.matchesBox("12343", "_____3") shouldBe false
    s.matchesBox("21", "_____1__________") shouldBe false
    s.matchesBox("1", "_____2") shouldBe true
    s.matchesBox("21", "_____2") shouldBe true
    s.matchesBox("21", "_____1") shouldBe false
  }
  it should "yield matching sudoku 2" in {
    val s = new Sudoku(3, 3)
    val te = Sudoku.concat(
      "827", "154", "396",
      "965", "327", "148",
      "341", "689", "752",

      "593", "468", "271",
      "472", "513", "689",
      "618", "972", "435",

      "786", "235", "914",
      "154", "796", "823",
      "239", "841", "567"
    )
    val result = s.solve(te).toVector
    result.nonEmpty shouldBe true
  }
  it should "match box 3x2" in {
    val s = new Sudoku(3, 2)
    val te =
      "5 64 3" +
        " 3  1 " +
        "6    2" +
        "3    4" +
        " 6  4 " +
        "2 46 1"
    val su = "51642"
    val const1 = s.matchesBox(su, te)
    const1 shouldBe true
  }
  it should "yield matching sudoku 2x3 in this case" in {
    val s = new Sudoku(3, 2)
    val te =
      "5 64 3" +
        " 3  1 " +
        "6    2" +
        "3    4" +
        " 6  4 " +
        "2 46 1"
    val su = "51642343"
    val matchesBox = s.matchesBox(su, te)
    matchesBox shouldBe true
  }
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
  it should "solve diabolical01 sudoku" in {
    val s = new Sudoku(3, 3)
    val steps = s.iterateCandidates(diabolical01).toArray
    println(f"nsteps: ${steps.length}")
    val result = s.solve(diabolical01).toVector
    //s.solve(diabolical01).toVector
    println(result.head)
    println(diabolical01)
    result.nonEmpty shouldBe true
  }
  it should "solve sudoku 2x3" in {
    val s = new Sudoku(3, 2)
    val te =
      "   4 3" +
        " 3  1 " +
        "     2" +
        "3     " +
        " 6  4 " +
        "  4   "
    val result = s.solve(te).toVector
    result.nonEmpty shouldBe true
  }
  it should "a string of random numbers" in {
    val s = new Sudoku(2, 2)
    val result = s.randomNumbers()
    result.length shouldBe s.valueCount
    result.contains("1") shouldBe true
    result.contains("2") shouldBe true
    result.contains("3") shouldBe true
    result.contains("4") shouldBe true
  }
  it should "compute a random template" in {
    val s = new Sudoku(3, 3)
    val result = s.randomTemplate()
    result.length shouldBe s.cellCount
  }
  it should "return random filled sudoku" in {
    val s = new Sudoku(3, 3)
    val result = s.randomFilledSudoku(100)
    s.isValid(result) shouldBe true
    result.length shouldBe 9*9
  }
  it should "transpose a sudoku matrix" in {
    val s = new Sudoku(2, 2)
    val x =
        "1234" +
        "3412" +
        "2143" +
        "4321"
    val expected =
        "1324" +
        "2413" +
        "3142" +
        "4231"
    s.transpose(x) shouldBe expected
  }
  it should "flip a sudoku matrix horizontally" in {
    val s = new Sudoku(2, 2)
    val x =
      "1234" +
        "3412" +
        "2143" +
        "4321"
    val expected =
      "4321" +
        "2143" +
        "3412" +
        "1234"
    s.flipHorizonal(x) shouldBe expected
  }
  it should "flip a sudoku matrix vertically" in {
    val s = new Sudoku(2, 2)
    val x =
      "1234" +
        "3412" +
        "2143" +
        "4321"
    val expected =
      "4321" +
        "2143" +
        "3412" +
        "1234"
    s.flipVertical(x) shouldBe expected
  }
  it should "check if sudoku has unique solution" in {
    val s = new Sudoku(3, 3)
    val unique = s.randomFilledSudoku().updated(10, '_').updated(77, '_')
    println(s.toString(unique))
    val nonUnique = s.randomTemplate()
    s.isUnique(unique) shouldBe true
    s.isUnique(nonUnique) shouldBe false
  }

  it should "generate first 10 sudocus" in {
    val s = new Sudoku(3, 3)
    var i = 0
    val result = s.allSudokus.filter(_.length == 81)
    result.
      foreach(
        x => {
          i += 1
          if (i % 100000 == 0)
            println(x)
          x
        }
      )
    result.length shouldBe 200
  }

}
