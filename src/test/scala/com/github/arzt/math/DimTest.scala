package com.github.arzt.math

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class DimTest extends FreeSpec with Matchers {
  "dim" - {
    "should" in {

      val a = UnitDim(4, 0 until 4)
      val b = Comp(5, Seq(0, 1, 0, 2, 3, 0), a)

      b.length shouldBe 24
    }
  }
}
