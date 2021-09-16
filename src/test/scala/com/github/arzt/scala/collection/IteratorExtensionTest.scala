package com.github.arzt.scala.collection

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class IteratorExtensionTest extends AnyFreeSpec with Matchers {
  "An iterator with no dublicater" - {
    "should have no duplicates" in {

      assert(Iterator(1,2).hasNoDuplicate)
      assert(!Iterator(1,2,1).hasNoDuplicate)
      assert(!Iterator(1, 3.7, "hallo", 'r', "hallo").hasNoDuplicate)
    }
  }
}
