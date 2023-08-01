package com.github.arzt.rubik

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UpdatableStringTest extends AnyFreeSpec with Matchers:
  "UpdatableString" - {
    "should to nothing on string" in {
      LazyString("test").toString shouldBe "test"
    }
    "should replace single character" in {
      LazyString("test").updated(0, 'T').toString shouldBe "Test"
    }
    "should replace multiple characters" in {
      val hui = LazyString("abcd").updated(0, 'h').updated(0, 'i')
      hui.toString shouldBe "ibcd"
    }
  }