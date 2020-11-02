package com.github.arzt.math

import com.github.arzt.math.Mandelbrot.{prodIm, prodRe}
import org.specs2.mutable.Specification

class MandelbrotTest extends Specification {

  "compute prod of complex numbers" >> {
    prodRe(3, 5, 4, 2) === 2
    prodIm(3, 5, 4, 2) === 26
  }

  "iterating" should {
    "test2" in {
      val bo = 40
      val ao = 20
      (-ao to ao).foreach{ i =>
        (-2*bo to bo).foreach{ j =>
          val a = i/ao.toDouble
          val b = j/bo.toDouble
          val result = if (Mandelbrot.mandelbrot(b, a) > 0) '*' else ' '
          val result2 = Mandelbrot.mandelbrot(b, a) % 10
          print(result)
        }
        println()
      }
      1 === 1
    }
    "test3" in {
      val i = Mandelbrot.mandelbrot(0.25000, 0.0)
      i === 0
    }
  }


}
