package com.github.arzt.audio

object MusicImplicits {

  implicit class DoubleOps(d: Double) {
    def logToHalfTone: Double = d*12
    def halfToneToLog: Double = d/12
    def logToHz: Double = math.pow(2, d)
    def hzToLog: Double = math.log(d)/math.log(2d)
    def rezi: Double = 1/d

    def asLog(f: Double => Double): Double =
      f(d.hzToLog).logToHz
  }

  implicit def const(d: Double): Double => Double = _ => d

  implicit class FunctionOps(f: Double => Double) {

    def +(g: Double => Double): Double => Double =
      x => f(x) + g(x)

    def -(g: Double => Double): Double => Double =
      x => f(x) - g(x)

    def *(g: Double => Double): Double => Double =
      x => f(x) * g(x)

    def /(g: Double => Double): Double => Double =
      x => f(x) / g(x)

    def >>(g: Double => Double): Double => Double =
      x => f(x + g(x))

    def <<(g: Double => Double): Double => Double =
      x => f(x - g(x))

    def append(t0: Double, g: Double => Double): Double => Double =
        Const.append(t0, f, g)

  }

}
