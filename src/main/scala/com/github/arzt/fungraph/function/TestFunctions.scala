package com.github.arzt.fungraph.function

import scala.math.{Pi, cos, exp, sin}
import scala.util.Random

def b2d(x: Boolean): Double = if (x) 1.0 else 0.0

def sigmoid(x: Double): Double = 1 / (1 + exp(x))

def circle(
    centerX: Double => Double,
    centerY: Double => Double
)(t: Double, x: Double, y: Double, i: Int): Double =
  val radius = 100 + 30 * sin(t)
  val radius2 = 200 + 10 * cos(t)
  val x0 = x - centerX(t)
  val y0 = y - centerY(t)
  val (a0, r0, g0, b0) = (1.0, 1.0, 0.0, 0.0)
  val (a1, r1, g1, b1) = (1.0, 1.0, 1.0, 0.0)
  val decision = x0 * x0 + y0 * y0 - radius * radius
  val insideSoft = sigmoid(decision * 0.0009)
  val incircleColor =
    b2d(i == 0) * a0 + b2d(i == 1) * r0 + b2d(i == 2) * g0 + b2d(i == 3) * b0
  val outCircleColor =
    b2d(i == 0) * a1 + b2d(i == 1) * r1 + b2d(i == 2) * g1 + b2d(i == 3) * b1
  insideSoft * incircleColor + (1.0 - insideSoft) * outCircleColor

def waves(t: Double, x: Double, y: Double, i: Int): Double =
  if (i == 0) 1.0
  else
    val colorOffsetX =
      if (i == 1)
        0.5
      else if (i == 2)
        -0.5
      else
        1.0

    val colorOffsetY =
      if (i == 1)
        -1.0
      else if (i == 2)
        0.5
      else
        1.0

    val timeOffsetX = ((t * colorOffsetX * 10) % 10) / 10 * Pi * 2
    val timeOffsetY = ((t * colorOffsetY * 10) % 10) / 10 * Pi * 2
    val xsin = sin(x / 10 + timeOffsetX) * 0.5 + 0.5
    val ysin = sin(y / 10 - timeOffsetY) * 0.5 + 0.5
    xsin * 0.5 + ysin * 0.5

def colors(t: Double, x: Double, y: Double, i: Int): Double =
  val r = new Random(seed = (t*1000).toLong + i)
  if (i == 0) 1.0
  else r.nextInt(255)

def noise(t: Double, x: Double, y: Double, i: Int): Double =
  if (i == 0)
    1.0
  else
    scala.math.random()
