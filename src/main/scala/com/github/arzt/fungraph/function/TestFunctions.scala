package com.github.arzt.fungraph.function

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

    val timeOffsetX = ((t * colorOffsetX * 10) % 10) / 10 * Math.PI * 2
    val timeOffsetY = ((t * colorOffsetY * 10) % 10) / 10 * Math.PI * 2
    val xsin = Math.sin(x / 10 + timeOffsetX) * 0.5 + 0.5
    val ysin = Math.sin(y / 10 - timeOffsetY) * 0.5 + 0.5
    xsin * 0.5 + ysin * 0.5

def noise(t: Double, x: Double, y: Double, i: Int): Double =
  if (i == 0)
    1.0
  else
    scala.math.random()
