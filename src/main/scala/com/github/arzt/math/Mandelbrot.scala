package com.github.arzt.math

import java.awt.image.BufferedImage
import java.io.File

import scala.annotation.tailrec
import com.github.arzt.tensor.TensorImplicits._
import com.github.arzt.tensor.image.ImageTool
import com.github.arzt.tensor.{IndexTensor, Tensor}
import javax.imageio.ImageIO

import scala.collection.immutable.NumericRange

object Mandelbrot {

  def len(a: Double, b: Double): Double = a * a + b * b

  def im(a: Double, b: Double): Double = b

  def re(a: Double, b: Double): Double = a

  def prodRe(a1: Double, b1: Double, a2: Double, b2: Double): Double = a1 * a2 - b1 * b2

  def prodIm(a1: Double, b1: Double, a2: Double, b2: Double): Double = a1 * b2 + b1 * a2

  def plusRe(a1: Double, b1: Double, a2: Double, b2: Double): Double = a1 + a2

  def plusIm(a1: Double, b1: Double, a2: Double, b2: Double): Double = b1 + b2

  def squareRe(a: Double, b: Double): Double = prodRe(a, b, a, b)

  def squareIm(a: Double, b: Double): Double = prodIm(a, b, a, b)

  def mandelbrot(a: Double, b: Double, maxIter: Int = 1000): Int = {
    @tailrec def mandelbrotRec(
        za: Double,
        zb: Double,
        i: Int
    ): Int = {
      if (len(za, zb) > 4 || i >= maxIter) {
        maxIter - i
      } else {
        val pa = za + a
        val pb = zb + b
        val sa = squareRe(pa, pb)
        val sb = squareIm(pa, pb)
        mandelbrotRec(sa, sb, i + 1)
      }
    }
    mandelbrotRec(0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val unit = 20
    val xs = (-2*unit until unit)
    val ys = (-unit until unit)
    val t = IndexTensor(List(xs.length, ys.length))
    val image = new BufferedImage(xs.length, ys.length, BufferedImage.TYPE_INT_RGB)
    import com.github.arzt.tensor.convert.implicits._
    val hui = t.map(x => (xs(x(0))/unit, ys(x(1))/unit))
      .map{
        case (a,b) =>  Mandelbrot.mandelbrot(a,b)
      }
      .inflate[Byte]
      .reshape(xs.length, ys.length, 4)

    val inputImg = ImageIO.read(new File("/home/sebastian/Desktop/test.png"))
    val inputImgTensor = inputImg.asTensor
    ImageTool.writeImage(image, hui)
    ImageIO.write(image, "png", new File("/home/sebastian/Desktop/test.png"))
    println("test")
  }
}
