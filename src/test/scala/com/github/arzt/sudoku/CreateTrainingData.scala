package com.github.arzt.sudoku

import java.awt.RenderingHints.{KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage.TYPE_BYTE_GRAY
import java.awt.image.{BufferedImage, ConvolveOp, DataBufferByte, Kernel}
import java.awt.{Color, Font, Graphics2D, RenderingHints}
import java.io.File
import java.lang.Byte.toUnsignedInt
import javax.imageio.ImageIO
import scala.util.Random
import scala.util.Random.{nextGaussian => gauss}

object CreateTrainingData {

  def randomElement[A](seq: A*): A = seq(Random.nextInt(seq.length))

  def getSample(label: String): BufferedImage = {
    val rh = new RenderingHints(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON);
    val img = new BufferedImage(30, 30, TYPE_BYTE_GRAY)
    val border = (gauss()*0.2 + 0.5).min(0.9).max(0.1)
    val graph = img.getGraphics().asInstanceOf[Graphics2D]
    val gauss1 = Random.nextDouble()
    val gauss2 = Random.nextDouble()
    val blackRaw = gauss1 * (border*0.9)
    val whiteRaw = border + gauss2*(1 - border*0.9)
    graph.setRenderingHints(rh)
    val black = (blackRaw.min(border).max(0.0)*255.0).toInt
    val white = (whiteRaw.min(1.0).max(border)*255.0).toInt
    val whiteColor = new Color(white, white, white)
    val blackColor = new Color(black, black, black)
    graph.setPaint(whiteColor)
    graph.fillRect(0,0,img.getWidth(), img.getHeight())
    graph.setPaint(blackColor)
    val font = {
      val size = 20
      val trans = new AffineTransform(
        1f + gauss() * 0.2, 0f + gauss() * 0.2,
        0f + gauss() * 0.2, 1f + gauss() * 0.2,
        0f + gauss() * 0.5, 0f + gauss() * 0.5
      )
      randomElement(
        new Font("FreeSans", Font.BOLD, size),
        new Font("FreeSans", Font.PLAIN, size),
        //new Font("FreeSans Oblique", Font.PLAIN, size)
      ).deriveFont(trans)
    }
    graph.setFont(font)
    if (label != "0") {
      graph.drawString(label, 9, 21)
    }
    val conv = {
      val kernelData: Array[Float] = Array.fill(3*3)(Random.nextFloat)
      val kSum = kernelData.sum
      kernelData.mapInPlace(_ / kSum)
      val convKernel = new Kernel(3, 3, kernelData)
      new ConvolveOp(convKernel, ConvolveOp.EDGE_NO_OP, null)
    }
    val outImg = new BufferedImage(img.getWidth(), img.getHeight(), img.getType)
    conv.filter(img, outImg)
    val muNoise = Random.nextInt(30)
    val buf = outImg.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
    for (i <- buf.indices) {
      val in = toUnsignedInt(buf(i))
      val noise = gauss()*muNoise
      val noisyInt = (in + noise).toInt
      val noisy = noisyInt.min(255).max(0).toByte
      buf(i) = noisy
    }
    outImg
  }


  def main(args: Array[String]): Unit = {
    Random.setSeed(1000)
    println(sys.env("CLASS"))
    val numXTiles = 10
    val numYTiles = 480
    val width = 30
    val height = width
    val img = new BufferedImage(width*numXTiles, height*numYTiles, TYPE_BYTE_GRAY)
    for (x <- 0 until numXTiles; y <- 0 until numYTiles) {
      val sample = getSample(x.toString)
      val xWidth = x * width
      val yHeight = y * height
      img.getGraphics.drawImage(sample, xWidth, yHeight, sample.getWidth(), sample.getHeight(), null)
    }
    ImageIO.write(img, "png", new File("/home/sebastian/Desktop/test3.png"))
  }
}
