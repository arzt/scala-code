package com.github.arzt.sudoku

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.awt.{Color, Font, Graphics2D, RenderingHints}
import java.io.File
import javax.imageio.ImageIO
    import java.awt.GraphicsEnvironment

object TestFonts {
  def main(args: Array[String]): Unit = {


    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment

    val allFonts = ge.getAllFonts
    allFonts.map(x => x.getFontName()).toSet.toSeq.sorted.foreach(println)
  }
}
