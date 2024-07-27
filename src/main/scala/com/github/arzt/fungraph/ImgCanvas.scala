package com.github.arzt.fungraph

import java.awt.{Canvas, Graphics, Image}

class ImgCanvas(var img: Image) extends Canvas:

  override def paint(g: Graphics): Unit = g.drawImage(img, 0, 0, null)

  override def update(g: Graphics): Unit = {}

  override def repaint(): Unit = paint(getGraphics)

  def setImage(newImg: Image): Unit =
    img = newImg
    repaint()
