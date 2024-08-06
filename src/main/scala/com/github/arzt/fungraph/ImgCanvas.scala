package com.github.arzt.fungraph

import java.awt.{Canvas, Graphics, Image}

class ImgCanvas(var img: Image) extends Canvas:

  override def paint(g: Graphics): Unit =
    g.drawImage(img, 0, 0, getWidth, getHeight, 0, 0, getWidth, getHeight, null)

  // override def update(g: Graphics): Unit = {}
  override def update(g: Graphics): Unit = paint(g)

  override def repaint(): Unit =
    super.repaint()
    paint(getGraphics)
