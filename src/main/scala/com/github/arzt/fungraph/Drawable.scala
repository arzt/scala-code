package com.github.arzt.fungraph

import java.awt.{Color, Font}

trait Drawable

case class Rectangle(x: Double, y: Double, width: Double, height: Double, color: Color = Color.BLACK) extends Drawable

case class Glyph(x: Double, y: Double, text: String, font: Font, color: Color = Color.BLACK) extends Drawable
