package com.github.arzt.fungraph

import java.awt.Color

trait Drawable

case class Rectangle(x: Double, y: Double, width: Double, height: Double, color: Color = Color.BLACK) extends Drawable
