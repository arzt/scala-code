package com.github.arzt.fungraph

trait Action

case class MouseMotionAction(x: Int, y: Int) extends Action

case class MouseClicked(x: Int, y: Int) extends Action

case class Resize(width: Int, height: Int) extends Action
