package com.github.arzt.fungraph.awt

import java.awt.Component
import java.awt.event.{MouseEvent, MouseMotionAdapter}

class MouseLocationRelative(val component: Component)
    extends MouseMotionAdapter:

  private var newestEvent: Option[MouseEvent] = None
  private var event: Option[MouseEvent] = None
  private var lastTime: Double = 0.0
  private var time: Double = 0.0
  component.addMouseMotionListener(this)

  override def mouseMoved(e: MouseEvent): Unit = newestEvent = Some(e)

  override def mouseDragged(e: MouseEvent): Unit = newestEvent = Some(e)

  def getX(t: Double): Double =
    if (t > lastTime)
      event = newestEvent
      lastTime = t
    event.map(_.getX.toDouble).getOrElse(0.0)

  def getY(t: Double): Double =
    if (t > lastTime)
      event = newestEvent
      lastTime = t
    event.map(x => component.getHeight - x.getY.toDouble).getOrElse(0.0)
