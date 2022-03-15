package com.github.arzt.sudokui

import com.github.arzt.fungraph.{Action, Drawable, MouseClicked, MouseMotionAction, Rectangle}

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{Canvas, Color, Dimension, Graphics, Graphics2D, RenderingHints}
import javax.swing.{JComponent, JFrame, WindowConstants}
import scala.util.Random

object Test:

  def transition(state: SudokuiState, action: Action): SudokuiState =
    action match
      case MouseMotionAction(x, y) =>
        val row = y / state.cubeSize
        val col = x / state.cubeSize
        val a = row / 3
        val b = col / 3
        val innerRow = row % 3
        val innerCol = col % 3
        val c = innerCol + innerRow * 3
        println((a, b, c))
        val idxNew = ((a * 9) + b)* 9 + c
        state.copy(hover = Some(idxNew))
      case MouseClicked(x, y) =>
        val row = y / state.cubeSize
        val col = x / state.cubeSize
        val a = row / 3
        val b = col / 3
        val innerRow = row % 3
        val innerCol = col % 3
        val c = innerCol + innerRow * 3
        println((a, b, c))
        val idx = ((a * 9) + b)* 9 + c
        state.copy(sudoku = state.sudoku.flipCell(idx))
      case unhandled =>
        println(f"unhandled: $unhandled")
        state


  def draw(state: SudokuiState): IndexedSeq[Drawable] =

    val SudokuiState(sudoku, hover, cubeSize) = state

    for (ind <- 0 until 9 * 9 * 9) yield
      val i = sudoku.getI(ind)
      val j = sudoku.getJ(ind)
      val k = sudoku.getK(ind)
      val isSet = state.sudoku.cells(ind)
      val yOffset = k / 3
      val xOffset = k % 3
      val x = (j * 3 + xOffset) * state.cubeSize
      val y = (i * 3 + yOffset) * state.cubeSize
      val width = state.cubeSize
      val height = state.cubeSize
      if (state.hover.contains(ind))
        Rectangle(x, y, width, height, Color.BLUE)
      else if (isSet)
        Rectangle(x, y, width, height, Color.GREEN)
      else
        Rectangle(x, y, width, height, Color.RED)


  def drawSwing(elements: Seq[Drawable], graphics2D: Graphics2D): Unit =
    val oldColor = graphics2D.getColor
    for (elem <- elements)
      elem match
        case Rectangle(x, y, width, height, color) =>
          graphics2D.setColor(color)
          graphics2D.fill(new Rectangle2D.Double(x, y, width, height))
    graphics2D.setColor(oldColor)

  def main(args: Array[String]): Unit =
    val cubeSize = 30
    var state = SudokuiState(cubeSize)
    val f = new JFrame()
    f.setSize(cubeSize*30, cubeSize*30)
    f.setTitle("Title!")
    val canvas = new JComponent():
      override def paintComponent(g: Graphics): Unit =
        val imgWidth = this.getWidth
        val imgHeight = this.getHeight
        val buffer = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_RGB)
        val elements = draw(state)
        drawSwing(elements, buffer.getGraphics.asInstanceOf[Graphics2D])
        g.drawImage(buffer, 0, 0, this)


    canvas.addMouseMotionListener(
      new MouseMotionListener:
        override def mouseDragged(e: MouseEvent): Unit = {}


        override def mouseMoved(e: MouseEvent): Unit =
          val action = MouseMotionAction(e.getX, e.getY)
          println(action)
          val nextState = transition(state, action)
          if (nextState != state)
            canvas.repaint(5L)
            state = nextState
    )

    canvas.addMouseListener(
      new MouseListener:
        override def mouseClicked(e: MouseEvent): Unit =
          val action = MouseClicked(e.getX, e.getY)
          println(action)
          val nextState = transition(state, action)
          if (nextState != state)
            state = nextState
            canvas.repaint(5L)

        override def mouseEntered(e: MouseEvent): Unit = {}

        override def mouseExited(e: MouseEvent): Unit = {}

        override def mouseReleased(e: MouseEvent): Unit = {}

        override def mousePressed(e: MouseEvent): Unit = {}
    )

    f.add(canvas)
    f.setVisible(true)
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

