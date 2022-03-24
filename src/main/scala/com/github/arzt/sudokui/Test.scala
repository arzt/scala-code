package com.github.arzt.sudokui

import com.github.arzt.fungraph.{Action, Drawable, MouseClicked, MouseMotionAction, Rectangle}

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{Canvas, Color, Dimension, Graphics, Graphics2D, RenderingHints}
import javax.swing.{JComponent, JFrame, WindowConstants}
import scala.util.Random

object Test:

  def transition(state: SudokuInterface, action: Action): SudokuInterface =
    val SudokuInterface(sudoku, cubeSize, hover) = state
    action match
      case MouseMotionAction(x, y) =>
        val idx = state.getIndex(x, y)
        SudokuInterface(sudoku, cubeSize, Some(idx))
      case MouseClicked(x, y) =>
        val idx = state.getIndex(x, y)
        SudokuInterface(sudoku.flipCell(idx), cubeSize, hover)
      case unhandled =>
        println(f"unhandled: $unhandled")
        state


  def draw(state: SudokuInterface): IndexedSeq[Drawable] =

    val SudokuInterface(sudoku, cubeSize, hover) = state

    for (ind <- 0 until 9 * 9 * 9) yield
      val i = sudoku.getI(ind)
      val j = sudoku.getJ(ind)
      val k = sudoku.getK(ind)
      val isSet = state.sudoku(ind)
      val x = sudoku.getX(ind)
      val y = sudoku.getY(ind)
      if (state.hover.contains(ind))
        Rectangle(x * state.cellSize, y * state.cellSize, state.cellSize, state.cellSize, Color.BLUE)
      else if (isSet)
        Rectangle(x * state.cellSize, y * state.cellSize, state.cellSize, state.cellSize, Color.GREEN)
      else
        Rectangle(x * state.cellSize, y * state.cellSize, state.cellSize, state.cellSize, Color.RED)


  def drawSwing(elements: Seq[Drawable], graphics2D: Graphics2D): Unit =
    val oldColor = graphics2D.getColor
    for (elem <- elements)
      elem match
        case Rectangle(x, y, width, height, color) =>
          graphics2D.setColor(color)
          graphics2D.fill(new Rectangle2D.Double(x, y, width, height))
    graphics2D.setColor(oldColor)

  def main(args: Array[String]): Unit =
    val cubeSize = 20
    var state = SudokuInterface(Sudoku(), cubeSize)
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
          val nextState = transition(state, action)
          if (nextState != state)
            canvas.repaint(5L)
            state = nextState
    )

    canvas.addMouseListener(
      new MouseListener:
        override def mouseClicked(e: MouseEvent): Unit =
          val action = MouseClicked(e.getX, e.getY)
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

