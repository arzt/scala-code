package com.github.arzt.sudokui

import com.github.arzt.fungraph
import com.github.arzt.fungraph.{Action, Drawable, Glyph, MouseClicked, MouseMotionAction, Rectangle, Resize}

import java.awt.event.{ComponentEvent, ComponentListener, MouseEvent, MouseListener, MouseMotionListener, WindowEvent, WindowListener, WindowStateListener}
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.{Canvas, Color, Dimension, Font, Graphics, Graphics2D, RenderingHints}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import javax.swing.{JComponent, JFrame, WindowConstants}
import scala.util.Random

object Test:

  def transition(state: SudokuInterface, action: Action): SudokuInterface =
    val SudokuInterface(sudoku, cubeSize, hover) = state
    action match
      case MouseMotionAction(x, y) =>
        val idx = state.getIndex(x, y)
        SudokuInterface(sudoku, cubeSize, idx)
      case MouseClicked(x, y) =>
        val idx = state.getIndex(x, y)
        idx match
          case Some(i) => SudokuInterface(sudoku.flipCell(i), cubeSize, hover)
          case None => state
      case Resize(width, height) =>
        state.copy(subCellSize = math.min(width, height)/28)
      case unhandled =>
        println(f"unhandled: $unhandled")
        state


  def draw(state: SudokuInterface): Iterable[Drawable] =

    val SudokuInterface(sudoku, cellSize, hover) = state

    val subCells = for (ind <- 0 until 9 * 9 * 9) yield
      val isSet = sudoku(ind)
      val x = state.getX(ind)
      val y = state.getY(ind)
      if (hover.contains(ind))
        Rectangle(x, y, cellSize, cellSize, Color.GREEN.brighter())
      else if (isSet)
        Rectangle(x, y, cellSize, cellSize, Color.WHITE)
      else
        Rectangle(x, y, cellSize, cellSize, Color.WHITE)

    val fontSize = state.subCellSize.*(0.75).toInt
    val glyphs = (0 until 9 * 9 * 9)
      .filter(sudoku.apply)
      .map(
        ind => {
          val k = state.sudoku.getK(ind)
          val x = state.getX(ind)
          val y = state.getY(ind)
          Glyph(x + cellSize/4, y + cellSize/1.5, (k + 1).toString, new Font(Font.MONOSPACED, Font.BOLD, fontSize), color = Color.darkGray)
        }
      )

    subCells ++ glyphs


  def drawSwing(elements: Iterable[Drawable], g: Graphics2D): Unit =
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val oldColor = g.getColor
    for (elem <- elements)
      elem match
        case Rectangle(x, y, width, height, color) =>
          g.setColor(color)
          g.fill(new Rectangle2D.Double(x, y, width, height))
        case Glyph(x, y, text, font, color) =>
          g.setColor(color)
          g.setFont(font)
          g.drawString(text, x.toFloat, y.toFloat)
    g.setColor(oldColor)

  def main(args: Array[String]): Unit =
    val subCellSize = 40
    var state = SudokuInterface(Sudoku(), subCellSize)
    val windowWidth = 1000
    val windowHeight = 808
    state = transition(state, Resize(windowWidth, windowHeight))
    val f = new JFrame()
    f.setSize(windowWidth, windowHeight)
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
        override def mouseClicked(e: MouseEvent): Unit = {}

        override def mouseEntered(e: MouseEvent): Unit = {}

        override def mouseExited(e: MouseEvent): Unit = {}

        override def mouseReleased(e: MouseEvent): Unit = {}

        override def mousePressed(e: MouseEvent): Unit =
          val action = MouseClicked(e.getX, e.getY)
          val nextState = transition(state, action)
          if (nextState != state)
            state = nextState
            canvas.repaint(5L)
    )

    canvas.addComponentListener(new ComponentListener {
      override def componentResized(e: ComponentEvent): Unit =
        state = transition(state, Resize(e.getComponent.getWidth, e.getComponent.getHeight))

      override def componentMoved(e: ComponentEvent): Unit = ???

      override def componentShown(e: ComponentEvent): Unit = ???

      override def componentHidden(e: ComponentEvent): Unit = ???
    })

    f.add(canvas)
    f.setVisible(true)
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

