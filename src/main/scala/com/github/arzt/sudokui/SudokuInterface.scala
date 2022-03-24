package com.github.arzt.sudokui

import scala.util.Random

case class Sudoku(cells: Seq[Boolean]):
  def flipCell(i: Int): Sudoku =
    this.copy(cells = cells.updated(i, !cells(i)))

  def apply(i: Int): Boolean = cells(i)

  def getK(index: Int): Int = index % 9

  def getJ(index: Int): Int = (index / 9) % 9

  def getI(index: Int): Int = (index / 9 / 9) % 9

  def getX(index: Int): Int =
    val i = getI(index)
    val j = getJ(index)
    val k = getK(index)
    val xOffset = k % 3
    val x = j * 3 + xOffset
    x

  def getY(index: Int): Int =
    val i = getI(index)
    val j = getJ(index)
    val k = getK(index)
    val yOffset = k / 3
    val y = i * 3 + yOffset
    y

object Sudoku:
  def apply(): Sudoku = Sudoku(Vector.fill(9*9*9)(true))

  def random(): Sudoku = Sudoku(Vector.fill(9*9*9)(Random.nextBoolean()))

case class SudokuInterface(sudoku: Sudoku, cellSize: Int, hover: Option[Int] = None):
  def getIndex(x: Int, y: Int): Int =
    val boxX = x/((cellSize*3 + 1)*3 + 1)
    val boxY = y/((cellSize*3 + 1)*3 + 1)
    val cellX = (x - boxX - 1)/(cellSize + 1)/3
    val cellY = (y - boxY - 1)/(cellSize + 1)/3
    val col = (x - boxX - 1 - cellX)/ cellSize
    val row = (y - boxY - 1 - cellY)/ cellSize
    println(f"boxX $boxX cellX $cellX subCellX $col")
    val a = row / 3
    val b = col / 3
    val innerRow = row % 3
    val innerCol = col % 3
    val c = innerCol + innerRow * 3
    val idxNew = ((a * 9) + b)* 9 + c
    idxNew

  def getX(index: Int): Int =
    val i = sudoku.getI(index)
    val j = sudoku.getJ(index)
    val k = sudoku.getK(index)
    val addBox = j/3
    val xOffset = k % 3
    val x = j * 3 + xOffset
    x * cellSize + addBox + j

  def getY(index: Int): Int =
    val i = sudoku.getI(index)
    val j = sudoku.getJ(index)
    val k = sudoku.getK(index)
    val add = i/3
    val yOffset = k / 3
    val y = i * 3 + yOffset
    y * cellSize + add + i

