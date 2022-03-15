package com.github.arzt.sudokui

import scala.util.Random

case class Sudoku(cells: Seq[Boolean]):
  def flipCell(i: Int): Sudoku =
    this.copy(cells = cells.updated(i, !cells(i)))

case class SudokuiState(sudoku: Sudoku, hover: Option[Int] = None, cubeSize: Int)

object SudokuiState:
  def apply(cubeSize: Int): SudokuiState =
    new SudokuiState(
      sudoku = Sudoku(Vector.fill(9*9*9)(false)),
      cubeSize = cubeSize
    )

  def random(cubeSize: Int = 5): SudokuiState =
    new SudokuiState(
      sudoku = Sudoku(Vector.fill(9*9*9)(Random.nextBoolean())),
      cubeSize = cubeSize
    )