package com.github.arzt.sudokui

import scala.util.Random

case class SudokuiState(state: Seq[Boolean], hover: Option[Int] = None, cubeSize: Int)

object SudokuiState:
  def apply(cubeSize: Int): SudokuiState =
    new SudokuiState(
      state = Vector.fill(9*9*9)(false),
      cubeSize = cubeSize
    )

  def random(cubeSize: Int = 5): SudokuiState =
    new SudokuiState(
      state = Vector.fill(9*9*9)(Random.nextBoolean()),
      cubeSize = cubeSize
    )