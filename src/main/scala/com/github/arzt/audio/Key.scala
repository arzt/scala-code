package com.github.arzt.audio
import MusicImplicits.DoubleOps
object Key {

  val Base = 440

  val A: Double = Base
  val A_# : Double = A.asLog(_ + 1d / 12)
  val B_b: Double = A.asLog(_ + 1d / 12)
  val B: Double = A.asLog(_ + 2d / 12)
  val B_# : Double = A.asLog(_ + 3d / 12)
  val C_b: Double = A.asLog(_ + 2d / 12)
  val C: Double = A.asLog(_ + 3d / 12)
  val C_# : Double = A.asLog(_ + 4d / 12)
  val D_b: Double = A.asLog(_ + 4d / 12)
  val D: Double = A.asLog(_ + 5d / 12)
  val D_# : Double = A.asLog(_ + 6d / 12)
  val E_b: Double = A.asLog(_ + 6d / 12)
  val E: Double = A.asLog(_ + 7d / 12)
  val E_# : Double = A.asLog(_ + 8d / 12)
  val F_b: Double = A.asLog(_ + 7d / 12)
  val F: Double = A.asLog(_ + 8d / 12)
  val F_# : Double = A.asLog(_ + 9d / 12)
  val G_b: Double = A.asLog(_ + 9d / 12)
  val G: Double = A.asLog(_ + 10d / 12)
  val G_# : Double = A.asLog(_ + 11d / 12)
  val H: Double = A.asLog(_ + 1)
  val A_b: Double = A.asLog(_ + 11d / 12)

}