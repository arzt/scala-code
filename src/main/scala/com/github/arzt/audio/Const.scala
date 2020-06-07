package com.github.arzt.audio

import java.util

import MusicImplicits._
import javax.sound.sampled.AudioFormat

import scala.collection.mutable
import scala.collection.mutable

object Const {

  val id: Double => Double = x => x

  val square: Double => Double = x => math.pow(x, 2)

  val sin: Double => Double = math.sin

  val cos: Double => Double = math.cos

  def pow(base: Double): Double => Double = x => math.pow(base, x)

  val exp: Double => Double = math.exp

  val lof: Double => Double = math.log

  val saw: Double => Double = _ % 1

  val was: Double => Double = saw * -1.0 + 1.0

  val sin1: Double => Double = (id * 2.0 * math.Pi) andThen sin

  val sin2: Double => Double = (sin1 + 1.0) / 2.0

  val Zero: Double => Double = _ => 0

  val One: Double => Double = _ => 1

  def osc(a: Double, b: Double, fr: Double): Double => Double =
    x => 0.5 * (b + a) * x - 0.5 * (b - a) * math.sin(fr * x + math.Pi / 2) / fr

  def split(
      t0: Double,
      f1: Double => Double,
      f2: Double => Double
  ): Double => Double =
    t => if (t <= t0) f1(t) else f2(t)

  def append(
      t0: Double,
      f1: Double => Double,
      f2: Double => Double
  ): Double => Double =
    t => if (t <= t0) f1(t) else f2(t - t0)

  def toMono(data: Array[Double], format: AudioFormat): Double => Double =
    t => {
      val idx = (t * format.getSampleRate).toInt
      data.applyOrElse(idx, (x: Int) => 0.0)
    }

  def toStereo(
      data: Array[Double],
      format: AudioFormat
  ): (Double => Double, Double => Double) = {
    val rate = format.getSampleRate
    val left: Double => Double =
      x => {
        val i = (x * rate).toInt * 2
        if (i >= 0 && i < data.length) data(i) else 0.0
      }
    val right: Double => Double =
      x => {
        val i = (x * rate).toInt * 2 + 1
        if (i >= 0 && i < data.length) data(i) else 0.0
      }
    (left, right)
  }

  def stepFun(x: Array[Double], y: Array[Double]): Double => Double =
    t => {
      val i = util.Arrays.binarySearch(x, t)
      val j = if (i >= 0) i else -(i + 2)
      y.applyOrElse(j, (_: Int) => 0)
    }

}
