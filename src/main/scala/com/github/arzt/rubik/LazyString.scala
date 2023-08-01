package com.github.arzt.rubik

trait LazyString extends (Int => Char):
  def length: Int
  def apply(i: Int): Char
  def updated(i: Int, value: Char): LazyString = UpdatedString(this, i, value)
  override def toString: String = (0 until length).view.map(apply).mkString

case class UpdatedString(s: LazyString, index: Int, value: Char) extends LazyString:
  def length: Int = s.length
  override def apply(i: Int): Char =
    if (i == index)
      value
    else
      s(i)

case class Root(s: String) extends LazyString:
  def length: Int = s.length
  override def apply(i: Int): Char = s(i)
  override def toString() = s

object LazyString:
  def apply(s: String): LazyString = Root(s)

