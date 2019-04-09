package com.github.arzt.audio

object Iterators {

  def interleave[T](i1: Iterator[T], i2: Iterator[T]): Iterator[T] =
    new Iterator[T] {

      var first = false

      override def hasNext: Boolean =
        if(first) i1.hasNext else i2.hasNext

      override def next(): T = {
        first = !first
        if(first) {
          i1.next()
        } else {
          i2.next()
        }
      }
    }

}
