package com.github.arzt.scala

package object collection {

  implicit class IteratorExtension[T](val it: Iterator[T]) extends AnyVal {

    def filterIndex(p: Long => Boolean): Iterator[T] = {
      var i = 0L
      it.filter(_ => {
        val a = i
        i += 1
        p(a)
      })
    }

    def everyK(k: Int): Iterator[T] =
      filterIndex(_ % k == 0)

    def sample(p: Double): Iterator[T] =
      it.filter(_ => math.random < p)

    def hasNoDuplicate: Boolean = {
      val init: (Boolean, Set[T]) = (true, Set())
      it.scanLeft(init) {
          case ((_, set), a) => {
            (!set.contains(a), set + a)
          }
        }
        .map(_._1)
        .forall(identity)
    }

  }

}
