package com.github.arzt.audio.simulate

case class Atom(
    x: Double = 0,
    speed: Double = 0,
    mass: Double = 1,
    fix: Boolean,
    force: List[Int] = Nil
)
