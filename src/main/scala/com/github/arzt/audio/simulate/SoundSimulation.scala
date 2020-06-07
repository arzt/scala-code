package com.github.arzt.audio.simulate


object SoundSimulation {

  def nextWorld(t: Double): World => World =
    old => {
      val newAtoms = old
        .atoms
        .map { atom =>
          val forces = atom
            .force
            .map { i =>
              (atom, old.atoms(i))
            }
            .map { case (thisAtom, otherAtom) =>
              otherAtom.x - thisAtom.x
            }
          val force = forces.sum * 0.005
          val newSpeed = (atom.speed + force / atom.mass)*0.999
          val aCopy = atom.copy(
            speed = newSpeed,
            x = atom.x + newSpeed
          )
          aCopy
        }
      World(newAtoms)
    }

  def main(args: Array[String]): Unit = {
    val start = Atom(
      fix = true
    )
    val end = start.copy()
    val middle =
      0.to(20)
        .map { x =>
          Atom(fix = false, force = List(x, x + 2))
        }
    val newMiddle = middle.updated(3, middle(3).copy(x = 1.0))
    val world = World(Vector(start) ++ newMiddle ++ Vector(end))
    val time = Iterator
      .iterate(world) { old =>
        nextWorld(0.1)(old)
      }

    time
        .zipWithIndex
      .foreach { case (x, i) => {

        val y = x.atoms(4).x
        println(s"$i: $y")
      }
      }
    println(world)
    println("test")
  }
}
