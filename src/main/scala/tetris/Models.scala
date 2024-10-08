package tetris

import Models.MergedIntersection._
import shape.Shape

object Models {

  // global coordinates
  case class Coord(x: Int, y: Int)

  sealed trait MergedIntersection[A] {
    def show(filled: A => String, hole: => String): String = this match {
      case NotIntersecting()         => "NotIntersecting"
      case ValidIntersection(bl, mi) => s"ValidIntersection(\nbottomLeft: $bl\n,\n${mi.show(filled, hole)}\n)"
      case CollidingIntersection(bl, i1, i2) =>
        s"Colliding(\nbottomLeft: $bl\n,\n${i1.show(filled, hole)}\n,\n${i2.show(filled, hole)}\n)"
    }
  }
  object MergedIntersection {
    case class NotIntersecting[A]() extends MergedIntersection[A]
    case class ValidIntersection[A](bottomLeft: Coord, mergedIntersection: Shape[A]) extends MergedIntersection[A]
    case class CollidingIntersection[A](bottomLeft: Coord, intersection1: Shape[A], intersection2: Shape[A])
        extends MergedIntersection[A]
  }

  object Mono

}
