package tetris

import shape.Shape
import Models.MergedIntersection._

object Models {

  // global coordinates
  case class Coord(x: Int, y: Int)

  sealed trait MergedIntersection[A] {
    def show(filled: A => String, hole: => String): String = this match {
      case NotIntersecting()             => "NotIntersecting"
      case ValidIntersection(mi)         => s"ValidIntersection(\n${mi.show(filled, hole)}\n)"
      case CollidingIntersection(i1, i2) => s"Colliding(\n${i1.show(filled, hole)}\n,\n${i2.show(filled, hole)}\n)"
    }
  }
  object MergedIntersection {
    case class NotIntersecting[A]() extends MergedIntersection[A]
    case class ValidIntersection[A](mergedIntersection: Shape[A]) extends MergedIntersection[A]
    case class CollidingIntersection[A](intersection1: Shape[A], intersection2: Shape[A]) extends MergedIntersection[A]
  }

  sealed trait Color
  object Color {
    case object Mono extends Color
  }

}