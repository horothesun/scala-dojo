package adventofcode22

import cats._
import cats.data.NonEmptyList
import cats.derived
import cats.implicits._
import scala.annotation.tailrec
import Day9.Rope._

object Day9 {

  sealed trait Rope[A] {

    def getHead: Knot[A] = this match {
      case k @ Knot(_)      => k
      case Segment(head, _) => head
    }

    def getLast: Knot[A] = Knot(this.foldLeft(getHead.a) { case (_, a) => a })

  }
  object Rope {

    case class Knot[A](a: A) extends Rope[A]
    case class Segment[A](head: Knot[A], tail: Rope[A]) extends Rope[A]

    implicit val foldable: Foldable[Rope] = derived.semiauto.foldable

    def make2[A](a: A): Rope[A] = fromNel(NonEmptyList.of(a, a))

    def fromNel[A](nel: NonEmptyList[A]): Rope[A] = {
      @tailrec
      def aux(acc: Rope[A], rest: List[A]): Rope[A] =
        rest match {
          case Nil     => acc
          case a :: as => aux(Segment(head = Knot(a), tail = acc), as)
        }
      val NonEmptyList(reversedHead, reversedTail) = nel.reverse
      aux(acc = Knot(reversedHead), rest = reversedTail)
    }

  }

  case class Pos(x: Int, y: Int) {

    def left: Pos = Pos(x - 1, y)
    def right: Pos = Pos(x + 1, y)
    def up: Pos = Pos(x, y + 1)
    def down: Pos = Pos(x, y - 1)

    // TODO: test!!! 🔥🔥🔥
    def isAdjacentTo(that: Pos): Boolean = (Math.abs(x - that.x) <= 1) && (Math.abs(y - that.y) <= 1)

  }
  object Pos {
    val start: Pos = Pos(0, 0)
  }

}
