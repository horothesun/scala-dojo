package adventofcode22

import cats._
import cats.data.NonEmptyList
import cats.derived
import cats.implicits._
import scala.annotation.tailrec
import Day9.Proximity._
import Day9.Rope._

object Day9 {

  sealed trait Rope[A] {

    def getHead: A = this match {
      case Knot(a)          => a
      case Segment(head, _) => head
    }

    def getLast: A = this.foldLeft(getHead) { case (_, a) => a }

    def length: Int = this.toList.length

    def toNel: NonEmptyList[A] = this match {
      case Knot(a)             => NonEmptyList.one(a)
      case Segment(head, tail) => NonEmptyList(head, tail.toList)
    }

    def scanLeft[B](b: B)(f: (B, A) => B): Rope[B] = {
      val NonEmptyList(a, as) = toNel
      fromNel(as.scanLeftNel(f(b, a))(f))
    }

  }
  object Rope {

    case class Knot[A](a: A) extends Rope[A]
    case class Segment[A](head: A, tail: Rope[A]) extends Rope[A]

    implicit val foldable: Foldable[Rope] = derived.semiauto.foldable

    def make2[A](a: A): Rope[A] = fromNel(NonEmptyList.of(a, a))

    def make10[A](a: A): Rope[A] = fromNel(NonEmptyList(a, List.fill[A](9)(a)))

    def fromNel[A](nel: NonEmptyList[A]): Rope[A] = {
      @tailrec
      def aux(acc: Rope[A], rest: List[A]): Rope[A] =
        rest match {
          case Nil     => acc
          case a :: as => aux(Segment(head = a, tail = acc), as)
        }

      val NonEmptyList(reversedHead, reversedTail) = nel.reverse
      aux(acc = Knot(reversedHead), rest = reversedTail)
    }

  }

  sealed trait Direction
  object Direction {
    case object Left extends Direction
    case object Right extends Direction
    case object Up extends Direction
    case object Down extends Direction

    def values: Array[Direction] = Array(Left, Right, Up, Down)

    def from(s: String): Option[Direction] = s match {
      case "L" => Some(Left)
      case "R" => Some(Right)
      case "U" => Some(Up)
      case "D" => Some(Down)
      case _   => None
    }
  }

  sealed trait Proximity
  object Proximity {
    case object Adjacent extends Proximity
    case object Distant extends Proximity
  }

  case class Pos(x: Int, y: Int) {

    val move: Direction => Pos = {
      case Direction.Left  => Pos(x - 1, y)
      case Direction.Right => Pos(x + 1, y)
      case Direction.Up    => Pos(x, y + 1)
      case Direction.Down  => Pos(x, y - 1)
    }

    def getProximity(that: Pos): Proximity =
      if ((Math.abs(x - that.x) <= 1) && (Math.abs(y - that.y) <= 1)) Adjacent else Distant

    def getNewFollowerPos(leader: Pos): Pos = getProximity(leader) match {
      case Adjacent => this
      case Distant =>
        Pos(
          x = if (x < leader.x) x + 1 else if (x > leader.x) x - 1 else x,
          y = if (y < leader.y) y + 1 else if (y > leader.y) y - 1 else y
        )
    }

  }
  object Pos {

    implicit val posToPosMonoid: Monoid[Pos => Pos] = new Monoid[Pos => Pos] {
      override def empty: Pos => Pos = identity
      override def combine(x: Pos => Pos, y: Pos => Pos): Pos => Pos = p => x(y(p))
    }

    val start: Pos = Pos(0, 0)

  }

  case class Motion(direction: Direction, steps: Int)
  object Motion {
    def from(s: String): Option[Motion] = s.split(" ") match {
      case Array(motion, steps) => (Direction.from(motion), steps.toIntOption).mapN(Motion.apply)
    }
  }

  def getMotions(input: List[String]): Option[List[Motion]] = input.traverse(Motion.from)

  def getSingleSteps(motions: List[Motion]): List[Direction] =
    motions.flatMap { case Motion(direction, steps) => List.fill[Direction](steps)(direction) }

  def getRopeAfterHeadMoves(direction: Direction, rope: Rope[Pos]): Rope[Pos] = {
    val newHead = rope.getHead.move(direction)
    rope match {
      case Knot(_) => Knot(newHead)
      case Segment(_, tail) =>
        Segment(
          newHead,
          tail.scanLeft(newHead) { case (leaderPos, p) => p.getNewFollowerPos(leaderPos) }
        )
    }
  }

  def getAllRopes(initialRope: Rope[Pos], singleStepMotions: List[Direction]): List[Rope[Pos]] =
    singleStepMotions.scanLeft(initialRope) { case (prevRope, d) => getRopeAfterHeadMoves(d, prevRope) }

  def getDistinctRopeTailPositionCount(initialRope: Rope[Pos], motions: List[Motion]): Int =
    getAllRopes(initialRope, singleStepMotions = getSingleSteps(motions)).map(_.getLast).toSet.size

  def getDistinctRopeTailPositionCount(initialRope: Rope[Pos], input: List[String]): Option[Int] =
    getMotions(input).map(motions => getDistinctRopeTailPositionCount(initialRope, motions))

}
