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

    def getHead: Knot[A] = this match {
      case k @ Knot(_)      => k
      case Segment(head, _) => head
    }

    def getLast: Knot[A] = Knot(this.foldLeft(getHead.a) { case (_, a) => a })

  }
  object Rope {

    case class Knot[A](a: A) extends Rope[A]
    case class Segment[A](head: Knot[A], tail: Rope[A]) extends Rope[A]

    implicit val functor: Functor[Rope] = derived.semiauto.functor
    implicit val foldable: Foldable[Rope] = derived.semiauto.foldable

    def make2[A](a: A): Rope[A] = fromNel(NonEmptyList.of(a, a))

    def make10[A](a: A): Rope[A] = fromNel(NonEmptyList(a, List.fill[A](9)(a)))

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

  sealed trait Direction
  object Direction {
    case object Left extends Direction
    case object Right extends Direction
    case object Up extends Direction
    case object Down extends Direction

    def values: Array[Direction] = Array(Left, Right, Up, Down)

    def from(s: String): Option[Direction] =
      s match {
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

    def getNewFollowerPos(leader: Pos): Pos =
      getProximity(leader) match {
        case Adjacent => this
        case Distant =>
          if (x == leader.x) {
            Pos(
              x,
              leader.y + (if (y < leader.y) -1 else 1)
            )
          } else if (y == leader.y) {
            Pos(
              leader.x + (if (x < leader.x) -1 else 1),
              y
            )
          } else {
            if (Math.abs(x - leader.x) > Math.abs(y - leader.y)) {
              Pos(
                leader.x + (if (x < leader.x) -1 else 1),
                leader.y
              )
            } else {
              Pos(
                leader.x,
                leader.y + (if (y < leader.y) -1 else 1)
              )
            }
          }
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
    val newHeadPos = rope.getHead.a.move(direction)
    rope match {
      case Knot(_) => Knot(newHeadPos)
      case Segment(_, tail) =>
        Rope.fromNel(
          tail
            .foldLeft((newHeadPos, NonEmptyList.one(newHeadPos))) { case ((newLeaderPos, acc), p) =>
              val newFollowerPos = p.getNewFollowerPos(newLeaderPos)
              (newFollowerPos, newFollowerPos :: acc)
            }
            ._2
            .reverse
        )
    }
  }

  def getAllRopes(singleStepMotions: List[Direction], initialRope: Rope[Pos]): List[Rope[Pos]] =
    singleStepMotions
      .foldLeft[(Rope[Pos], List[Rope[Pos]])]((initialRope, List(initialRope))) { case ((prevRope, acc), d) =>
        val newRope = getRopeAfterHeadMoves(d, prevRope)
        (newRope, newRope :: acc)
      }
      ._2

  def getDistinctRopeTailPositionCount(initialRope: Rope[Pos], motions: List[Motion]): Int =
    getAllRopes(singleStepMotions = getSingleSteps(motions), initialRope)
      .map(_.getLast.a)
      .toSet
      .size

  def getDistinctRopeTailPositionCount(initialRope: Rope[Pos], input: List[String]): Option[Int] =
    getMotions(input).map(motions => getDistinctRopeTailPositionCount(initialRope, motions))

}
