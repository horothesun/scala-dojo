import ClassicSnake.Direction._
import ClassicSnake.Snake._
import cats._
import cats.syntax.all._
import scala.annotation.tailrec

object ClassicSnake {

  sealed trait Snake[A] {
    def head: A

    def grownForward(n: Int, a: A): Snake[A] = grownForward(as = List.fill(n)(a))
    def grownForward(as: Seq[A]): Snake[A] = {
      def s(a: A): Endo[Snake[A]] = GrownForward[A](head = a, _: Snake[A])
      val f: Endo[Snake[A]] = as.map(s).foldK
      f(this)
    }
    def grownLeft(head: A): Snake[A] = GrownLeft(head, tail = this)
    def grownRight(head: A): Snake[A] = GrownRight(head, tail = this)

    def map[B](f: A => B): Snake[B] = Functor[Snake].map(this)(f)

    def movedForward = ???
  }
  object Snake {
    case class Dot[A](head: A) extends Snake[A]
    case class GrownForward[A](head: A, tail: Snake[A]) extends Snake[A]
    case class GrownLeft[A](head: A, tail: Snake[A]) extends Snake[A]
    case class GrownRight[A](head: A, tail: Snake[A]) extends Snake[A]

    implicit def eq[A: Eq]: Eq[Snake[A]] = Eq.fromUniversalEquals

    implicit val functor: Functor[Snake] = new Functor[Snake] {
      override def map[A, B](fa: Snake[A])(f: A => B): Snake[B] =
        fa match {
          case Dot(head)                => Dot(f(head))
          case GrownForward(head, tail) => GrownForward(f(head), map(tail)(f))
          case GrownLeft(head, tail)    => GrownLeft(f(head), map(tail)(f))
          case GrownRight(head, tail)   => GrownRight(f(head), map(tail)(f))
        }
    }
  }

  sealed trait Direction {
    def leftTurned: Direction =
      this match {
        case North => West
        case South => East
        case West  => South
        case East  => North
      }
    def rightTurned: Direction =
      this match {
        case North => East
        case South => West
        case West  => North
        case East  => South
      }
  }
  object Direction {
    case object North extends Direction
    case object South extends Direction
    case object West extends Direction
    case object East extends Direction
  }

  def getHeadDirection[A](start: Direction): Snake[A] => Direction = getSnakeWithDirections(start)(_).head._2

  def getSnakeWithDirections[A](start: Direction): Snake[A] => Snake[(A, Direction)] = enrich(start, getNextDirection)

  def getNextDirection[A]: Snake[A] => Direction => Direction = {
    case Dot(_) | GrownForward(_, _) => identity
    case GrownLeft(_, _)             => _.leftTurned
    case GrownRight(_, _)            => _.rightTurned
  }

  def enrich[A, B](start: B, next: Snake[A] => B => B): Snake[A] => Snake[(A, B)] = {
    case s @ Dot(head)                => Dot((head, next(s)(start)))
    case s @ GrownForward(head, tail) =>
      val tb = enrich(start, next)(tail)
      val b = tb.head._2
      GrownForward((head, next(s)(b)), tb)
    case s @ GrownLeft(head, tail) =>
      val tb = enrich(start, next)(tail)
      val b = tb.head._2
      GrownLeft((head, next(s)(b)), tb)
    case s @ GrownRight(head, tail) =>
      val tb = enrich(start, next)(tail)
      val b = tb.head._2
      GrownRight((head, next(s)(b)), tb)
  }

  def main(args: Array[String]): Unit = {
    val snake1: Snake[String] = Dot("a").grownForward(2, "b").grownLeft("c")
//    val snake1FoldLeft = snake1.foldLeft(List.empty[String])((acc, s) => acc :+ s).mkString(",")
//    val snake1FoldRight =
//      snake1.foldRight(Eval.now(List.empty[String]))((s, acc) => acc.map(s :: _)).value.mkString(",")
    println(s"snake1: $snake1")
//    println(s"snake1FoldLeft: $snake1FoldLeft")
//    println(s"snake1FoldRight: $snake1FoldRight")
  }

}
