import cats._
import cats.implicits._
import ClassicSnake.Direction._
import ClassicSnake.Snake._

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

    def foldLeft[B](b: B)(f: (B, A) => B): B = Foldable[Snake].foldLeft(this, b)(f)
    def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = Foldable[Snake].foldRight(this, lb)(f)

    def movedForward = ???
  }
  object Snake {
    case class Dot[A](head: A) extends Snake[A]
    case class GrownForward[A](head: A, tail: Snake[A]) extends Snake[A]
    case class GrownLeft[A](head: A, tail: Snake[A]) extends Snake[A]
    case class GrownRight[A](head: A, tail: Snake[A]) extends Snake[A]

    implicit val foldable: Foldable[Snake] = new Foldable[Snake] {
      override def foldLeft[A, B](fa: Snake[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Dot(head)                => f(b, head)
          case GrownForward(head, tail) => f(foldLeft(tail, b)(f), head)
          case GrownLeft(head, tail)    => f(foldLeft(tail, b)(f), head)
          case GrownRight(head, tail)   => f(foldLeft(tail, b)(f), head)
        }
      @tailrec
      override def foldRight[A, B](fa: Snake[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case Dot(head)                => f(head, lb)
          case GrownForward(head, tail) => foldRight(tail, f(head, lb))(f)
          case GrownLeft(head, tail)    => foldRight(tail, f(head, lb))(f)
          case GrownRight(head, tail)   => foldRight(tail, f(head, lb))(f)
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
    case s @ Dot(head) => Dot((head, next(s)(start)))
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

  def enrich_2[A, B](start: B, next: Snake[A] => B => B): Snake[A] => Snake[(A, B)] = ???
//    _.foldRight[Snake[(A, B)]]()

  def main(args: Array[String]): Unit = {
    val snake1: Snake[String] = Dot("a").grownForward(2, "b").grownLeft("c")
    val snake1FoldLeft = snake1.foldLeft(List.empty[String]) { case (acc, s) => s +: acc }.mkString(",")
    val snake1FoldRight =
      snake1.foldRight(Eval.now(List.empty[String])) { case (s, acc) => acc.map(_ :+ s) }.value.mkString(",")
    println(s"snake1: $snake1")
    println(s"snake1FoldLeft: $snake1FoldLeft")
    println(s"snake1FoldRight: $snake1FoldRight")
  }

}
