package adventofcode22

import cats._
import Stack._

sealed trait Stack[A] {

  def push(a: A): Stack[A] = Cons(a, this)

  def pop: Option[(A, Stack[A])] = this match {
    case Empty()         => None
    case Cons(top, tail) => Some((top, tail))
  }

  def peek: Option[A] = this match {
    case Empty()      => None
    case Cons(top, _) => Some(top)
  }

  def map[B](f: A => B): Stack[B] = Functor[Stack].map(this)(f)

}
object Stack {

  case class Empty[A]() extends Stack[A]
  case class Cons[A](top: A, tail: Stack[A]) extends Stack[A]

  def empty[A]: Stack[A] = Empty()
  def of[A](a: A): Stack[A] = Stack.empty[A].push(a)

  implicit val foldable: Foldable[Stack] = derived.semiauto.foldable
  implicit val functor: Functor[Stack] = derived.semiauto.functor

}
