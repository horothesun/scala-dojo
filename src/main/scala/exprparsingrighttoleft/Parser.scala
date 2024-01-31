package exprparsingrighttoleft

import cats._
import scala.annotation.tailrec

case class Parser[A](run: String => Option[(A, String)]) {

  def map[B](f: A => B): Parser[B] = Functor[Parser].map(this)(f)

  def flatMap[B](f: A => Parser[B]): Parser[B] = Monad[Parser].flatMap(this)(f)
  def pure(a: A): Parser[A] = Monad[Parser].pure(a)

}
object Parser {

  implicit def eq[A]: Eq[Parser[A]] = Eq.fromUniversalEquals

  implicit val functor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser[B] { s =>
      fa.run(s).map { case (a, rest) => (f(a), rest) }
    }
  }

  implicit val monad: Monad[Parser] = new Monad[Parser] {

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Parser[B] { s =>
      fa.run(s) match {
        case None            => None
        case Some((a, rest)) => f(a).run(rest)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = Parser[B] { s =>
      @tailrec
      def step(thisA: A): Parser[B] =
        f(thisA).run(s) match {
          case None                   => Parser[B](_ => None)
          case Some((Left(nextA), _)) => step(nextA)
          case Some((Right(b), rest)) => Parser[B](_ => Some((b, rest)))
        }
      step(a).run(s)
    }

    override def pure[A](x: A): Parser[A] = Parser[A](_ => Some((x, "")))

  }

//  object ParserOps {
//
//    def natural: Parser[Int] = some(digitChar).map(_.toList.mkString.toInt)
//
//    def digitChar: Parser[Char] = Parser[Char] {
//      _.toList match {
//        case Nil          => None
//        case head :: tail => s"$head".toIntOption.map(_ => (head, tail.mkString))
//      }
//    }
//
//    def some[A](p: Parser[A]): Parser[NonEmptyList[A]] = ???
//
//  }

}
